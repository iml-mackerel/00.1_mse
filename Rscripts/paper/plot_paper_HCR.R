#################################################################################################################
#*** Mackerel MSE
#*** Plot Egg index based Harvest Control Rules
#################################################################################################################

wdimg <- 'img/paper/'
load(file='Rdata/input/dat.Rdata')

Ihist <- cbind(I=exp(dat$logobs[,1]),dat$aux)
Ihist <- data.frame(Ihist[which(Ihist[,3]==3),c(1,2)])

Ytarget <- c(1979,1983,1984,1987,1988,1989,1990,1992,1993,1994,1995)
Y0 <- c(1996:2000,2004:2008)
Ycurrent <- unname(tail(Ihist[,2],3))

Itarget <- gmean(Ihist[which(Ihist[,2] %in% Ytarget),1])
I0 <- gmean(Ihist[which(Ihist[,2] %in% Y0),1])
Irecent <- gmean(Ihist[Ihist$year %in% Ycurrent,1])


ItoTac <- function(x,Itarget,I0,TACtarget,TACmin,TACfloor,TACmax,ramp=FALSE){
    w <- TACmin/TACtarget
    if(x<I0){
        TAC <- w*TACtarget*(x/I0)^3
    }else{
        TAC <- TACtarget*(w+(1-w)*((x-I0)/(Itarget-I0)))
    }
    if(!ramp) TAC <- ifelse(TAC<TACmin,TACfloor,TAC)
    TAC <- ifelse(TAC>TACmax,TACmax,TAC)
    return(TAC)
}

Ihist[Ihist$year %in% Ytarget,'class'] <- 'Ihigh'
Ihist[Ihist$year %in% Y0,'class'] <- 'Ilow'
Ihist[Ihist$year %in% Ycurrent,'class'] <- 'Iy'

treshcol <- c('limegreen','red3','lightseagreen')

p1 <- ggplot(Ihist,aes(x=year,y=I*10^-15))+
    theme(legend.position = c(0.8,0.8),
          legend.background = element_rect(fill=alpha('white', 0)),
          legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
    geom_hline(yintercept = I0*10^-15,col=treshcol[2],linetype='dashed')+
    geom_hline(yintercept = Itarget*10^-15,col=treshcol[1],linetype='dashed')+
    geom_hline(yintercept = Irecent*10^-15,col=treshcol[3],linetype='dotted')+
    geom_line(size=0.6)+
    geom_point(aes(col=class),size=1.1)+
    labs(col='',y=bquote('Total Egg Production'~(10^-15)),x='Year')+
    scale_color_manual(values=treshcol,breaks=c("Ilow", "Ihigh",'Iy'),labels=c(expression(I["low"]),expression(I["high"]),expression(I["y"])))+
    scale_y_continuous(limits=c(0,max(Ihist$I)*1.05*10^-15),expand=c(0,0))+
    geom_text(label='A)',y=Inf,x=-Inf,hjust=-0.3,vjust=1.3)

BtoC <- data.frame(x=seq(0,Itarget*1.2,length.out = 100))
BtoC$HCR2 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,0,0,25000)
BtoC$HCR3 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,2000,2000,25000)
BtoC$HCR4 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,4000,4000,25000)
BtoC$HCR5 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,6000,6000,25000)
BtoC$HCR6 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,8000,8000,25000)
BtoC$HCR7 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,10000,10000,25000)
BtoC$HCR8 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,8000,0,25000) #abrupt
BtoC$HCR9 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,8000,0,25000,TRUE) #ramp

BtoC <- reshape2::melt(BtoC,id='x',value.name='TAC',variable.name='HCR')
BtoC$HCR <- as.numeric(gsub('HCR','',BtoC$HCR))

TACtarget=25000

p2 <- ggplot(BtoC,aes(x=x*10^-15,y=TAC))+
    labs(x=bquote('Total Egg Production'~(10^-15)),col='')+
    geom_vline(xintercept=Itarget*10^-15,col=treshcol[1],linetype='dashed')+geom_text(x=Itarget,y=TACtarget,label=expression(I["high"]),vjust=-0.1,hjust=-0.2,col=treshcol[1],size=3)+
    geom_vline(xintercept=I0*10^-15,col=treshcol[2],linetype='dashed')+  geom_text(x=I0,y=TACtarget,label=expression(I["low"]),vjust=-0.1,hjust=-0.4,col=treshcol[2],size=3)+
    geom_vline(xintercept=Irecent*10^-15,col=treshcol[3],linetype='dotted')+geom_text(x=Irecent,y=TACtarget,label=expression(I["y"]),vjust=-0.1,hjust=-0.2,col=treshcol[3],size=3)+
    scale_y_continuous(expand=c(0,0),limits=c(0,30000))+scale_x_continuous(expand=c(0,0))+
    geom_text(label='B)',y=Inf,x=-Inf,hjust=-0.3,vjust=1.3)+
    geom_line(size=0.6,aes(col=as.factor(HCR)))+
    scale_color_viridis(discrete = TRUE)


#savepng(grid.arrange(p1,p2,ncol=2,widths=c(0.5,0.6)),wdimg,"HCR",c(18,6.5))
pdf("img/paper/pdf/Fig2.pdf", width=17/2.54, height=7/2.54)
grid.arrange(p1,p2,ncol=2,widths=c(0.5,0.6))
dev.off()




