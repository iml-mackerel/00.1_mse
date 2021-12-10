#################################################################################################################
#*** Mackerel MSE
#*** Plot Egg index based Harvest Control Rules
#################################################################################################################

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

p1 <- ggplot(Ihist,aes(x=year,y=I))+geom_line(size=1)+
    geom_point(aes(col=class),size=1.1)+
    labs(col='',y='Total Egg Production',x='Year')+
    theme(legend.position = c(0.8,0.8),
          legend.background = element_rect(fill=alpha('white', 0)),
          legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
    geom_hline(yintercept = I0,col='darkred',linetype='dashed')+
    geom_hline(yintercept = Itarget,col='darkgreen',linetype='dashed')+
    geom_hline(yintercept = Irecent,col='darkblue',linetype='dotted')+
    scale_color_manual(values=c('darkgreen','darkred','darkblue'),breaks=c("Ilow", "Ihigh",'Iy'),labels=c(expression(I["low"]),expression(I["high"]),expression(I["y"])))+
    scale_y_continuous(limits=c(0,max(Ihist$I)*1.05),expand=c(0,0))+
    geom_text(label='A)',y=Inf,x=-Inf,hjust=-0.3,vjust=1.3)

BtoC <- data.frame(x=seq(0,Itarget*1.2,length.out = 100))
BtoC$HCR4 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,0,0,25000)
BtoC$HCR5 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,8000,0,25000) #abrupt
BtoC$HCR6 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,8000,0,25000,TRUE) #ramp
BtoC$HCR7 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,2000,2000,25000)
BtoC$HCR8 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,4000,4000,25000)
BtoC$HCR9 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,6000,6000,25000)
BtoC$HCR10 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,8000,8000,25000)
BtoC$HCR11 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,10000,10000,25000)
#BtoC$HCR11 <- sapply(BtoC$x, ItoTac, Itarget,I0,25000,15000,15000,25000)

BtoC <- reshape2::melt(BtoC,id='x',value.name='TAC',variable.name='HCR')
BtoC$HCR <- as.numeric(gsub('HCR','',BtoC$HCR))

TACtarget=25000

p2 <- ggplot(BtoC,aes(x=x,y=TAC))+
    labs(x='Total Egg Production',col='')+
    geom_vline(xintercept=Itarget,col='darkgreen',linetype='dashed',size=0.8)+geom_text(x=Itarget,y=TACtarget,label=expression(I["high"]),vjust=-0.1,hjust=-0.2,col='darkgreen',size=3)+
    geom_vline(xintercept=I0,col='darkred',linetype='dashed',size=0.8)+  geom_text(x=I0,y=TACtarget,label=expression(I["low"]),vjust=-0.1,hjust=-0.4,col='darkred',size=3)+
    geom_vline(xintercept=Irecent,col='darkblue',linetype='dotted',size=0.8)+geom_text(x=Irecent,y=TACtarget,label=expression(I["y"]),vjust=-0.1,hjust=-0.2,col='darkblue',size=3)+
    scale_y_continuous(expand=c(0,0),limits=c(0,30000))+scale_x_continuous(expand=c(0,0))+
    geom_text(label='B)',y=Inf,x=-Inf,hjust=-0.3,vjust=1.3)+
    geom_line(size=1,aes(col=as.factor(HCR)))+
    scale_color_viridis(discrete = TRUE)

p <- grid.arrange(p1,p2,ncol=2,widths=c(0.5,0.6))
saveplot(p,name="HCR",dim=c(18,6.5),wd='img/resdoc')


### French verions
p1 <- ggplot(Ihist,aes(x=year,y=I))+geom_line(size=1)+
    geom_point(aes(col=class),size=1.1)+
    labs(col='',y='Production totale d’œufs',x='Année')+
    theme(legend.position = c(0.8,0.8),
          legend.background = element_rect(fill=alpha('white', 0)),
          legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
    geom_hline(yintercept = I0,col='darkred',linetype='dashed')+
    geom_hline(yintercept = Itarget,col='darkgreen',linetype='dashed')+
    geom_hline(yintercept = Irecent,col='darkblue',linetype='dotted')+
    scale_color_manual(values=c('darkgreen','darkred','darkblue'),breaks=c("Ilow", "Ihigh",'Iy'),labels=c(expression(I["low"]),expression(I["high"]),expression(I["y"])))+
    scale_y_continuous(limits=c(0,max(Ihist$I)*1.05),expand=c(0,0))+
    geom_text(label='A)',y=Inf,x=-Inf,hjust=-0.3,vjust=1.3)

p2 <- ggplot(BtoC,aes(x=x,y=TAC))+
    labs(x='Production totale d’œufs',col='')+
    geom_vline(xintercept=Itarget,col='darkgreen',linetype='dashed',size=0.8)+geom_text(x=Itarget,y=TACtarget,label=expression(I["high"]),vjust=-0.1,hjust=-0.2,col='darkgreen',size=3)+
    geom_vline(xintercept=I0,col='darkred',linetype='dashed',size=0.8)+  geom_text(x=I0,y=TACtarget,label=expression(I["low"]),vjust=-0.1,hjust=-0.4,col='darkred',size=3)+
    geom_vline(xintercept=Irecent,col='darkblue',linetype='dotted',size=0.8)+geom_text(x=Irecent,y=TACtarget,label=expression(I["y"]),vjust=-0.1,hjust=-0.2,col='darkblue',size=3)+
    scale_y_continuous(expand=c(0,0),limits=c(0,30000))+scale_x_continuous(expand=c(0,0))+
    geom_text(label='B)',y=Inf,x=-Inf,hjust=-0.3,vjust=1.3)+
    geom_line(size=1,aes(col=as.factor(HCR)))+
    scale_color_viridis(discrete = TRUE)


saveplot(grid.arrange(p1,p2,ncol=2,widths=c(0.5,0.6)),name="HCR",dim=c(18,6.5),wd='img/resdoc/fr')


