#################################################################################################################
#*** Mackerel MSE
#*** Plot objectives
#################################################################################################################

#******************************************************************************
#************* Load all predictions *******************************************
#******************************************************************************

load(file='Rdata/runlist,2019.04.09.Rdata')

#******************************************************************************
#************* plots **********************************************************
#******************************************************************************

sub <- subMSE(runlist,MP=c(2:11))

OMnam <- c('OMbase',paste0('OMcore',1:4),paste0('OMstress',1:3))
OMshape <- c(17,rep(16,7))
OMcol <- c('black',brewer.pal(5,'Blues')[5:2],brewer.pal(4,'Reds')[4:2])
threshold <- 0.75
basey <- 2019

#--------------------------------------------------------------------------------
#### Objective 1               	Probability that SSB > LRP ###################
#--------------------------------------------------------------------------------
ggadd <- function(p){p+
    scale_color_manual(values=OMcol)+
    scale_shape_manual(values=OMshape)+
    scale_y_continuous(limits=c(0,1),expand=c(0,0))+
    geom_vline(xintercept=2.5,linetype='dotted')+
    theme(strip.background = element_blank())
    }
p3 <- diamondplot(runlist,what='probCZ',ylab='Probability out of the CZ',year=basey+3,xlab='HCR',hline=0.65,OMtype = FALSE)
saveplot(ggadd(p3),name="Obj1_3y",dim=c(10,7),wd='img/mse')

p5 <- diamondplot(runlist,what='probCZ',ylab='Probability out of the CZ',year=basey+5,xlab='HCR',hline=0.75,OMtype = FALSE)
saveplot(ggadd(p5),name="Obj1_5y",dim=c(10,7),wd='img/mse')

p10 <- diamondplot(runlist,what='probCZ',ylab='Probability out of the CZ',xlab='HCR',year=basey+10,hline=0.75,OMtype=FALSE)
saveplot(ggadd(p10),name="Obj1_10y",dim=c(10,7),wd='img/mse')


### combine ---------------------------------------------------------------------
mylegend<-extractLegend(ggadd(p5)+theme(legend.position="bottom"))
lheight <- sum(mylegend$heights)
gA <- ggplotGrob(ggadd(p3) + theme(legend.position="none")+labs(x=''))
gB <- ggplotGrob(ggadd(p5)+ theme(legend.position="none")+labs(y='',x='HCR'))
gC <- ggplotGrob(ggadd(p10)+ theme(legend.position="none")+labs(y='',x=''))

saveplot(grid.arrange(gtable_cbind(gA, gB, gC),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight)),
         name="Obj1_3y_5y_10y",
         dim=c(20,7),
         wd='img/mse')


### text files ---------------------------------------------------------------------
obj1 <- diamondplot(runlist,what='probCZ',year=basey+3,data=TRUE)
names(obj1)[6] <- 'y3'
obj1 <- obj1[,-c(3:5)]
obj1$y5 <- diamondplot(runlist,what='probCZ',year=basey+5,data=TRUE)$y
obj1$y10 <- diamondplot(runlist,what='probCZ',year=basey+10,data=TRUE)$y
obj1$n <- diamondplot(runlist,what='probCZ',year='threshold',threshold=0.75,data=TRUE)$y

write.csv(obj1, file = "csv/obj1.csv")

### difference between OMs -----------------------------------------------------------
df1 <- foreplot(sub,what.y='probCZ',ylab='Probability out of the CZ',by='MP',data=T)
df1$MP <- as.numeric(gsub('MP','',df1$MP))
df1$uncertainty <- 'base'
df1[df1$OM %in% c('OMcore1','OMstress1'),'uncertainty'] <- 'rec'
df1[df1$OM %in% c('OMcore2','OMcore3'),'uncertainty'] <- 'M'
df1[df1$OM %in% c('OMcore4','OMstress2','OMstress3'),'uncertainty'] <- 'US Catch'
df1$MPfac <- factor(paste('HCR',df1$MP),levels = paste('HCR',1:11))
df1base <- df1[df1$MP %in% c(4,11) & df1$OM == 'OMbase',-7]
df1rest <- df1[df1$MP %in% c(4,11) & df1$OM != 'OMbase',]

pobj1OM <- ggplot(df1rest,aes(x=x,y=y,col=OM))+
    geom_hline(yintercept=0.75,linetype='dashed',col='darkgreen')+
    geom_line(size=1)+
    geom_line(data=df1base,aes(x=x,y=y),size=1.5)+
    facet_grid(uncertainty~MPfac)+
    scale_color_manual(values=OMcol)+
    scale_x_continuous(breaks = c(2019,2024,2029))+
    theme(strip.background = element_blank())+
    labs(y='Probability SSB > LRP',x='Year')

#--------------------------------------------------------------------------------
#### Objective 2	avoid decline ###############################################
#--------------------------------------------------------------------------------

# function -----------------------------------------------------------------------
probdecliney <- function(x,year,start=1,vector=FALSE){
    y <- x[[start]]
    ssb <- y$ssb
    ntot <- NULL
    ndecline <- NULL
    for(i in (start+1):(start-1+year+1)){
        y <- x[[i]]
        nssb <- y$ssb
        decline <- nssb<ssb
        ntot <- c(ntot,length(decline))
        ndecline <- c(ndecline,length(decline[decline]))
        ssb <- nssb
    }
    if(!vector){
        ntot <- sum(ntot)
        ndecline <- sum(ndecline)
    }
    round(ndecline/ntot,2) 
}

# dataframe -------------------------------------------------------------------------
dfdecliney <- ldply(runlist,probdecliney,3)
names(dfdecliney) <- c('id','y3')
dfdecliney$y5 <- ldply(runlist,probdecliney,5)$V1
dfdecliney$y10 <- ldply(runlist,probdecliney,10)$V1
dfdecliney$OM <- ldply(runlist,function(x) attr(x,'OMlabel'))$V1
dfdecliney$MP <- as.numeric(gsub("MP","",ldply(runlist,function(x) attr(x,'MPlabel'))$V1))

ggadd2 <- function(p){
    p+
        geom_rect(aes(ymin=0.5,ymax=1,xmin=-Inf,xmax=Inf),fill='lightgrey',col='lightgrey',alpha=0.5)+
        geom_point(size=1.5)+
        labs(x='HCR',col='OM',shape='OM')+
        scale_x_continuous(labels = as.character(unique(dfdecliney$MP)), breaks = unique(dfdecliney$MP))+
        scale_color_manual(values=OMcol)+
        scale_shape_manual(values=OMshape)+
        scale_y_continuous(limits=c(0,1),expand=c(0,0))+
        geom_vline(xintercept=2.5,linetype='dotted')
}

p3 <- ggadd2(ggplot(dfdecliney,aes(x=MP,y=y3,col=OM,shape=OM))+ggtitle('2019-2022'))
p5 <- ggadd2(ggplot(dfdecliney,aes(x=MP,y=y5,col=OM,shape=OM))+ggtitle('2019-2024'))
p10 <- ggadd2(ggplot(dfdecliney,aes(x=MP,y=y10,col=OM,shape=OM))+ggtitle('2019-2029'))

mylegend<-extractLegend(p3+ theme(legend.position="bottom"))
lheight <- sum(mylegend$height)
gA <- ggplotGrob(p3 + theme(legend.position="none")+ylab('Probability of decline')+xlab(''))
gB <- ggplotGrob(p5+ theme(legend.position="none")+ylab('')+xlab('HCR'))
gC <- ggplotGrob(p10+ theme(legend.position="none")+ylab(''))

saveplot(grid.arrange(gtable_cbind(gA, gB, gC),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight)),
         name="Obj2_3y_5y_10y",
         dim=c(20,7),
         wd='img/mse')

### text files ---------------------------------------------------------------------
obj2 <- dfdecliney[,-1]
write.csv(obj2, file = "csv/obj2.csv")

### difference between OMs -----------------------------------------------------------
df2 <- ldply(sub,probdecliney,10,vector=TRUE)
df2 <- cbind(df2,dfdecliney[dfdecliney$MP!=1,c('OM','MP')])
df2 <- melt(df2,id=c('OM','MP','.id'),variable.name = 'x',value.name='y')
df2$x <- as.numeric(df2$x)+basey-1
df2$uncertainty <- 'base'
df2[df2$OM %in% c('OMcore1','OMstress1'),'uncertainty'] <- 'rec'
df2[df2$OM %in% c('OMcore2','OMcore3'),'uncertainty'] <- 'M'
df2[df2$OM %in% c('OMcore4','OMstress2','OMstress3'),'uncertainty'] <- 'US Catch'
df2$MPfac <- factor(paste('HCR',df2$MP),levels = paste('HCR',1:11))
df2base <- df2[df2$MP %in% c(4,11) & df2$OM == 'OMbase',-6]
df2rest <- df2[df2$MP %in% c(4,11) & df2$OM != 'OMbase',]

pobj2OM <- ggplot(df2rest,aes(x=x,y=y,col=OM))+
    geom_rect(aes(ymin=0.5,ymax=1,xmin=-Inf,xmax=Inf),fill='lightgrey',col='lightgrey',alpha=0.5)+
    geom_line(size=1)+
    geom_line(data=df2base,aes(x=x,y=y),size=1.5)+
    facet_grid(uncertainty~MPfac)+
    scale_color_manual(values=OMcol)+
    scale_x_continuous(breaks = c(2019,2024,2029))+
    theme(strip.background = element_blank())+
    labs(y='Probability of decline',x='Year')

#--------------------------------------------------------------------------------
#### Objective 3	maximmise catch ###############################################
#--------------------------------------------------------------------------------

## see plot_obj3.R
load(file='Rdata/df_obj3.Rdata')
df3 <- ddply(df,c('OM','MP','year'),summarise,y=median(can3),q1=quantile(can3,0.025),q3=quantile(can3,0.975))
df$year <- as.numeric(as.character(df$year))+2018
obj3 <- ddply(df[df$year %in% 2019:2022,],c('OM','MP'),summarise,y3=median(can3))
obj3$y5 <- ddply(df[df$year %in% 2019:2024,],c('OM','MP'),summarise,y=median(can3))$y
obj3$y10 <- ddply(df[df$year %in% 2019:2029,],c('OM','MP'),summarise,y=median(can3))$y

ggadd3 <- function(p){
    p+geom_point(size=1.5)+labs(col='OM',shape='OM')+
        scale_x_continuous(labels = as.character(unique(df$MP)), breaks = unique(df$MP))+
        scale_color_manual(values=OMcol)+
        scale_shape_manual(values=OMshape)+
        scale_y_continuous(expand=c(0,0),limits=c(0,14000))+
        geom_vline(xintercept=2.5,linetype='dotted')+
        theme(strip.background = element_blank())
}

### Plot showing median catch after 3,5 and 10 years
p1 <- ggadd3(ggplot(obj3,aes(x=MP,y=y3,col=OM,shape=OM)))
p2 <-ggadd3(ggplot(obj3,aes(x=MP,y=y5,col=OM,shape=OM)))
p3 <-ggadd3(ggplot(obj3,aes(x=MP,y=y10,col=OM,shape=OM)))

mylegend<-extractLegend(p1+theme(legend.position="bottom"))
lheight <- sum(mylegend$heights)
gA <- ggplotGrob(p1 + theme(legend.position="none")+labs(x='',y='Catch (t)')+ggtitle('2019-2022'))
gB <- ggplotGrob(p2 + theme(legend.position="none")+labs(x='HCR',y='')+ggtitle('2019-2024'))
gC <- ggplotGrob(p3 + theme(legend.position="none")+labs(x='',y='')+ggtitle('2019-2029'))

saveplot(grid.arrange(gtable_cbind(gA, gB, gC),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight)),
         name="Obj3_3y_5y_10y",
         dim=c(20,7),
         wd='img/mse')

### text files ---------------------------------------------------------------------
write.csv(obj3, file = "csv/obj3.csv")

### difference between OMs -----------------------------------------------------------
df3$uncertainty <- 'base'
df3[df3$OM %in% c('OMcore1','OMstress1'),'uncertainty'] <- 'rec'
df3[df3$OM %in% c('OMcore2','OMcore3'),'uncertainty'] <- 'M'
df3[df3$OM %in% c('OMcore4','OMstress2','OMstress3'),'uncertainty'] <- 'US Catch'
df3$MPfac <- factor(paste('HCR',df3$MP),levels = paste('HCR',1:11))
df3$year <- as.numeric(as.character(df3$year))
df3base <- df3[df3$MP %in% c(4,11) & df3$OM == 'OMbase',-(ncol(df3)-1)]
df3rest <- df3[df3$MP %in% c(4,11) & df3$OM != 'OMbase',]

pobj3OM <- ggplot(df3rest,aes(x=year,y=y))+
    geom_ribbon(aes(x=year,ymin=q1,ymax=q3,fill=OM),alpha=0.2)+
    geom_ribbon(data=df3base,aes(x=year,ymin=q1,ymax=q3,fill=OM),alpha=0.2)+
    geom_line(size=1,aes(col=OM))+
    geom_line(data=df3base,aes(x=year,y=y,col=OM),size=1.5)+
    facet_grid(uncertainty~MPfac)+
    scale_color_manual(values=OMcol)+
    scale_fill_manual(values=OMcol)+
    scale_x_continuous(breaks =c(2019,2024,2029))+
    theme(strip.background = element_blank())+
    labs(y='Catch',x='Year')

#--------------------------------------------------------------------------------
#### Differences between OMs ###############################################
#--------------------------------------------------------------------------------

mylegend<-extractLegend(pobj1OM+theme(legend.position="bottom"))
lheight <- sum(mylegend$heights)
gA <- ggplotGrob(pobj1OM + theme(legend.position="none"))
gB <- ggplotGrob(pobj2OM + theme(legend.position="none"))
gC <- ggplotGrob(pobj3OM + theme(legend.position="none"))

saveplot(grid.arrange(gtable_cbind(gA, gB, gC),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight)),
         name="Obj_all_OMtypes",
         dim=c(28,13),
         wd='img/mse')

#--------------------------------------------------------------------------------
#### Trade-offs ###############################################
#--------------------------------------------------------------------------------
obj1 <- read.csv(file = "csv/obj1.csv",row.names = 1)
obj2 <- read.csv(file = "csv/obj2.csv",row.names = 1)
obj3 <- read.csv(file = "csv/obj3.csv",row.names = 1)

obj1$n <- NULL

### normal plot
na <- paste0(c(3,5,10),'y')
names(obj1)[which(names(obj1) %in% na)] <- paste('obj1')
tradewide <- Reduce(function(x, y) merge(x, y, all=FALSE), list(melt(obj1,id=c('OM','MP'),value.name = 'obj1'), 
                                                                melt(obj2,id=c('OM','MP'),value.name = 'obj2'), 
                                                                melt(obj3,id=c('OM','MP'),value.name = 'obj3')))

tradewide <- tradewide[order(tradewide$MP),]
tradewide$variable <- revalue(tradewide$variable, replace = c("y3" = "3 years",
                                                "y5" = "5 years",
                                                "y10" = "10 years"))


ptrade1 <- ggplot(tradewide,aes(x=obj3,y=obj1,col=factor(MP),shape=variable))+
    geom_hline(yintercept=0.75,linetype='dashed',col='darkgreen')+
    geom_point()+
    facet_wrap(~OM,ncol=4)+
    scale_color_viridis_d()+
    labs(x='Catch (t)',y='Probability SSB>LRP',col='HCR',shape='Time')+
    theme(strip.background = element_blank())
saveplot(ptrade1,name="Trade_obj1",dim=c(15,11),wd='img/mse')

ptrade2 <- ggplot(tradewide,aes(x=obj3,y=obj2,col=factor(MP),shape=variable))+
    geom_rect(aes(ymin=0.5,ymax=1,xmin=-Inf,xmax=Inf),fill='lightgrey',col='lightgrey',alpha=0.5)+
    geom_point()+
    facet_wrap(~OM,ncol=4)+
    scale_color_viridis_d()+
    labs(x='Catch',y='Probability of decline',col='HCR',shape='Time')+
    theme(strip.background = element_blank())+
    scale_y_continuous(expand = c(0,0),limits = c(0,1))
saveplot(ptrade2,name="Trade_obj2",dim=c(15,11),wd='img/mse')

t <- as.character(unique(tradewide$variable))
tshort <- unlist(lapply(strsplit(t,' '),'[[',1))
for(i in t){
    ptrade1sub <- ggplot(tradewide[tradewide$variable==i,],aes(x=obj3,y=obj1,col=factor(MP)))+
        geom_hline(yintercept=0.75,linetype='dashed',col='darkgreen')+
        geom_point()+
        facet_wrap(~OM,ncol=4)+
        scale_color_viridis_d()+
        labs(x='Catch (t)',y='Probability SSB>LRP',col='HCR',title = i)+
        theme(strip.background = element_blank())
    saveplot(ptrade1sub,name=paste0("Trade_obj1_",tshort[which(i==t)]),dim=c(18,10),wd='img/mse')
    ptrade2sub <- ggplot(tradewide[tradewide$variable==i,],aes(x=obj3,y=obj2,col=factor(MP)))+
        geom_rect(aes(ymin=0.5,ymax=1,xmin=-Inf,xmax=Inf),fill='lightgrey',col='lightgrey',alpha=0.5)+
        geom_point()+
        facet_wrap(~OM,ncol=4)+
        scale_color_viridis_d()+
        labs(x='Catch',y='Probability of decline',col='HCR',title = i)+
        theme(strip.background = element_blank())+
        scale_y_continuous(expand = c(0,0),limits = c(0,1))
    saveplot(ptrade2sub,name=paste0("Trade_obj2_",tshort[which(i==t)]),dim=c(18,10),wd='img/mse')
}


### Spiderplot
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
obj301 <- obj3
obj301$y3 <- range01(obj301$y3)
obj301$y5 <- range01(obj301$y5)
obj301$y10 <- range01(obj301$y10)
trade <- rbind(cbind(melt(obj1,id=c('OM','MP')),obj=1),
               cbind(melt(obj2,id=c('OM','MP')),obj=2),
               cbind(melt(obj301,id=c('OM','MP')),obj=3))
trade <- trade[order(trade$MP),]

spider <- ggplot(trade,  aes(x=factor(obj), y=value, group= MP, colour=MP, fill=MP)) + 
    geom_point(size=2) + 
    geom_polygon(size = 1, alpha= 0.2) + 
    theme_light()+
    coord_polar()+
    facet_wrap(~OM)

#--------------------------------------------------------------------------------
#### Table ######################################################################
#--------------------------------------------------------------------------------
trade <- rbind(cbind(melt(obj1,id=c('OM','MP')),obj=1),
               cbind(melt(obj2,id=c('OM','MP')),obj=2),
               cbind(melt(obj3,id=c('OM','MP')),obj=3))
trade <- trade[order(trade$MP),]
trade$type <- gsub("OM","",gsub('[0-9]+', '', trade$OM))
out <- ddply(trade[trade$type %in%c('base','core'),],c('MP','variable','obj'),summarise,value=min(value))
out <- dcast(out,MP~obj+variable,value.var = 'value')
out

trade[trade$obj %in% 1:2,'value'] <- round(trade[trade$obj %in% 1:2,'value']*100,0)
trade[trade$obj %in% 3,'value'] <- round(trade[trade$obj %in% 3,'value']/1000,1)
fn <- function(x,y){ifelse(y[1]==2,max(x),min(x))}
out <- ddply(trade[trade$type %in%c('base','core'),],c('MP','variable','obj'),summarise,value=paste0(fn(value,obj),'% (',value[1],'%)'))
out <- dcast(out,MP~obj+variable,value.var = 'value')
out[,8:10] <- apply(out[,8:10],2,function(x){gsub('%',' kt',x)})
out

write.csv(out, file = "csv/msetable.csv")

outfull <- dcast(trade[,-ncol(trade)],OM+MP~obj+variable,value.var = 'value')
outfull [,3:8] <- apply(outfull [,3:8],2,function(x){paste0(x,'%')})
outfull [,9:11] <- apply(outfull [,9:11],2,function(x){paste0(x,' kt')})
outfull 
write.csv(outfull, file = "csv/msetable_full.csv")

