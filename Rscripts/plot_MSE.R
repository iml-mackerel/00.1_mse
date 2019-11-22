#################################################################################################################
#*** Mackerel MSE
#*** Plot random MSE results
#################################################################################################################

#******************************************************************************
#************* Load all predictions *******************************************
#******************************************************************************

load(file='Rdata/runlist.2019-04-09.Rdata')

#******************************************************************************
#************* plots **********************************************************
#******************************************************************************

sub <- subMSE(runlist,MP=c(2:11))
OMnam <- c('OMbase','OMcore1','OMcore2','OMcore3','OMcore4','OMstress1','OMstress2','OMstress3')
OMshape <- c(17,16,16,16,16,16,16,16)
OMcol <- c('black',brewer.pal(5,'Blues')[5:2],brewer.pal(4,'Reds')[4:2])
threshold <- 0.75

#--------------------------------------------------------------------------------
#### Example run ssb (for ppt) ##################################################
#--------------------------------------------------------------------------------
base <- subMSE(runlist,MP=1,OM='base')[[1]]

saveplot(ssb0plot(base,linesize=0.5,years=2005:2030),name="forecast_ssb_MP1_OMbase",dim=c(8,6),wd='img/MSE')

baserec <- subMSE(runlist,MP=c(1,8),OM='base') # effect BH on recruitment

saveplot(recplot(baserec,linesize=1,years=1969:2030,ci=FALSE),name="forecast_ssb_MP1_OMbase",dim=c(8,6),wd='img/MSE')


## different Ms
nmex <- subMSE(runlist,OM=c('base','core3','stress1'),MP=1)
saveplot(ssb0plot(nmex,linesize=0.5,ci=FALSE),name="forecast_ssb_MP1_OMnm",dim=c(12,6),wd='img/MSE')

#--------------------------------------------------------------------------------
#### Example of forecasts ################################################
#--------------------------------------------------------------------------------

## when F0 -----------------------------------------------------------------------

F0 <- subMSE(runlist,MP=1,OM=c('base','core1','core2'))
p1 <- recplot(F0,ci=TRUE,years=2000:2029,linesize=1,legendnames=c(1:3))
p2 <- recplot(F0[[2]],years=2000:2029,linesize=1)+ggtitle('OMcore1')
p3 <- ssb0plot(F0[[1]],years=2000:2029,linesize=1)
p4 <- ssb0plot(F0[[2]],years=2000:2029,linesize=1)

saveplot(grid.arrange(p1,p2,p3,p4,ncol=2),name='forecast_SSBrec',dim=c(16,14),wd='img/resdoc')

#--------------------------------------------------------------------------------
#### Minimum Time for Rebuilding ################################################
#--------------------------------------------------------------------------------

## when F0 -----------------------------------------------------------------------

HCR1 <- subMSE(runlist,MP=1)

pHCR1.ssb <- ssb0plot(HCR1,ci=FALSE,linesize=1,legendnames=OMnam)+geom_vline(xintercept=2018,color='grey',linetype='dashed')+ggtitle('HCR1')
saveplot(pHCR1.ssb,name="Trebuild_HCR1_ssb",dim=c(17,10),wd='img/mse')

pHCR1.cz <- foreplot(HCR1,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = OMnam)+ggtitle('HCR1')
saveplot(pHCR1.cz,name="Trebuild_HCR1_cz",dim=c(17,10),wd='img/mse')

df <- foreplot(HCR1,what.y='probCZ',data=TRUE)
rebuildHCR1 <- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))

## when TAC0 but still missing  ---------------------------------------------------

HCR2 <- subMSE(runlist,MP=2)

pHCR2.ssb <- ssb0plot(HCR2,ci=FALSE,linesize=1,legendnames=OMnam)+
    geom_vline(xintercept=2018,color='grey',linetype='dashed')+
    ggtitle('HCR2')
saveplot(pHCR2.ssb,name="Trebuild_HCR2_ssb",dim=c(17,10),wd='img/mse')

pHCR2.cz <- foreplot(HCR2,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',
              legendnames = OMnam)+ggtitle('HCR2')
saveplot(pHCR2.cz,name="Trebuild_HCR2_cz",dim=c(17,10),wd='img/mse')

df <- foreplot(HCR2,what.y='probCZ',data=TRUE)
rebuildHCR2 <- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))

## combine------------------------------------------------------------------------------

mylegend<-extractLegend(pHCR1.cz + theme(legend.position="bottom"))
lheight <- sum(mylegend$height)
gA <- ggplotGrob(pHCR1.cz + theme(legend.position="none")+ylab(''))
gB <- ggplotGrob(pHCR2.cz+ theme(legend.position="none")+ylab(''))

saveplot(grid.arrange(gtable_rbind(gA, gB),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight),
                      left='Probability out of the Critical Zone'),
         name="Trebuild_min",
         dim=c(11,12),
         wd='img/mse')


dfrebuild <- cbind(rebuildHCR1[,-c(3:4)],HCR2=rebuildHCR2$ny)
names(dfrebuild)[3] <- 'HCR1'
write.csv(dfrebuild, file = "csv/Trebuild.csv")


#--------------------------------------------------------------------------------
#### Objective 1b.	Years until SSB>LRP with 75% probability ####################
#--------------------------------------------------------------------------------
diamond_cz <- diamondplot(sub,what='probCZ',ylab='Years until SSB > LRP (75%)',year='threshold',threshold=0.75,IE=1,IEnames='',hline=c(5,10),OMtype = FALSE)+ 
    scale_color_manual(values=OMcol)+
    scale_shape_manual(values=OMshape)+
    scale_y_continuous(limits=c(0,11),expand = c(0,0))

saveplot(diamond_cz,name="Obj1b_75perc",dim=c(10,8),wd='img/mse')

#--------------------------------------------------------------------------------
#### Objective 2.	Probability of Growth #######################################
#--------------------------------------------------------------------------------
probgrowthCZ <- function(x){
    y <- x[[1]]
    ssb <- y$ssb
    inCZ <- y$ssb<y$CZ #the ones that are in the CZ to start
    ntot <- 0
    ngrow <- 0
    for(i in 2:length(x)){
        y <- x[[i]]
        nssb <- y$ssb
        inCZ[inCZ] <- y$ssb[inCZ]<y$CZ[inCZ] # the ones that are still in the next year
        grow <- nssb[inCZ]>ssb[inCZ]
        #print(length(grow))
        ntot <- ntot + length(grow)
        ngrow <- ngrow + length(grow[grow])
        print(length(grow[grow])/length(grow))
        ssb <- nssb
    }
    round(ngrow/ntot*100,1) 
}

#check heaviest exploit
x=subMSE(runlist,MP=11,OM='stress1')[[4]]
probgrowthCZ(x)

#the rest
dfgrow <- ldply(sub,probgrowthCZ)
dfgrow$OM <- gsub('[0-9]+', '', gsub("OM","",ldply(sub,function(x) attr(x,'OMlabel'))$V1))
dfgrow$MP <- as.numeric(gsub("MP","",ldply(sub,function(x) attr(x,'MPlabel'))$V1))
dfgrow$IE <- as.factor(gsub("IE","",unlist(lapply(strsplit(dfgrow$.id,'[.]'),'[[',3))))
levels(dfgrow$IE) <- IEnam
dfgrow$IE <- factor(dfgrow$IE,levels(dfgrow$IE)[attr(IEnam,'order')])

p <- ggplot(dfgrow,aes(x=MP,y=V1,col=OM,shape=OM))+
    facet_wrap(~IE)+
    geom_point(size=1.5)+labs(col='OM',shape='OM')+
    labs(y='Growth probability (when in CZ)',x='HCR')+
    scale_x_continuous(labels = as.character(unique(dfgrow$MP)), breaks = unique(dfgrow$MP))+
    scale_color_manual(values=OMcol)+
    scale_shape_manual(values=OMshape)+
    scale_y_continuous(limits=c(50,100),expand=c(0,0))

saveplot(p,name="obj2_growthCZ",dim=c(16,8),wd='img/mse')


#--------------------------------------------------------------------------------
#### Milestone 2.	Probability of Growth #######################################
#--------------------------------------------------------------------------------
probgrowthy <- function(x,year){
    y <- x[[1]]
    ssb <- y$ssb
    ntot <- 0
    ngrow <- 0
    for(i in 2:(year+1)){
        y <- x[[i]]
        nssb <- y$ssb
        grow <- nssb>ssb
        ntot <- ntot + length(grow)
        ngrow <- ngrow + length(grow[grow])
        ssb <- nssb
    }
    round(ngrow/ntot*100,1) 
}

#check heaviest exploit
x=subMSE(runlist,MP=11,OM='stress1')[[4]]
probgrowthy(x,3)

# the rest
dfgrowy <- ldply(sub,probgrowthy,3)
names(dfgrowy) <- c('id','y3')
dfgrowy$y5 <- ldply(sub,probgrowthy,5)$V1
dfgrowy$y10 <- ldply(sub,probgrowthy,10)$V1
dfgrowy$OM <- ldply(sub,function(x) attr(x,'OMlabel'))$V1
dfgrowy$type <- gsub('[0-9]+', '', gsub("OM","",dfgrowy$OM))
dfgrowy$MP <- as.numeric(gsub("MP","",ldply(sub,function(x) attr(x,'MPlabel'))$V1))
dfgrowy$IE <- as.factor(gsub("IE","",unlist(lapply(strsplit(dfgrowy$id,'[.]'),'[[',3))))
levels(dfgrowy$IE) <- IEnam
dfgrowy$IE <- factor(dfgrowy$IE,levels(dfgrowy$IE)[attr(IEnam,'order')])


add <- function(p){
    p+facet_wrap(~IE)+
        geom_hline(yintercept=50,col='grey',linetype='dashed')+
        geom_hline(yintercept=95,col='darkgreen',linetype='dashed')+
        geom_point(size=1.5)+
        labs(x='HCR',col='OM',shape='OM')+
        scale_x_continuous(labels = as.character(unique(dfgrowy$MP)), breaks = unique(dfgrowy$MP))+
        scale_color_manual(values=OMcol)+
        scale_shape_manual(values=OMshape)+
        scale_y_continuous(limits=c(0,100),expand=c(0,0))
}

p3 <- add(ggplot(dfgrowy,aes(x=MP,y=y3,col=type,shape=type))+ggtitle('3 years'))
p5 <- add(ggplot(dfgrowy,aes(x=MP,y=y5,col=type,shape=type))+ggtitle('5 years'))
p10 <- add(ggplot(dfgrowy,aes(x=MP,y=y10,col=type,shape=type))+ggtitle('10 years'))

mylegend<-extractLegend(p3+ theme(legend.position="bottom"))
lheight <- sum(mylegend$height)
gA <- ggplotGrob(p3 + theme(legend.position="none")+ylab('')+xlab(''))
gB <- ggplotGrob(p5+ theme(legend.position="none")+ylab('')+xlab(''))
gC <- ggplotGrob(p10+ theme(legend.position="none")+ylab(''))

saveplot(grid.arrange(gtable_rbind(gA, gB, gC),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight),
                      left='Probability of growth'),
         name="Mile2_3y_5y_10y",
         dim=c(15,22),
         wd='img/mse')

range(dfgrowy[,c(2:4)])


obj2 <- cbind(dfgrowy,CZ=dfgrow[,'V1'])
obj2 <- obj2[,-1]
write.csv(obj2, file = "csv/obj2.csv")

#--------------------------------------------------------------------------------
#### Objective 2new.	Avoid decline   #######################################
#--------------------------------------------------------------------------------
probdeclineCZ <- function(x){
    y <- x[[2]]
    ssb <- y$ssb
    inCZ <- y$ssb<y$CZ #the ones that are in the CZ in 2019
    ntot <- 0
    ndecline <- 0
    for(i in 3:length(x)){
        y <- x[[i]]
        nssb <- y$ssb
        inCZ[inCZ] <- y$ssb[inCZ]<y$CZ[inCZ] # the ones that are still in the next year
        decline <- nssb[inCZ]<ssb[inCZ]
        #print(length(grow))
        ntot <- ntot + length(decline)
        ndecline <- ndecline + length(decline[decline])
        print(length(decline[decline])/length(decline))
        ssb <- nssb
    }
    round(ndecline/ntot*100,1) 
}

#check heaviest exploit
x=subMSE(runlist,MP=11,OM='stress1')[[1]]
probdeclineCZ(x)

#the rest
dfdecline <- ldply(sub,probdeclineCZ)
dfdecline$OM <- ldply(sub,function(x) attr(x,'OMlabel'))$V1
dfdecline$MP <- as.numeric(gsub("MP","",ldply(sub,function(x) attr(x,'MPlabel'))$V1))

p <- ggplot(dfdecline,aes(x=MP,y=V1,col=OM,shape=OM))+
    geom_point(size=1.5)+labs(col='OM',shape='OM')+
    labs(y='probability of decline (when in CZ)',x='HCR')+
    scale_x_continuous(labels = as.character(unique(dfdecline$MP)), breaks = unique(dfdecline$MP))+
    scale_color_manual(values=OMcol)+
    scale_shape_manual(values=OMshape)+
    scale_y_continuous(limits=c(0,100),expand=c(0,0))

saveplot(p,name="obj2_declineCZ",dim=c(16,8),wd='img/mse')


#--------------------------------------------------------------------------------
#### Milestone 1b.	SSB/LRP #######################################
#--------------------------------------------------------------------------------
ssblrp <- function(x,quan){
    simssb <- do.call('cbind',llply(x,function(x) x$ssb))
    simcz <- do.call('cbind',llply(x,function(x) x$CZ))
    sim <- simssb/simcz
    q <- round(apply(sim,2,quantile,quan),2)
    q
}

#check heaviest exploit
x=subMSE(runlist,MP=11,OM='stress1')[[1]]
ssblrp(x,0.35)

# the rest
dfratio <- ldply(sub,ssblrp,0.35)
dfratio$OM <- ldply(sub,function(x) attr(x,'OMlabel'))$V1
dfratio$MP <- as.numeric(gsub("MP","",ldply(sub,function(x) attr(x,'MPlabel'))$V1))

add <- function(p){
    p+
    geom_hline(yintercept=1,linetype='dashed',col='darkgrey')+
    geom_point(size=1.5)+labs(col='OM',shape='OM')+
    labs(x='HCR')+
    scale_x_continuous(labels = as.character(unique(dfratio$MP)), breaks = unique(dfratio$MP))+
    scale_color_manual(values=OMcol)+
    scale_shape_manual(values=OMshape)
}

p3 <- add(ggplot(dfratio,aes(x=MP,y=V4,col=OM,shape=OM))+ggtitle('3 years'))
p5 <- add(ggplot(dfratio,aes(x=MP,y=V6,col=OM,shape=OM))+ggtitle('5 years'))

mylegend<-extractLegend(p3+ theme(legend.position="bottom"))
lheight <- sum(mylegend$height)
gA <- ggplotGrob(p3 + theme(legend.position="none")+ylab(''))
gB <- ggplotGrob(p5+ theme(legend.position="none")+ylab('')+xlab(''))

saveplot(grid.arrange(gtable_rbind(gA, gB),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight),
                      left='SSB / LRP (65th quantile)'),
         name="Mile1b_3y_5y",
         dim=c(11,12),
         wd='img/mse')

mile1b <- dfratio[,-1]
names(mile1b)[1:26] <- 2018:(2018+25)
write.csv(mile1b, file = "csv/mile1b.csv")

#--------------------------------------------------------------------------------
#### catch/TAC #################################
#--------------------------------------------------------------------------------
foreplot(sub,what.y = 'TAC',by='MP',year=2019:2030) #seems like TAC is increasing too fast or soon because it drops directly after
OMcol2 <- c('black',brewer.pal(4,'Greens'),brewer.pal(3,'Reds'))

c1 <- diamondplot(sub,what='TAC',ylab='Average TAC',xlab='HCR',year=c(2019:2029),OMtype = FALSE)+ 
    scale_color_manual(values=OMcol2)+
    scale_shape_manual(values=OMshape)

c2 <- diamondplot(sub,what='catch',ylab='Average catch',xlab='HCR',year=c(2019:2029),OMtype = FALSE)+ 
    scale_color_manual(values=OMcol2)+
    scale_shape_manual(values=OMshape)

saveplot(c1,name="Obj_TAC",dim=c(16,8),wd='img/mse')
saveplot(c2,name="Obj_catch",dim=c(16,8),wd='img/mse')


#--------------------------------------------------------------------------------
#### Trade-off: catch - prob CZ #################################
#--------------------------------------------------------------------------------

trade <- extract(sub,'probCZ',add=T)
trade$TAC <- extract(sub,'TAC',add=T)[,1]
trade$MP <- as.numeric(gsub('MP','',trade$MP))
trade$type <- gsub('[0-9]+', '', gsub("OM","",trade$OM))
trade <- trade[order(trade$MP),]
trade <- trade[trade$year %in% c(2021,2023,2028) & trade$type %in% c('base'),]
trade <- trade[trade$MP != 3,]

ggplot(trade,aes(x=TAC,y=var,col=MP))+geom_point()+
    facet_wrap(~year)+
    scale_color_viridis_c()+
    labs(y='Probability out of CZ',col='HCR')+
    theme(strip.background = element_blank())

## see DLMtools

#--------------------------------------------------------------------------------
#### Exceptional circumstances #################################
#--------------------------------------------------------------------------------
runmp <- subMSE(runlist,MP=c(3:11))

ind <- function(x,quan){
    simindex <- do.call('cbind',llply(x,function(x) x$index))
    q <- t(round(apply(simindex,2,quantile,quan),2))
    colnames(q) <- c('q1','median','q3') 
    q <- cbind(q,year=1:nrow(q)+2017)
}
dfindex <- ldply(runmp,ind,c(0.95,.5,0.05))
dfindex$OM <- sapply(strsplit(dfindex$.id,'[.]'),'[[',1)
dfindex$MP <- as.numeric(gsub('MP','',sapply(strsplit(dfindex$.id,'[.]'),'[[',2)))

ggplot(dfindex[dfindex$OM=='OMcore4' & dfindex$MP==4,],aes(x=year,y=median))+
    #geom_ribbon(aes(ymin=q1,ymax=q3),fill='grey')+
    geom_line()+
    facet_grid(OM~MP)



#--------------------------------------------------------------------------------
#### Other graphs ###############################################################
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#### Extinction #######################################
#--------------------------------------------------------------------------------

pext <- diamondplot(sub,what='percExtinct',ylab='Percentage Extinct',xlab='HCR',year=c(2019:2029),OMtype = FALSE)+ 
    scale_color_manual(values=OMcol)+
    scale_shape_manual(values=OMshape)

saveplot(pext,name="Extinction",dim=c(16,8),wd='img/mse')


#--------------------------------------------------------------------------------
#### Effect Rec, C and M #######################################
#--------------------------------------------------------------------------------

probgrowthyall <- function(x,year){
    y <- x[[1]]
    ssb <- y$ssb
    res <- c()
    for(i in 2:(year+1)){
        y <- x[[i]]
        nssb <- y$ssb
        grow <- nssb>ssb
        ntot <- length(grow)
        ngrow <- length(grow[grow])
        ssb <- nssb
        res <- c(res,round(ngrow/ntot*100,1) )
    }
    return(res)
}

#### REC
rec <- subMSE(runlist,OM=c('base','core1','stress1'))

recdf <- extract(rec,'probCZ',add=TRUE)
recdf$type <- gsub('[0-9]+', '', gsub("OM","",recdf$OM))
recdf$MP <- as.numeric(gsub("MP","",recdf$MP))
recdf$IE <- as.factor(gsub("IE","",unlist(lapply(strsplit(recdf$IE,'[.]'),'[[',1))))
levels(recdf$IE) <- IEnam
recdf$IE <- factor(recdf$IE,levels(recdf$IE)[attr(IEnam,'order')])
recdf <- recdf[recdf$year %in% c(2019:2028),]
recdf$year <- factor(recdf$year)
recdf$growth <- do.call('c',llply(rec,probgrowthyall,10))
recdf <- recdf[!recdf$MP %in% c(1,3) & !recdf$IE=='no Canadian missing',]


p1 <- ggplot(recdf,aes(x=year,y=var*100,col=OM))+geom_boxplot()+
    facet_wrap(~MP)+
    scale_x_discrete(breaks = levels(recdf$year)[c(T, rep(F, 4))])+
    scale_color_viridis_d()+
    ggtitle('Recruitment OMs')+
    labs(y='Probability SSB > LRP',x='Year')

p2 <- ggplot(recdf,aes(x=year,y=growth,col=OM))+geom_boxplot()+
    facet_wrap(~MP)+
    scale_x_discrete(breaks = levels(recdf$year)[c(T, rep(F, 4))])+
    scale_color_viridis_d()+
    ggtitle('Recruitment OMs')+
    labs(y='Probability SSBy+1 > SSBy',xlab='Year')

saveplot(p1,name="proj_rec_LRP",dim=c(14,12),wd='img/mse')
saveplot(p2,name="proj_rec_growth",dim=c(14,12),wd='img/mse')

#### M
nm <- subMSE(runlist,OM=c('base','core3','stress1'))

nmdf <- extract(nm,'probCZ',add=TRUE)
nmdf$type <- gsub('[0-9]+', '', gsub("OM","",nmdf$OM))
nmdf$MP <- as.numeric(gsub("MP","",nmdf$MP))
nmdf$IE <- as.factor(gsub("IE","",unlist(lapply(strsplit(nmdf$IE,'[.]'),'[[',1))))
levels(nmdf$IE) <- IEnam
nmdf$IE <- factor(nmdf$IE,levels(nmdf$IE)[attr(IEnam,'order')])
nmdf <- nmdf[nmdf$year %in% c(2019:2028),]
nmdf$year <- factor(nmdf$year)
nmdf$growth <- do.call('c',llply(nm,probgrowthyall,10))
nmdf <- nmdf[!nmdf$MP %in% c(1,3) & !nmdf$IE=='no Canadian missing',]


p1 <- ggplot(nmdf,aes(x=year,y=var*100,col=OM))+geom_boxplot()+
    facet_wrap(~MP)+
    scale_x_discrete(breaks = levels(nmdf$year)[c(T, rep(F, 4))])+
    scale_color_viridis_d()+
    ggtitle('natural mortality OMs')+
    labs(y='Probability SSB > LRP',x='Year')

p2 <- ggplot(nmdf,aes(x=year,y=growth,col=OM))+geom_boxplot()+
    facet_wrap(~MP)+
    scale_x_discrete(breaks = levels(nmdf$year)[c(T, rep(F, 4))])+
    scale_color_viridis_d()+
    ggtitle('natural mortality OMs')+
    labs(y='Probability SSBy+1 > SSBy',xlab='Year')

saveplot(p1,name="proj_nm_LRP",dim=c(14,12),wd='img/mse')
saveplot(p2,name="proj_nm_growth",dim=c(14,12),wd='img/mse')

#### C
catch <- subMSE(runlist,OM=c('base','core3','stress3'))

catchdf <- extract(catch,'probCZ',add=TRUE)
catchdf$type <- gsub('[0-9]+', '', gsub("OM","",catchdf$OM))
catchdf$MP <- as.numeric(gsub("MP","",catchdf$MP))
catchdf$IE <- as.factor(gsub("IE","",unlist(lapply(strsplit(catchdf$IE,'[.]'),'[[',1))))
levels(catchdf$IE) <- IEnam
catchdf$IE <- factor(catchdf$IE,levels(catchdf$IE)[attr(IEnam,'order')])
catchdf <- catchdf[catchdf$year %in% c(2019:2028),]
catchdf$year <- factor(catchdf$year)
catchdf$growth <- do.call('c',llply(catch,probgrowthyall,10))
catchdf <- catchdf[!catchdf$MP %in% c(1,3) & !catchdf$IE=='no Canadian missing',]


p1 <- ggplot(catchdf,aes(x=year,y=var*100,col=OM))+geom_boxplot()+
    facet_wrap(~MP)+
    scale_x_discrete(breaks = levels(catchdf$year)[c(T, rep(F, 4))])+
    scale_color_viridis_d()+
    ggtitle('catch OMs')+
    labs(y='Probability SSB > LRP',x='Year')

p2 <- ggplot(catchdf,aes(x=year,y=growth,col=OM))+geom_boxplot()+
    facet_wrap(~MP)+
    scale_x_discrete(breaks = levels(catchdf$year)[c(T, rep(F, 4))])+
    scale_color_viridis_d()+
    ggtitle('catch OMs')+
    labs(y='Probability SSBy+1 > SSBy',xlab='Year')

saveplot(p1,name="proj_catch_LRP",dim=c(14,12),wd='img/mse')
saveplot(p2,name="proj_catch_growth",dim=c(14,12),wd='img/mse')

#--------------------------------------------------------------------------------
#### TO DELETE #################################################################
#--------------------------------------------------------------------------------




#### Age structure ##############################################################################################################
library(stringr)
getMedage <- function(x){ 
    medAge <- lapply(x,function(y){
        m <- exp(y$sim[1:10,])
        median(rowSums(sweep(m,2,1:10,'*'))/rowSums(m))
    })
    df=data.frame(a=unlist(medAge),
                  year=2016:(2016+25),
                  OM=unlist(lapply(x,function(y){attr(x,"OMlabel")})),
                  MP=unlist(lapply(x,function(y){attr(x,"MPlabel")})),
                  IE=paste0(as.character(attr(x,'parameters')$IE),collapse = '.'))
    return(df)
}
medAge <- do.call('rbind',lapply(myrunlist,getMedage))
medAge$IE <- str_match(medAge$IE, "IE(.*?).IE")[,2]

savepng(
    ggplot(medAge[!medAge$MP %in% c('MP2','MP12','MP13','MP14') & medAge$OM=='OMbase' & !medAge$year %in% c(2016) ,],aes(x=year,y=a,col=MP))+geom_line(size=1)+
        facet_wrap(~IE)+
        ylab('Fish age')+xlab('Year')+ggtitle('OMbase')
    ,wdIMG,'/MSE/HalifaxDec/Timeseries_age',c(20,14))

## average age at the moment we were still in CZ or HZ
ssbpast<-ssbtable(fitBase)
LRP <- ypr(fitBase)$f40ssb*0.4
URP <- ypr(fitBase)$f40ssb*0.8

yc <- ssbpast[max(which(ssbpast[,1]>LRP)),'year']
yh <- ssbpast[max(which(ssbpast[,1]>URP)),'year']

n <- ntable(fitBase)
a <- rowSums(sweep(n,2,1:10,'*'))/rowSums(n)
ac <- round(a[which(names(a)==yc)],2)
ah <- round(a[which(names(a)==yh)],2)

savepng(
    ggplot(medAge[!medAge$MP %in% c('MP2','MP12','MP13','MP14') & medAge$OM=='OMbase' & !medAge$year %in% c(2016) ,],aes(x=year,y=a,col=MP))+geom_line(size=1)+
        facet_wrap(~IE)+
        ylab('Fish age')+xlab('Year')+ggtitle('OMbase')+
        geom_hline(yintercept = ac,col='red')+
        geom_hline(yintercept = ah,col='green')
    ,wdIMG,'/MSE/HalifaxDec/Timeseries_age_critheal',c(20,14))

#### shit ##############################################################################################################


## trade off plots by OM or MP or IE
name='2018.08.16'
savepng(foreplot(myrunlist,what.y='ssb',ci=FALSE,ylab='SSB'),wdIMG,paste0('/MSE/',name,'/Ibased_ssb'),c(15,10))
savepng(ssbplot(myrunlist,ci=FALSE)+scale_y_continuous(limits=c(0,800000)),wdIMG,paste0('/MSE/',name,'/Ibased_ssb_full'),c(15,10))

savepng(foreplot(myrunlist,what.y='catch',ci=FALSE),wdIMG,paste0('/MSE/',name,'/Ibased_catch'),c(15,10))
savepng(foreplot(myrunlist,what.y='TAC',ci=FALSE),wdIMG,paste0('/MSE/',name,'/Ibased_TAC'),c(15,10))

savepng(fbarplot(myrunlist,ci=FALSE),wdIMG,paste0('/MSE/',name,'/Ibased_Fbar_full'),c(15,10))
savepng(foreplot(myrunlist,what.y='fbar',ci=FALSE),wdIMG,paste0('/MSE/',name,'/Ibased_fbar'),c(15,10))

savepng(foreplot(myrunlist,what.y='probCZ',ci=FALSE,hline=0.75,ylab = 'Probability out of CZ'),wdIMG,paste0('/MSE/',name,'/Ibased_probCZ'),c(15,10))
savepng(foreplot(myrunlist,what.y='probHZ',ci=FALSE,hline=0.75,ylab='Probability into the HZ'),wdIMG,paste0('/MSE/',name,'/Ibased_probHZ'),c(15,10))

savepng(foreplot(myrunlist,what.y='Fmsyratio',ci=FALSE,hline=0.75,ylab='F/F40%'),wdIMG,paste0('/MSE/',name,'/ctC_Fmsyratio'),c(15,10))

#### Objectives ##############################################################################################################



## Objective 3: Maximize annual catches
savepng(foreplot(runlist,what.y='catch',by=c('OM','MP'),IE=1,ci=FALSE),wdIMG,'/MSE/HalifaxSept/obj3_catch',c(40,25))
savepng(diamondplot(runlist,what='catch',ylab='Average catch',IE=1,IEnames=IEnam,year = c(2019:2014))+ scale_color_manual(values=c('orange','grey25','grey60')),wdIMG,'/MSE/HalifaxSept/obj3_Caver_diamond_ally',c(25,14))
savepng(diamondplot(runlist,what='catchcumul',ylab='Cumulative catch',IE=1,IEnames=IEnam,year=2016+25)+ scale_color_manual(values=c('orange','grey25','grey60')),wdIMG,'/MSE/HalifaxSept/obj3_TACaver_diamond_ally',c(25,14))


write.table(diamondplot(runlist,what='catch',year=2022:2026,IE=1,IEnames=IEnam,data=TRUE),
            'Cave_20222026.txt', dec = ".")
write.table(diamondplot(runlist,what='TAC',year=2022:2026,IE=1,IEnames=IEnam,data=TRUE),
            'TACave_20222026.txt', dec = ".")
write.table(diamondplot(runlist,what='catch',year=2017:2021,IE=1,IEnames=IEnam,data=TRUE),
            'Cave_20172021.txt', dec = ".")
write.table(diamondplot(runlist,what='TAC',year=2017:2021,IE=1,IEnames=IEnam,data=TRUE),
            'TACave_20172021.txt', dec = ".")

df=extract(runlist,'catch',add = TRUE)
names(df)[1]='IEcan'
names(df)[2]='IEusa'
df=df[,-3]
df=df[df$year!=min(df$year),]
newdf<-do.call('rbind',lapply(strsplit(names(runlist),'[.]'),function(x){
    run=runlist[[paste(x,collapse='.')]]
    d <- df[df$OM==x[1] & df$MP==x[2] & df$IE==paste(x[3:4],collapse='.'),]
    d[,'IEcan']=apply(attr(run,'IE')[[1]],2,median)
    d[,'IEusa']=apply(attr(run,'IE')[[2]],2,median)
    return(d)
}))
head(newdf[newdf$id==6,])

dfc =extract(runlist,'catch',add = TRUE)
dft =extract(runlist,'TAC',add = TRUE)
dfc=dfc[dfc$year!=min(dfc$year),]
dft=dft[dft$year!=min(dft$year),]

newdf$catch=dfc$catch.median
newdf$tac=dft$TAC.median
newdf$type <- gsub('[0-9]+', '', gsub("OM","",newdf$OM))

write.table(newdf,'catchbytype_ally.txt', dec = ".")

mydf1 <- newdf[newdf$year %in% 2017:2021,]
mydf1 <- ddply(mydf1,c('OM','MP','IE','id','type'),summarise,IEcan=median(IEcan),IEusa=median(IEusa),catch=median(catch),tac=median(tac))

mydf2 <- newdf[newdf$year %in% 2022:2026,]
mydf2 <- ddply(mydf2,c('OM','MP','IE','id','type'),summarise,IEcan=median(IEcan),IEusa=median(IEusa),catch=median(catch),tac=median(tac))

mydf1$potcatch=mydf1$IEcan+mydf1$IEusa+mydf1$tac
mydf2$potcatch=mydf2$IEcan+mydf2$IEusa+mydf2$tac
write.table(mydf1,'catchbytype_20172021.txt', dec = ".")
write.table(mydf2,'catchbytype_20222026.txt', dec = ".")

mydf1$diff=mydf1$potcatch-mydf1$catch

## Obective 4: maximise fishery stability
foreplot(runlist,what.y='TACrel',by=c('OM','MP'),IE=1,ci=FALSE)

byopt <- c('OM','IE','MP')
if(!is.null(by) & !all(by %in% byopt)){stop('"by" may only include OM, IE or MP')}

# create data frame
df <- extract(x,'TACrel',add=TRUE)
colnames(df)[1]='y'
if(!is.null(IE)){
    df$IE <- unlist(lapply(strsplit(df$IE,'[.]'),'[[',IE))
}
MPnum <- as.numeric(unlist(lapply(strsplit(df$MP, "\\D+"),'[[',2)))
if(length(MPnum)>0) df$MP <- MPnum
savepng(ggplot(df,aes(x=1,y=y))+geom_violin(draw_quantiles = TRUE,fill='black')+facet_wrap(.~MP,scale='free_y')+labs(y='relative change in TAC',x=''),wdIMG,'/MSE/HalifaxSept/obj4_TACrel_violin_ally',c(25,14))






