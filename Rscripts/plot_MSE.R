#################################################################################################################
#*** Mackerel MSE
#*** Plot random MSE results
#################################################################################################################

#******************************************************************************
#************* Load all predictions *******************************************
#******************************************************************************

load(file='Rdata/runlist.2019.04.09.Rdata')

#******************************************************************************
#************* plots **********************************************************
#******************************************************************************

sub <- subMSE(runlist,MP=c(2:11))
OMnam <- c('OMbase','OMcore1','OMcore2','OMcore3','OMcore4','OMstress1','OMstress2','OMstress3')
OMshape <- c(17,16,16,16,16,16,16,16)
OMcol <- c('black',brewer.pal(5,'Blues')[5:2],brewer.pal(4,'Reds')[4:2])
threshold <- 0.75

#--------------------------------------------------------------------------------
#### Example runs for ppt ssb  ##################################################
#--------------------------------------------------------------------------------
base <- subMSE(runlist,MP=1,OM='base')[[1]]
saveplot(ssb0plot(base,years=2005:2030),name="forecast_ssb_MP1_OMbase",dim=c(8,6),wd='img/MSE')

baserec <- subMSE(runlist,MP=c(1,8),OM='base') # effect BH on recruitment
saveplot(recplot(baserec,years=1969:2030,ci=FALSE),name="forecast_ssb_MP1_OMbase",dim=c(8,6),wd='img/MSE')

nmex <- subMSE(runlist,OM=c('base','core3','stress1'),MP=1)
saveplot(ssb0plot(nmex,ci=FALSE),name="forecast_ssb_MP1_OMnm",dim=c(12,6),wd='img/MSE')

## when F0 -----------------------------------------------------------------------
F0 <- subMSE(runlist,MP=1,OM=c('base','core1','core2'))
p1 <- recplot(F0,ci=TRUE,years=2000:2029,legendnames=c(1:3))
p2 <- ssb0plot(F0,ci=TRUE,years=2000:2029,legendnames=c(1:3))

saveplot(grid.arrange(p1,p2,ncol=2),name='forecast_SSBrec',dim=c(16,10),wd='img/resdoc')

#--------------------------------------------------------------------------------
#### Minimum Time for Rebuilding ################################################
#--------------------------------------------------------------------------------

## when F0 -----------------------------------------------------------------------

HCR1 <- subMSE(runlist,MP=1)

pHCR1.ssb <- ssb0plot(HCR1,ci=FALSE,legendnames=OMnam)+geom_vline(xintercept=2018,color='grey',linetype='dashed')+ggtitle('HCR1')
saveplot(pHCR1.ssb,name="Trebuild_HCR1_ssb",dim=c(17,10),wd='img/mse')

pHCR1.cz <- foreplot(HCR1,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = OMnam)+ggtitle('HCR1')
saveplot(pHCR1.cz,name="Trebuild_HCR1_cz",dim=c(17,10),wd='img/mse')

df <- foreplot(HCR1,what.y='probCZ',data=TRUE)
rebuildHCR1 <- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))

## when TAC0 but still missing  ---------------------------------------------------

HCR2 <- subMSE(runlist,MP=2)

pHCR2.ssb <- ssb0plot(HCR2,ci=FALSE,legendnames=OMnam)+
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

