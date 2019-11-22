#################################################################################################################
#*** Mackerel MSE
#*** Compare different OM model fits
#################################################################################################################

load(file='Rdata/OMs/fitBase.Rdata')
load(file='Rdata/OMs/fitC2.Rdata')
load(file='Rdata/OMs/fitC1.Rdata')
load(file='Rdata/OMs/fitM.Rdata')

OMfits <- c(fitBase=fitBase,FitM=fitM,fitC.50.75=fitC1,fitC.0.25=fitC2)
names(OMfits) <- c('OMbase','OMcore2','OMcore4','OMstress2')

### plots
p1 <- ssbplot(OMfits,ci=FALSE)+scale_y_continuous(limits=c(0,8e5),expand=c(0,0))
p2 <- catchplot(OMfits,ci=FALSE)+scale_y_continuous(limits=c(0,1e5),expand=c(0,0))+ylab('Catch')
saveplot(p1,name="OMssb",dim=c(17,10),wd='img/fit_compare')
saveplot(p2,name="OMcatch",dim=c(17,10),wd='img/fit_compare')

saveplot(plot(OMfits,ci=FALSE,linesize=1),name="OMall",dim=c(11,13),wd='img/fit_compare')


catch <- catchtable(OMfits)
catch$low <-NA
catch$high <-NA
e <-lapply(names(OMfits),function(x){
    d <- OMfits[[x]]$data
    ix <- d$idx1[1,]+1
    catch[catch$fit==x,c('low','high')]<<-exp(d$logobs[ix,])
})
catch <- melt(catch[,-c(2,3)],id=c('year','fit'))
pc<-ggplot(catch,aes(x=year,y=value,col=variable))+geom_line(aes(size=variable))+
    facet_wrap(~fit)+
    scale_color_manual(values=c('black','darkgrey','darkgrey'))+
    labs(col='',linetype='',y='Catch (t)',x='Year')+
    scale_size_manual(values=c(1,0.3,0.3))+
    theme(legend.position = 'none')
    
saveplot(pc,name="OMcatch_limits",dim=c(14,10),wd='img/fit_compare')


### parameters
y <- partable(OMfits[[1]])[,1:2]
y <- rbind(y,AIC=c(round(AIC(OMfits[[1]]),0),''))
e <- lapply(OMfits[c(2:length(OMfits))],function(x){
    n <- partable(x)[,1:2]
    n <- rbind(n,AIC=c(round(AIC(x),0),''))
    y <<- cbind(y,n)
})
y

write.csv(y, "csv/para.csv")

### reference points
ssb <- ssbtable(OMfits)
ssb0 <- ssb0table(OMfits)
f <- fbartable(OMfits)
rp <- ypr(OMfits)
rp <- ldply(rp,function(x) unlist(x[c('f40','f40ssb')]))
rp$LRP <- rp$f40ssb*0.4
rp$USR <- rp$f40ssb*0.8
names(rp)[1:3] <- c('OM','F40%','SSBF40%')
rp$f <- round(f[f$year==max(f$year),'Estimate'],2)
rp$SSB <- ssb0[ssb0$year==max(ssb0$year),'Estimate']
rp[,c(3,4,5,7)] <- round(rp[,c(3,4,5,7)]/1000,2)
rp$LRP.SSB <- round(rp$SSB/rp$LRP,2)
rp

write.csv(rp, "csv/rp.csv")


