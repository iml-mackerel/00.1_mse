#################################################################################################################
#*** Mackerel MSE
#*** Objective: catch
#*** split up between the US and Canada?
#################################################################################################################

#******************************************************************************
#************* Load and prepare ***********************************************
#******************************************************************************

load(file='Rdata/runlist.2019.04.09.Rdata')

### raw data
catch <- raw(runlist,'catch')
TAC <- raw(runlist,'TAC')
IE <- raw(runlist,'IE')
IE$statedim <- revalue(IE$statedim, replace = c("IEconstant" = "IEcan",
                                     "IEindep2019" = "IEcan",
                                     "IEdep2550" = "IEusa",
                                     "IEdep5075" = "IEusa",
                                     "IEdep0025" = "IEusa",
                                     "IEdepcopy" = "IEusa"))
IE <- dcast(IE,.id+nsim+year+MP+OM~statedim,value.var = 'IE')
IE[is.na(IE$IEusa),'IEusa'] <- 0

df <- merge(cbind(catch,TAC=TAC$TAC),IE)
df[,c('catch','TAC','IEcan','IEusa')] <- round(df[,c('catch','TAC','IEcan','IEusa')],0)
df$pot <- df$TAC+df$IEcan+df$IEusa
df$missing <- df$pot-df$catch
df$MP <- as.numeric(gsub('MP','',df$MP))

save(df, file='Rdata/df_obj3.Rdata')

#******************************************************************************
#*************Analyse ***********************************************
#******************************************************************************

load(file='Rdata/df_obj3.Rdata')

phist <- ggplot(df[df$missing <20000 & !df$MP %in% 1:2,],aes(x=missing))+geom_histogram()+
    scale_y_continuous(expand = c(0,0),limits=c(0,0.2*10^6))+
    facet_wrap(~MP)
    
round(nrow(df[df$missing>1,])/nrow(df),2) # fraction of all sims were Cpot was not attained

head(df[df$missing>1000,])

### three options
## If potential not attained: Canada takes first
df$can1 <- ifelse(df$missing<1,df$catch-df$IEusa,ifelse(
    df$missing<df$IEusa,df$TAC+df$IEcan,df$catch
))
## If potential not attained: US takes first (even TAC canada is not prioritised)
df$can2 <- ifelse(df$missing<1,df$catch-df$IEusa,ifelse(
    df$missing<df$IEusa,df$catch-df$missing,df$catch-df$IEusa
))
df$can2 <- pmax(0,df$can2)

## If potential not attained: something in between
percusa <- df$IEusa/df$pot
hist(percusa)

set.seed(123)
IEfrac <- rnorm(nrow(df),0.5,0.1)    #hist(IEfrac);range(IEfrac)
IEfrac[df$missing<=1] <- 1
df$IEfrac <- IEfrac
df$can3 <- pmax(0,df$catch-pmin(IEfrac*df$IEusa,df$missing)) # IEusa should never be more than missing
df[df$missing<1,'can3'] <- df[df$missing<1,'catch']-df[df$missing<1,'IEusa'] # if potential catch is attained


par(mfrow=c(3,1))
hist(df[df$can1<30000,'can1'],xlim=c(0,30000),main='Canada first')
hist(df[df$can2<30000,'can1'],xlim=c(0,30000),main='US first')
hist(df[df$can3<30000,'can1'],xlim=c(0,30000),main='US roughly 50%')

save(df, file='Rdata/df_obj3.Rdata')

#******************************************************************************
#*************plot ***********************************************
#******************************************************************************

load(file='Rdata/df_obj3.Rdata')
dfmed <- ddply(df[df$year %in% 2019:2024,],c('OM','MP'),summarise,can1=median(can1),can2=median(can2),can3=median(can3))

ggadd3 <- function(p){
    p+geom_point(size=1.5)+labs(col='OM',shape='OM')+
        scale_x_continuous(labels = as.character(unique(dfmed$MP)), breaks = unique(dfmed$MP))+
        scale_color_manual(values=OMcol)+
        scale_shape_manual(values=OMshape)+
        scale_y_continuous(expand=c(0,0),limits=c(0,13000))+
        #geom_vline(xintercept=2.5,linetype='dotted')+
        theme(strip.background = element_blank())
}

### Plot to compare influence split after 5 years
p1 <- ggadd3(ggplot(dfmed,aes(x=MP,y=can1,col=OM,shape=OM)))
p2 <-ggadd3(ggplot(dfmed,aes(x=MP,y=can2,col=OM,shape=OM)))
p3 <-ggadd3(ggplot(dfmed,aes(x=MP,y=can3,col=OM,shape=OM)))

mylegend<-extractLegend(p1+theme(legend.position="bottom"))
lheight <- sum(mylegend$heights)
gA <- ggplotGrob(p1 + theme(legend.position="none")+labs(x='',y='Catch (t)')+ggtitle('0% US'))
gB <- ggplotGrob(p3 + theme(legend.position="none")+labs(x='HCR',y='')+ggtitle('~50% US'))
gC <- ggplotGrob(p2 + theme(legend.position="none")+labs(x='',y='')+ggtitle('100% US'))

saveplot(grid.arrange(gtable_cbind(gA, gB, gC),
                      mylegend,
                      ncol = 1,
                      heights = unit.c(unit(1, "npc") - lheight, lheight)),
         name="Obj3_5y_3options",
         dim=c(20,7),
         wd='img/mse')







