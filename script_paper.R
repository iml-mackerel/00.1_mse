#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** based on CCAM package
#*** analyses for publication in peer-reviewed paper (focus on missing catch)
#################################################################################################################

wdimg <- 'img/paper/'
wdRdata <- 'Rdata/paper/'
wdrun <- paste0(wdRdata,'runs/')
legsus <- c('0-25%','25-50%','50-75%','75-100%')
legscan <- c('0 t',paste0('decrease to ~',c(seq(1000,4000,1000),500),' t'))
attr(legscan,'order') <- c(1,6,2:5)

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################

cn <- read.ices("data/cn.dat")
ct <- read.ices("data/ct.dat")
ctUSA <- read.ices("data/ctUSA.dat")
ctForeign <- read.ices("data/ctForeign.dat")
cw <- read.ices("data/cw.dat")
dw <- read.ices("data/dw.dat")
lf <- read.ices("data/lf.dat")
lw <- read.ices("data/lw.dat")
mo <- read.ices("data/mo.dat")
mo <- smoothmatrix(mo,subset=8:nrow(mo),max=1,plot=FALSE)
nm <- read.ices("data/nm.dat")
nm[]<- 0.27
pf <- read.ices("data/pf.dat")
pm <- read.ices("data/pm.dat")
sw <- read.ices("data/sw.dat")
sw0 <- read.ices("data/sw0.dat")
surveys <- read.ices("data/survey.dat")
surveys[[1]] <- surveys[[1]][!is.na(surveys[[1]]),1,drop=FALSE]
attr(surveys[[1]],'time') <- c(0.47)
tep <- read.ices("data/tep.dat")
tep[[1]] <- tep[[1]][!is.na(tep[[1]]),1,drop=FALSE]
tep[[1]][,1] <- tep[[1]][,1]*1000000000000
attr(tep[[1]],'time') <- c(0.47)
pfem <- read.ices("data/propFemale.dat")
fec <- read.ices("data/fec.dat")

# define catch limits (start with 0-25%)
ct1 <- ct
ct1[,1] <- ct[,1]*1.10 + ctUSA[-c(1:8),1]*0
ct1[,2] <- ct[,2] + ctUSA[-c(1:8),1]*0.25

dat <- setup.ccam.data(surveys=tep,
                       residual.fleet=cn,
                       total.catch=ct1,
                       prop.mature=mo,
                       stock.mean.weight=sw,
                       stock.start.weight=sw0,
                       catch.mean.weight=cw,
                       dis.mean.weight=dw,
                       land.mean.weight=lw,
                       prop.f=pf,
                       prop.m=pm,
                       natural.mortality=nm,
                       land.frac=lf,
                       prop.fem=pfem,
                       fec=fec)

conf <- defcon(dat)
conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
conf$keyVarObs[1,]=-1                     
conf$keyVarObs[2,1:9]=c(0,1,2,2,2,2,2,2,1) 
conf$keyVarObs[3,1]=3           
conf$stockRecruitmentModelCode=2 #0: RW, 1: ricker, 2: BH, 3:mean
conf$obsLikelihoodFlag[1]='CE'
conf$keyBiomassTreat[3]=5
conf$fbarRange=c(5,10) #fully recruited fish

par <- defpar(dat,conf)

#################################################################################################################
########### fit OM1 #############################################################################################
#################################################################################################################

    fit1 <- ccam.fit(dat,conf,par,silent=FALSE,paracheck = FALSE,debug=TRUE)            # censored
    fit1
    
    save(fit1, file=paste0(wdRdata,'OMfit1.Rdata'))
    #load(file='Rdata/paper/OMfit1.Rdata')
    
    ccam.fit(fit1$data,fit1$conf,defpar(fit1$data,fit1$conf),debug=TRUE,newtonsteps = 0) 
    

#################################################################################################################
########### MSE #################################################################################################
#################################################################################################################

#***************************************************************************
#************* define Operating Models *************************************
#***************************************************************************
ny=25
nosim=2000

#--------------------- OM1 ------------------------------------------
OM1 <- list(fit=fit1,
               nosim=nosim,
               OMlabel='OM1',
               IE=c('IEconstant','IEdep0025'),
               year.base=2018,
               ave.years=tail(fit1$data$years,25),
               rec.years=1969:2016,
               rec.meth=1, #1: BH AC
               UL.years=tail(fit1$data$years,25),
               deadzone=1000,
               Flim=2.5,
               fleet=3)

copy(x=OM1,n=c(4),name='OM')

# --------------------- OM 2 to 4 ------------------------------------------
ct2 <- ct3 <- ct4 <- ct

ct2[,1] <- ct[,1]*1.10 + ctUSA[-c(1:8),1]*0.25
ct2[,2] <- ct[,2] + ctUSA[-c(1:8),1]*0.50

ct3[,1] <- ct[,1]*1.10 + ctUSA[-c(1:8),1]*0.50
ct3[,2] <- ct[,2] + ctUSA[-c(1:8),1]*0.75

ct4[,1] <- ct[,1]*1.10 + ctUSA[-c(1:8),1]*0.75
ct4[,2] <- ct[,2] + ctUSA[-c(1:8),1]*1

dfc <- data.frame(cbind(rbind(ct1,ct2,ct3,ct4)),OM=rep(1:4,each=nrow(ct)),year=as.numeric(as.character(rownames(ct))))

dat2 <- dat3 <- dat4 <- dat
dat2$logobs[which(!is.na(dat2$logobs[,2])),] <- log(ct2)
dat3$logobs[which(!is.na(dat3$logobs[,2])),] <- log(ct3)
dat4$logobs[which(!is.na(dat4$logobs[,2])),] <- log(ct4)

fit2 <- ccam.fit(dat2,conf,par)     # run phase 1 + censored
fit3 <- ccam.fit(dat3,conf,par)     # run phase 1 + censored
fit4 <- ccam.fit(dat4,conf,par)     # run phase 1 + censored

    save(fit2, file=paste0(wdRdata,'OMfit2.Rdata'))
    save(fit3, file=paste0(wdRdata,'OMfit3.Rdata'))
    save(fit4, file=paste0(wdRdata,'OMfit4.Rdata'))
    #load(file=paste0(wdRdata,'OMfit2.Rdata'))
    #load(file=paste0(wdRdata,'OMfit3.Rdata'))
    #load(file=paste0(wdRdata,'OMfit4.Rdata'))
    
OM2$fit <- fit2
OM2$IE[2] <- c('IEdep2550')
OM3$fit <- fit3
OM3$IE[2] <- c('IEdep5075')
OM4$fit <- fit4
OM4$IE[2] <- c('IEdep75100')

# --------------------- OM list ------------------------------------------
OM.list=list(OM1=OM1,
             OM2=OM2,
             OM3=OM3,
             OM4=OM4)

# --------------------- OM comparisons ------------------------------------------
OMfits=c(fit1=fit1,fit2=fit2,fit3=fit3,fit4=fit4)

save(OMfits, file=paste0(wdRdata,'OMfits.Rdata'))
#load(file=paste0(wdRdata,'OMfits.Rdata'))

OMdf <- data.frame(modeltable(OMfits))
OMdf$LRP <- laply(ypr(OMfits),'[[','LRP')
OMdf$ssb0 <- subset(ssb0table(OMfits),year==2018)$Estimate
OMdf$ssb <- subset(ssbtable(OMfits),year==2018)$Estimate
OMdf$CZ <- OMdf$ssb0/OMdf$LRP

savepng(ssbplot(OMfits,ci=FALSE,linesize=1,legendnames=legsus)+scale_y_continuous(limits=c(0,5e5),expand=c(0,0)),wdimg,"OM_ssb",c(14,6))
savepng(catchplot(OMfits,ci=FALSE,linesize=1,legendnames=legsus)+scale_y_continuous(limits=c(0,1.2e5),expand=c(0,0))+ylab('Catch'),wdimg,"OM_catch",c(14,6))
savepng(recplot(OMfits,ci=FALSE,linesize=1,legendnames=legsus),wdimg,"OM_rec",c(14,6))

df <- catchtable(OMfits)
df$min <- dfc$min
df$max <- dfc$max
levels(df$fit) <- legsus
p <- ggplot(df,aes(x=year,group=fit))+
    geom_line(aes(col=fit,y=Estimate),size=1)+
    labs(y='Catch (t)',x='Year',col='',fill='')+
    geom_ribbon(aes(ymin=min,ymax=max,fill=factor(fit)),alpha=0.4)+
    scale_color_viridis(discrete = TRUE)+
    scale_fill_viridis(discrete = TRUE)+
    facet_wrap(~fit)+
    geom_line(data=data.frame(y=ct[,1],year=dat$years),aes(y=y,x=year,group=NULL),size=1)+
    theme(legend.position ='none')+scale_x_continuous(expand = c(0,0))
savepng(p,wdimg,"OM_catch_facet",c(16,12))

s <- 
r <- rectable(OMfits)

prod <- merge(ssbtable(OMfits)[,c(1,4:5)],rectable(OMfits)[,c(1,4:5)],by=c('fit','year'))
prod$prod <- prod$Estimate.y/prod$Estimate.x
ggplot(prod,aes(x=year,y=prod,col=fit))+geom_line(size=0.5)+scale_color_viridis_d()

#*****************************************************************************
#************* define Harvest Control Rules **********************************
#*****************************************************************************

# --------------------- base MPs ----------------------------------------------

nMP=9

MP1 <- list(MPlabel='MP1',
            capLower=0,
            TAC.base=10000)

copy(x=MP1,n=nMP,name=c('MP'))

avail('MP')
MP1$catchval <- rep(0,ny)
MP2$MP <- rep('MPeggcomplex0',ny)
MP3$MP <- rep('MPeggcomplex2000',ny)
MP4$MP <- rep('MPeggcomplex4000',ny)
MP5$MP <- rep('MPeggcomplex6000',ny)
MP6$MP <- rep('MPeggcomplex8000',ny)
MP7$MP <- rep('MPeggcomplex10000',ny)
MP8$MP <- rep('MPeggcomplex',ny)
MP9$MP <- rep('MPeggcomplexramp',ny)

# --------------------- OM list ------------------------------------------
MP.list=list(MP1=MP1,
             MP2=MP2,
             MP3=MP3,
             MP4=MP4,
             MP5=MP5,
             MP6=MP6,
             MP7=MP7,
             MP8=MP8,
             MP9=MP9)

#******************************************************************************
#************* forecast for each combination **********************************
#******************************************************************************

ie1a <- c('IEconstant',paste0('IEindep',c(500,1000,2000,3000,4000)))
ie1b <- paste0('IEindep',c('0025','2550','5075','75100'))
ie2 <- seq(0,15000,by=1000)

scen <- expand.grid(OM=names(OM.list), MP=names(MP.list), IE=c(ie1a,ie2))
scen <- apply(scen,2,as.character)
scennames<- apply(scen,1,paste,collapse = ".")

# create a list with all scenarios to test (combos MP/OM/IE)
scen.list <- lapply(split(scen,1:nrow(scen)),function(x){
                OM <- OM.list[[x[1]]]
                MP <- MP.list[[x[2]]]
                OMnr <- suppressWarnings(as.numeric(x[3]))
                if(is.na(OMnr)) OM$IE[1] <- x[3] else OM$IE <- as.numeric(x[3])
                return(c(OM,MP))
})
names(scen.list) <- scennames
length(scen.list)

save(scen.list, file=paste0(wdRdata,'scen.list.Rdata'))
#load(file=paste0(wdRdata,'scen.list.Rdata'))

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

# run what has not been done yet
filenames <- dir(wdrun, pattern = ".Rdata")
files <- paste0(wdrun,filenames)
done <- gsub(pattern = ".Rdata",replacement = "",x = filenames)
sublist <- scen.list[-which(scennames %in% done)]    
    
multi.forecast(sublist,wdrun,parallel=TRUE,ncores=7)

# load all runs
filenames <- dir(wdrun, pattern = ".Rdata")
files <- paste0(wdrun,filenames)
runs <- lapply(files, function(x) {print(x);get(load(x))})
names(runs) <- gsub(wdrun,"",gsub(".Rdata","",filenames))
class(runs) <- 'forecastset'

sto <- subMSE(runs,IE = sub('IE','',ie1a))
det <- runs[-which(names(runs) %in% names(runs1))]
class(det) <- 'forecastset'
class(sto) <- 'forecastset'
save(sto, file=paste0(wdRdata,'runs.stochastic.Rdata'))
save(det, file=paste0(wdRdata,'runs.deterministic.Rdata'))
#load(file=paste0(wdRdata,'runs.stochastic.Rdata'))
#load(file=paste0(wdRdata,'runs.deterministic.Rdata'))

#******************************************************************************
#************* PLOTS **********************************************************
#******************************************************************************

threshold <-  0.75

#------------------------------------------------------------------------------
# --------------------- WITH 2 IE ---------------------------------------------
#------------------------------------------------------------------------------

#### diamond plot with year out of CZ/HZ on y axis 
p <- diamondplot(sto,what='probCZ',ylab='Years until SSB > LRP (75%)',xlab='Harvest Control Rule',
            year='threshold',threshold=0.75,IE='IEindep',IEnames=legscan,legnames=legsus,hline=10)+ 
    scale_color_viridis(discrete = TRUE,direction = -1)+
    scale_shape_manual(values=c(17,16,18,15))
savepng(p,wdimg,'CZ_diamond',c(18,9))
pdf("img/paper/pdf/Fig3.pdf", width=17/2.54, height=9/2.54)
p
dev.off()

p <- diamondplot(sto,what='probHZ',ylab='Years until SSB > URP (75%)',xlab='Harvest Control Rule',
            year='threshold',threshold=0.75,IE='IEindep',IEnames=legscan,legnames=legsus,hline=20)+ 
    scale_color_viridis(discrete = TRUE,direction = -1)+
    scale_shape_manual(values=c(17,16,18,15))+
    scale_y_continuous(breaks=c(15,20,25))
savepng(p,wdimg,'HZ_diamond',c(18,9))
pdf("img/paper/pdf/Fig5.pdf", width=17/2.54, height=9/2.54)
p
dev.off()

#------------------------------------------------------------------------------
# --------------------- WITH deterministic IE ---------------------------------
#------------------------------------------------------------------------------

### lineplot prob ~ missing catch: CZ
what <- 'probCZ'
df <- extract(det,what,add=TRUE)
colnames(df)[1]='y'
df$IE <- as.numeric(df$IE)

df$type <- as.factor(gsub("OM","",df$OM))
levels(df$type) <- legsus
df$MP <- gsub('MP','HCR',df$MP)

colnames(df)[which(colnames(df)=='year')] <- 'x'
df <- df[df$x!=min(df$x),] #remove the first year because not part of the future
m <- min(as.numeric(df$x))-1
df <- ddply(df,c('OM','MP','IE','id','type'),summarise,y=ifelse(any(y>=threshold),min(x[which(y>=threshold)]),max(x)))
df$y <- df$y-m

df1 <- df
p1 <- ggplot(df1,aes(x=IE,y=y,col=type,shape=type,group=type))+
    geom_line(size=1)+
    labs(col='OM',shape='OM')+
    ylab('Years until SSB > LRP (75%)')+xlab('Missing catch (t)')+
    facet_wrap(~MP)+
    scale_color_viridis(discrete = TRUE,direction = -1)+
    scale_shape_manual(values=c(17,16,18,15))+
    scale_y_continuous(expand=c(0,0),limits=c(0,25))+scale_x_continuous(expand = c(0,0))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_hline(yintercept=10,linetype='dashed',col='darkgreen')


savepng(p1,wdimg,'CZ_det',c(17,11))
pdf("img/paper/pdf/Fig4.pdf", width=17/2.54, height=11/2.54)
p1
dev.off()

### lineplot prob ~ missing catch: HZ
what <- 'probHZ'
df <- extract(det,what,add=TRUE)
colnames(df)[1]='y'
df$IE <- as.numeric(df$IE)

df$type <- as.factor(gsub("OM","",df$OM))
levels(df$type) <-  legsus
df$MP <- as.factor(df$MP)
levels(df$MP) <- paste0('HCR',1:9)

colnames(df)[which(colnames(df)=='year')] <- 'x'
df <- df[df$x!=min(df$x),] #remove the first year because not part of the future
m <- min(as.numeric(df$x))-1
df$x <- as.numeric(df$x)
df <- ddply(df,c('OM','MP','IE','id','type'),summarise,y=ifelse(any(y>=threshold),min(x[which(y>=threshold)]),max(x)))
df$y <- df$y-m

df2 <- df
p2 <- ggplot(df2,aes(x=IE,y=y,col=type,shape=type,group=type))+
    geom_line(size=1)+
    labs(col='OM',shape='OM')+
    ylab('Years until SSB > URP (75%)')+xlab('Missing catch (t)')+
    facet_wrap(~MP)+
    scale_color_viridis(discrete = TRUE,direction = -1)+
    scale_shape_manual(values=c(17,16,18,15))+
    scale_y_continuous(expand=c(0,0),limits=c(0,25))+scale_x_continuous(expand = c(0,0))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_hline(yintercept=20,linetype='dashed',col='darkgreen')

savepng(p2,wdimg,'HZ_det',c(17,11))
pdf("img/paper/pdf/Fig6.pdf", width=17/2.54, height=11/2.54)
p2
dev.off()


#------------------------------------------------------------------------------
# --------------------- Missing US catch examples -----------------------------
#------------------------------------------------------------------------------

submis <- subMSE(sto,MP=1,OM='2',IE='indep3000')[[1]]
ie <- attr(submis,'IE')[[2]][21:30,]
dimnames(ie) <- list(n=1:10,year=2019:(2018+25))
pie <- ggplot(xm, aes(x = year, y = value, col = n, group = n)) + 
    geom_line(lwd = 1) + 
    labs(y='Missing catch: US (t)',x="Year",col = "") + 
    scale_color_viridis()+
    theme(legend.position = 'none')+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))

savepng(pie,wdimg,"IE_us",dim=c(11,8))

#### numbers in manuscript
#min rebuilding time LRP
min(dia$y)
rebuild <- ddply(dia[dia$MP==1,],c('OM','MP','IE'),summarise,min(y))
dcast(rebuild,MP+OM~IE)
df1[df1$OM=='OM4' &df1$MP=='HCR1',]
df1[df1$OM=='OM1' &df1$MP=='HCR1',]
df2[df1$OM=='OM1' &df1$MP=='HCR2',]
