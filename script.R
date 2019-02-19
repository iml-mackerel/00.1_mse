#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** based on CCAM package
#################################################################################################################

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
nm <- read.ices("data/nm.dat")
nm[]<- 0.27
pf <- read.ices("data/pf.dat")
pm <- read.ices("data/pm.dat")
sw <- read.ices("data/sw.dat")
sw0 <- read.ices("data/sw0.dat")
surveys <- read.ices("data/survey.dat")
surveys[[1]] <- surveys[[1]][!is.na(surveys[[1]]),1,drop=FALSE]
attr(surveys[[1]],'time') <- c(0.47)

# define catch limits
ct[,1] <- ct[,1]*1.10 + ctUSA[-c(1:8),1]*0.25
ct[,2] <- ct[,2] + ctUSA[-c(1:8),1]*0.50

dat <- setup.ccam.data(surveys=surveys,
                      residual.fleet=cn, # add argument called split.catch and see that residual.fleet does nothing if null
                      total.catch=ct,
                      prop.mature=mo,
                      stock.mean.weight=sw,
                      stock.start.weight=sw0,
                      catch.mean.weight=cw,
                      dis.mean.weight=dw,
                      land.mean.weight=lw,
                      prop.f=pf,
                      prop.m=pm,
                      natural.mortality=nm,
                      land.frac=lf)

conf <- defcon(dat)
conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
conf$keyVarObs[1,]=-1                     
conf$keyVarObs[2,1:9]=c(0,1,2,2,2,2,2,2,1) 
conf$keyVarObs[3,1]=3           
conf$stockRecruitmentModelCode=0 #0: RW, 1: ricker, 2: BH, 3:mean
conf$fbarRange=c(5,10)
conf$obsLikelihoodFlag[1]='CE'

par <- defpar(dat,conf)

save(dat,file='Rdata/input/dat.Rdata')
save(conf,file='Rdata/input/conf.Rdata')
save(par,file='Rdata/input/par.Rdata')

# plots
# source('Rscripts/plot_data.R')

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

fitBase <- ccam.fit(dat,conf,par)            
fitBase

save(fitBase, file='Rdata/OMs/fitBase.Rdata')
    #load(file='Rdata/OMs/fitBase.Rdata')

#################################################################################################################
########### PLOTS & TABLES ######################################################################################
#################################################################################################################

# plots
# x <- fit
# name <- 'fitBase'
# source('Rscripts/plot_fit.R')

#################################################################################################################
########### MSE #################################################################################################
#################################################################################################################

#***************************************************************************
#************* define Operating Models *************************************
#***************************************************************************
ny=25
nosim=500

#--------------------- base model ------------------------------------------
ny=25
nosim=300
OMbase <- list(fit=fitBase,
               nosim=nosim,
               OMlabel='OMbase',
               year.base=2016,
               ave.years=tail(fitBase$data$years,10),
               rec.years=1969:2016,
               rec.meth=4, # trailing sampling recruitment
               UL.years=tail(fitBase$data$years,10),
               deadzone=1000,
               Flim=2.5)

copy(x=OMbase,n=c(5,2),name=c('OMcore','OMstress'))

# --------------------- recruitment ------------------------------------------
OMcore1$rec.meth=2 # around average
attr(OMstress1$rec.meth,'AC')=0.9

# ---------------------  M ----------------------------------------------------
newdat1 <- dat
newdat1$natMor[,] <- 0.15

fitM <- ccam.fit(newdat1,conf,par)     # run phase 1 + censored

    save(fitM, file='Rdata/OMs/fitM.Rdata')
    #load(file='Rdata/OMs/fitM.Rdata')

OMcore2$fit=fitM
OMstress2$bio.scale=list('nm'=1.2) #because 0.8 would not be a stress..


# --------------------- Upper limit with 25% extra USA------------------------------------------
newdat2 <- dat
oldUpper <- newdat2$logobs[which(!is.na(newdat2$logobs[,2])),]
newUpper <- log(sweep(exp(oldUpper),1,0.25*ctUSA[-c(1:8),1],'+'))
prettymatplot(sweep(exp(newUpper),1,exp(newUpper)[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
newdat2$logobs[which(!is.na(newdat2$logobs[,2])),] <- newUpper

fitC <- ccam.fit(newdat2,conf,par)     # run phase 1 + censored

    save(fitC, file='Rdata/OMs/fitC.Rdata')
    #load(file='Rdata/OMs/fitC.Rdata')

OMcore3$fit=fitC

# --------------------- OM list ------------------------------------------

OM.list=list(OMbase=OMbase,
             OMcore1=OMcore1,
             OMcore2=OMcore2,
             OMcore3=OMcore3,
             OMstress1=OMstress1,
             OMstress2=OMstress2)


OMfits=c(fitBase=fitBase,FitM=fitM,fitC=fitC)
p1 <- ssbplot(OMfits,ci=FALSE)+scale_y_continuous(limits=c(0,5e5),expand=c(0,0))
p2 <- catchplot(OMfits,ci=FALSE)+scale_y_continuous(limits=c(0,1e5),expand=c(0,0))+ylab('Catch')
saveplot(p1,name="exploitUSA",dim=c(17,10),wd='img/fit_compare')
saveplot(p2,name="exploitUSA",dim=c(17,10),wd='img/fit_compare')

#*****************************************************************************
#************* define Harvest Control Rules **********************************
#*****************************************************************************

# --------------------- base MPs ----------------------------------------------

nMP=11

MP1 <- list(MPlabel='MP1',
            IE=NULL,
            capLower=0,
            TAC.base=10000)

copy(x=MP1,n=nMP,name=c('MP'))

avail('MP')
MP1$catchval <- rep(0,ny)
MP2$MP <- rep('MPeggsimple',ny)
MP3$MP <- rep('MPeggcomplex0',ny)
MP4$MP <- rep('MPeggcomplex',ny)
MP5$MP <- rep('MPeggcomplexramp',ny)
MP6$MP <- rep('MPeggcomplex2000',ny)
MP7$MP <- rep('MPeggcomplex4000',ny)
MP8$MP <- rep('MPeggcomplex6000',ny)
MP9$MP <- rep('MPeggcomplex8000',ny)
MP10$MP <- rep('MPeggcomplex10000',ny)
MP11$MP <- rep('MPeggcomplex15000',ny)

# --------------------- MP list (include different IEs) ------------------------------------------

## with 25%-50% USA
nIE=5

MPmat1=expand.grid(MP=paste0('MP',1:nMP), IE=c("IEindep4800", "IEindep6000", "IEindep7200", 
                   "IEindepdecr", "IEindepdecrsteep", "IEnothing"))
MP.list1 <- lapply(split(MPmat1,1:nrow(MPmat1)),function(x){
    MPx <- get(as.character(x[1,1]))
    if(!is.na(x[1,2])) MPx$IE <- c(as.character(x[1,2]),'IEdep2550')
    return(MPx)
})
names(MP.list1) <- paste(as.character(MPmat1[,1]),as.character(MPmat1[,2]),'IEdep2550',sep=".")

## with 50-75% USA
MPmat2=expand.grid(MP=paste0('MP',1:nMP), IE=c("IEindep4800", "IEindep6000", "IEindep7200", 
                                               "IEindepdecr", "IEindepdecrsteep", "IEnothing"))
MP.list2 <- lapply(split(MPmat2,1:nrow(MPmat2)),function(x){
    MPx <- get(as.character(x[1,1]))
    if(!is.na(x[1,2])) MPx$IE <- c(as.character(x[1,2]),'IEdep5075')
    return(MPx)
})
names(MP.list2) <- paste(as.character(MPmat2[,1]),as.character(MPmat2[,2]),'IEdep5075',sep=".")

#******************************************************************************
#************* forecast for each combination **********************************
#******************************************************************************
scenmat <- expand.grid(OM=names(OM.list), MP=names(MP.list1))
scenmat[,1] <- as.character(scenmat[,1])
scenmat[,2] <- as.character(scenmat[,2])
scenmat[scenmat[,1]=='OMcore3',2] <- names(MP.list2)
scennames <- apply(scenmat,1,paste,collapse = ".")


# create a list with all scenarios to test (combos MP/OM)
scen.list <- lapply(split(scenmat,1:nrow(scenmat)),function(x){
    if(x[1,1]=='OMcore3'){
        c(OM.list[[as.character(x[1,1])]],MP.list2[[as.character(x[1,2])]])
    }else{
        c(OM.list[[as.character(x[1,1])]],MP.list1[[as.character(x[1,2])]])
    }
    
})
names(scen.list) <- scennames

length(scen.list)

# forecast each scenario (combos MP/OM)
Date = Sys.Date()

DateDir = paste0("Rdata",Date,"/")
dir.create(DateDir,showWarnings = FALSE)

    save(scen.list, file='Rdata/scen.list.2018.11.29.Rdata')
    save(scen.list, file='Rdata/scen.list.F0.Rdata')
    #load(file='Rdata/scen.list.2018.08.24.Rdata')

# 48 is missing
    
toMatch <- paste0("MP",c(14),".IEindep6000")
matches <- unique (grep(paste(toMatch,collapse="|"), 
                            names(scen.list), value=TRUE))
mylist <- scen.list[matches]

mylist <- scen.list
length(mylist)

empty <- lapply(1:length(mylist),function(x){
    RUN <- do.call(forecast, mylist[[x]])
    save(RUN,file=paste0(DateDir,names(mylist)[x],'.Rdata'))
})


