#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** based on CCAM package
#
# Intermediate steps are saved as Rdata, so either script can be run from scratch or from different points
# - base model definition and fitting
# - define core and stress OMs
# - define HCRs
# - create one list with all scenarios (OMs * HCRs)
# - do forecasting
# - load all forecast into one list
# - plotting in separate scripts
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
mo <- smoothmatrix(mo,subset=8:nrow(mo),max=1,plot=TRUE)
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

# define catch limits
ctwusa <- ct
ctwusa[,1] <- ct[,1]*1.10 + ctUSA[-c(1:8),1]*0.25
ctwusa[,2] <- ct[,2] + ctUSA[-c(1:8),1]*0.50

dat <- setup.ccam.data(surveys=tep,
                       residual.fleet=cn,
                       total.catch=ctwusa,
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

save(dat,file='Rdata/input/dat.Rdata')
save(conf,file='Rdata/input/conf.Rdata')
save(par,file='Rdata/input/par.Rdata')

# plots
# source('Rscripts/plot_data.R')

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

fitBase <- ccam.fit(dat,conf,par,silent=FALSE,paracheck = FALSE)            
fitBase

save(fitBase, file='Rdata/OMs/fitBase.Rdata')
    #load(file='Rdata/OMs/fitBase.Rdata')

#################################################################################################################
########### PLOTS & TABLES ######################################################################################
#################################################################################################################

# plots
# x <- fitBase
# name <- 'fitBase'
# source('Rscripts/plot_fit.R')

#################################################################################################################
########### MSE #################################################################################################
#################################################################################################################

#***************************************************************************
#************* define Operating Models *************************************
#***************************************************************************
ny=11
nosim=2000

#--------------------- base model ------------------------------------------
OMbase <- list(fit=fitBase,
               nosim=nosim,
               OMlabel='OMbase',
               IE=c('IEindep2019','IEdep2550'),
               year.base=2018,
               ave.years=tail(fitBase$data$years,25),
               rec.years=1969:2018,
               rec.meth=1, # BH with AC
               UL.years=tail(fitBase$data$years,25),
               deadzone=1000,
               Flim=2.5,
               fleet=3)

copy(x=OMbase,n=c(4,3),name=c('OMcore','OMstress'))

# --------------------- recruitment ------------------------------------------
OMstress1$rec.meth=2 # around average
OMcore1$rec.meth=2 #around average, delayed
attr(OMcore1$rec.meth,'AC')=0.9

# ---------------------  M ----------------------------------------------------
newdat1 <- dat
newdat1$natMor[,] <- 0.15

#fitM <- ccam.fit(newdat1,conf,par)     # run phase 1 + censored
#fitM
#save(fitM, file='Rdata/OMs/fitM.Rdata')
    load(file='Rdata/OMs/fitM.Rdata')

OMcore2$fit=fitM
OMcore3$bio.scale=list('nm'=1.2) #because 0.8 would not be a stress..

# --------------------- Upper limit with 25% extra USA------------------------------------------
# newdat2 <- dat
# oldUpper <- newdat2$logobs[which(!is.na(newdat2$logobs[,2])),]
# newUpper <- log(sweep(exp(oldUpper),1,0.25*ctUSA[-c(1:8),1],'+'))
# prettymatplot(sweep(exp(newUpper),1,exp(newUpper)[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
# newdat2$logobs[which(!is.na(newdat2$logobs[,2])),] <- newUpper
# 
# fitC1 <- ccam.fit(newdat2,conf,par)     # run phase 1 + censored
# fitC1
# save(fitC1, file='Rdata/OMs/fitC1.Rdata')
    load(file='Rdata/OMs/fitC1.Rdata')

OMcore4$fit=fitC1
OMcore4$IE <- c('IEindep2019','IEdep5075')

# --------------------- Upper limit with 25% less USA------------------------------------------
# newdat3 <- dat
# 
# ctwusa2 <- ct*1.10
# ctwusa2[,2] <- ct[,2] + ctUSA[-c(1:8),1]*0.25
# prettymatplot(sweep(ctwusa2,1,ctwusa2[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
# 
# newdat3$logobs[which(!is.na(newdat3$logobs[,2])),] <- log(ctwusa2)
# 
# fitC2 <- ccam.fit(newdat3,conf,par)     # run phase 1 + censored
# fitC2
# 
# save(fitC2, file='Rdata/OMs/fitC2.Rdata')
load(file='Rdata/OMs/fitC2.Rdata')

OMstress2$fit=fitC2
OMstress2$IE <- c('IEindep2019','IEdep0025')

# --------------------- USA uses Canadian TAC------------------------------------------
OMstress3$IE <- c('IEindep2019','IEdepcopy')

# --------------------- OM plots ------------------------------------------

# plots
# source('Rscripts/plot_OMs.R')

#*****************************************************************************
#************* define Harvest Control Rules **********************************
#*****************************************************************************

# --------------------- base MPs ----------------------------------------------

nMP=11

MP1 <- list(MPlabel='MP1',
            capLower=0,
            TAC.base=10000)

copy(x=MP1,n=nMP,name=c('MP'))

avail('MP')
MP1$catchval <- rep(0,ny)
MP2$catchval <- rep(0,ny)
MP3$MP <- rep('MPeggsimple',ny)
MP4$MP <- rep('MPeggcomplex0',ny)
MP5$MP <- rep('MPeggcomplex',ny)
MP6$MP <- rep('MPeggcomplexramp',ny)
MP7$MP <- rep('MPeggcomplex2000',ny)
MP8$MP <- rep('MPeggcomplex4000',ny)
MP9$MP <- rep('MPeggcomplex6000',ny)
MP10$MP <- rep('MPeggcomplex8000',ny)
MP11$MP <- rep('MPeggcomplex10000',ny)

#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************
OMs <- c('OMbase','OMcore1','OMcore2','OMcore3','OMcore4',
         'OMstress1','OMstress2','OMstress3')
MPs <-paste0('MP',1:nMP)

scenmat <- expand.grid(OM=OMs,MP=MPs)

scenlist <- lapply(split(scenmat,1:nrow(scenmat)),function(x){
    OMx <- get(as.character(x[1,1]))
    MPx <- get(as.character(x[1,2]))
    if(x[1,2]=='MP1'){                    # MP1 only has one IE (nothing)
        OMx$IE <- c('IEconstant')
    }
    c(OMx,MPx)
})
scennames <- apply(scenmat,1,paste,collapse = ".")
names(scenlist) <- scennames

save(scenlist, file='Rdata/scenlist.2019.04.09.Rdata')

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

load(file='Rdata/scenlist.2019.04.09.Rdata')

newdir <- FALSE

if(newdir) Date <- Sys.Date() else Date <- "2019-04-09"   # continue in old directory or set new one

DateDir <- paste0("Rdata/",Date,"/")
dir.create(DateDir,showWarnings = FALSE)


# select scenarios to run
sublist <- scenlist

            # # select specific things to run first
            # sublist <- subMSE(scenlist,OM=c('core4','stress2','stress3'))
            # 
            # # or run what has not been done yet
            # filenames <- dir(DateDir, pattern = ".Rdata")
            # files <- paste0(DateDir,'/',filenames)
            # done <- gsub(pattern = ".Rdata",replacement = "",x = filenames)
            # sublist <- scenlist[-which(scennames %in% done)]

length(sublist)

# run
multi.forecast(sublist,DateDir,parallel=TRUE,ncores=7)

#******************************************************************************
#************* Load all predictions *******************************************
#******************************************************************************

filenames <- dir(DateDir, pattern = ".Rdata")
files <- paste0(DateDir,'/',filenames)
runlist <- lapply(files, function(x) {print(x);get(load(x))})
names(runlist) <- gsub(pattern = ".Rdata",replacement = "",x = filenames)
class(runlist) <- 'forecastset'

save(runlist, file=paste0('Rdata/runlist.',Date,'.Rdata'))

#******************************************************************************
#************* PLOTS  *********************************************************
#******************************************************************************

## see following scripts (messy yet so no to be sourced entirely):
# Rscripts/plot_MSE   -> examples and minimum time to rebuilding
# Rscripts/plot_obj   -> plot of objectives and trafe-offs. Generates csv files with raw data
# Rscripts/plot_obj3  -> explores how to split catch
# Rscripts/plot_resdoc_... -> plots for resdoc to illustrate HCRs, IE, etc.

