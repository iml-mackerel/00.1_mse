#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** based on CCAM package
#
# get stuff for the minister (2021)
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

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

load(file='Rdata/OMs/fitBase.Rdata')

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

OMnoIE <- list(fit=fitBase,
               nosim=nosim,
               OMlabel='OMbase',
               IE='IEconstant', # no IE!!! NOT the US and NO missing in Canada
               year.base=2018,
               ave.years=tail(fitBase$data$years,25),
               rec.years=1969:2018,
               rec.meth=1, # BH with AC
               UL.years=tail(fitBase$data$years,25),
               deadzone=1000,
               Flim=2.5,
               fleet=3)

#*****************************************************************************
#************* define Harvest Control Rules **********************************
#*****************************************************************************

# --------------------- new MPs ----------------------------------------------

nMP=1
ny=11

MPmoratorium <- list(MPlabel='MPmoratorium',
                     capLower=0,
                     TAC.base=10000,
                     catchval=c(rep(1,5),seq(2000,12000,by = 2000)))

MP4000 <- list(MPlabel='catch4000',
            capLower=0,
            TAC.base=10000,
            catchval=rep(4000,ny))

#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************
OMs <- c('OMbase','OMnoIE')
MPs <- c('MPmoratorium','MP4000')

scenmat <- expand.grid(OM=OMs,MP=MPs)
scenmat <- scenmat[c(2,3),]

scenlist <- lapply(split(scenmat,1:nrow(scenmat)),function(x){
    OMx <- get(as.character(x[1,1]))
    MPx <- get(as.character(x[1,2]))
    c(OMx,MPx)
})
scennames <- apply(scenmat,1,paste,collapse = ".")
names(scenlist) <- scennames

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

newdir <- TRUE

if(newdir) Date <- Sys.Date() else Date <- "2021-12-09"   # continue in old directory or set new one

DateDir <- paste0("Rdata/",Date,"/")
dir.create(DateDir,showWarnings = FALSE)

# multi.forecast(scenlist,DateDir,parallel=FALSE,ncores=7) # forecast function: onyl run when needed

#******************************************************************************
#************* Load all predictions *******************************************
#******************************************************************************

filenames <- dir(DateDir, pattern = ".Rdata")
files <- paste0(DateDir,'/',filenames)
runlist <- lapply(files, function(x) {print(x);get(load(x))})
names(runlist) <- gsub(pattern = ".Rdata",replacement = "",x = filenames)
class(runlist) <- 'forecastset'

#******************************************************************************
#************* PLOTS  *********************************************************
#******************************************************************************

# some basic project plots
p1 <- ssb0plot(runlist$OMbase.MP4000)
p2 <- catchplot(runlist$OMbase.MP4000,ci=FALSE)
saveplot(p1,name="ssb_4000",dim=c(15,10),wd='img/minister')
saveplot(p2,name="catch_4000",dim=c(15,10),wd='img/minister')

p1 <- ssb0plot(runlist$OMnoIE.MPmoratorium)
p2 <- catchplot(runlist$OMnoIE.MPmoratorium,ci=FALSE)
saveplot(p1,name="ssb_mora",dim=c(15,10),wd='img/minister')
saveplot(p2,name="catch_mora",dim=c(15,10),wd='img/minister')

# catch vs ssb
a <- as.data.frame(attr(runlist$OMnoIE.MPmoratorium,"tab"))
b <- as.data.frame(attr(runlist$OMbase.MP4000,"tab"))
a$fit <- 'moratorium'
b$fit <- 'as_usual'

all <- rbind(a,b)

ca <- catchtable(runlist)
ss <- ssbtable(runlist)
names(ca)[1:3] <- c('catch.total','catch.total.low','catch.total.high')
names(ss)[1:3] <- c('ssb','ssb.low','ssb.high')

all <- merge(ca,ss)

p <- ggplot(all[all$year %in% 2019:2029,],aes(x=catch.total,y=ssb,col=fit,group=fit))+
    geom_path(size=0.1)+
    geom_text(data=all[all$year %in% c(2019,2022,2024,2029),],aes(label=year),hjust=-0.1,vjust=1)+
    scale_color_manual(values=c('orange','darkred'))+
    theme(legend.position = 'none')+
    labs(y='SSB (t)', x='Total catch (t)')
saveplot(p2,name="trade-off",dim=c(15,10),wd='img/minister')
    
#--------------------------------------------------------------------------------
#### Objective 2	avoid decline ###############################################
#--------------------------------------------------------------------------------
OMshape <- c(17,rep(16,7))
OMcol <- c('black',brewer.pal(5,'Blues')[5:2],brewer.pal(4,'Reds')[4:2])
threshold <- 0.75
basey <- 2019

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
dfdecliney$MP <- c('4000','moratorium')

ggadd2 <- function(p){
    p+
        geom_rect(aes(ymin=0.5,ymax=1,xmin=-Inf,xmax=Inf),fill='lightgrey',col='lightgrey',alpha=0.5)+
        geom_point(size=1.5)+
        labs(x='HCR',col='OM',shape='OM')+
        scale_x_discrete(labels = as.character(unique(dfdecliney$MP)), breaks = unique(dfdecliney$MP))+
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
         wd='img/minister')

