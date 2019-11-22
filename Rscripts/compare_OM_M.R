#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE Different M in operating models
#################################################################################################################

#################################################################################################################
########### PREPARE DATA ########################################################################################
#################################################################################################################
load(file='Rdata/input/dat.Rdata')
load(file='Rdata/input/conf.Rdata')
load(file='Rdata/input/par.Rdata')
load(file='Rdata/OMs/fitBase.Rdata')
par <- fitBase$obj$env$parList(fitBase$opt$par)

nm.alv <- read.ices("data/nm_Alverson.dat")
nm.zhang <- read.ices("data/nm_Zhang.dat")
nm.gisl <- read.ices("data/nm_Gislason.dat")
nm.gund <- read.ices("data/nm_Gunderson.dat")
nm.DFO2017 <- read.ices("data/nm_DFO2017.dat")

Mrange <- seq(0.15,0.3,0.01)

n <- length(Mrange)
ntot <- n +5

mydats <- replicate(ntot,dat,simplify=FALSE)
mydats[1:n] <- lapply(1:n,function(i){
    mydats[[i]]$natMor[] <- Mrange[i]
    mydats[[i]]
} )
mydats[[n+1]]$natMor[] <- nm.gund
mydats[[n+2]]$natMor[] <- nm.alv
mydats[[n+3]]$natMor[] <- nm.zhang
mydats[[n+4]]$natMor[] <- nm.gisl
mydats[[n+5]]$natMor[] <- nm.DFO2017

Mtypes <- c(Mrange,'Gunderson','Alverson','Zhang','Gislason','DFO2017')

#################################################################################################################
########### fit models ###########################################################################################
#################################################################################################################

# run
Mruns <- lapply(1:length(mydats),function(x){
    fit  <- ccam.fit(mydats[[x]],conf,par,silent=FALSE) 
    save(fit,file=paste0('Rdata/OMs/M/',Mtypes[x],'.Rdata'))
    })

# load
Mruns <- lapply(dir("Rdata/OMs/M"), function(x) {get(load(paste0('Rdata/OMs/M/',x)))})
names(Mruns) <- Mtypes
class(Mruns) <- 'ccamset'

#################################################################################################################
########### plot ###########################################################################################
#################################################################################################################

.wd <- 'img/fit_compare/M/'
dir.create(.wd, showWarnings = FALSE)
update_geom_defaults("line", list(size = 0.8))

savepng(ssbplot(Mruns,ci=FALSE),.wd,"SSB",c(14,7))
savepng(catchplot(Mruns,ci=FALSE),.wd,"catch",c(14,7))
savepng(recplot(Mruns,ci=FALSE,years=1969:2018),.wd,"recruitment",c(14,7))
savepng(fitplot(Mruns,type='AIC',n=FALSE),.wd,"AIC",c(12,6))
savepng(fitplot(Mruns,type='nll',n=FALSE),.wd,"nll",c(12,6))

df <- data.frame(M=Mtypes,LRP=unlist(lapply(ypr(Mruns),'[','f40ssb'))*0.4)
mLRP <- ggplot(df,aes(x=M,y=LRP))+geom_point()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
saveplot(mLRP,name='LRP',dim=c(8,6),wd=.wd)


