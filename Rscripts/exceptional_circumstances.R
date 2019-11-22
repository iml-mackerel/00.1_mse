#################################################################################################################
#*** Mackerel MSE
#*** Exceptional circumstances
#*** exploratory: plot predicted TEP observations
#################################################################################################################

#### LOAD: projections (future) and observed tep (past)
# future
load(file='Rdata/runlist_WG_April2019.Rdata')
msemp <- subMSE(runlist,MP=c(3:11))
tepfut <- raw(msemp,'index') # raw data
tepfut$year <- as.numeric(as.character(tepfut$year))+2017
tepfut <- tepfut[tepfut$year != 2018,]

#past
tep <- read.ices("data/tep.dat")
tep <-  tep[[1]][,1]*1000000000000
teppast <- data.frame(.id=NA,nsim=1,year=names(tep),statedim=1,value=tep,MP='MP4',OM='OMbase')
teppast$year <- as.numeric(as.character(teppast$year))

#### combine
tepexample <- rbind(teppast,tepfut[tepfut$MP=='MP4' &tepfut$OM=='OMbase',])

ggplot(tepexample[tepexample$nsim %in% 1:30,],aes(x=year,y=value,group=nsim))+geom_line(size=0.5)+
    facet_grid(MP~OM)

ind <- function(x,quan){
    simindex <- do.call('cbind',llply(x,function(x) x$index))
    q <- t(round(apply(simindex,2,quantile,quan),2))
    colnames(q) <- c('q1','median','q3') 
    q <- cbind(q,year=1:nrow(q)+2017)
}
dfindex <- ldply(runmp,ind,c(0.95,.5,0.05))
dfindex$OM <- sapply(strsplit(dfindex$.id,'[.]'),'[[',1)
dfindex$MP <- as.numeric(gsub('MP','',sapply(strsplit(dfindex$.id,'[.]'),'[[',2)))

ggplot(dfindex[dfindex$OM=='OMcore4' & dfindex$MP==4,],aes(x=year,y=median))
    