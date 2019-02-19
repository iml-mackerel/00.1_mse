#################################################################################################################
#*** Mackerel MSE
#*** Plot MSE results
#################################################################################################################

#******************************************************************************
#************* Load all predictions *******************************************
#******************************************************************************

DateDir <- "Rdata/2019-02-19"
filenames <- dir(DateDir, pattern = ".Rdata")
files <- paste0(DateDir,filenames)
runlist <- lapply(files, function(x) {print(x);get(load(x))})
names(runlist)=gsub(pattern = ".Rdata",replacement = "",x = filenames)
class(runlist)='forecastset'

#******************************************************************************
#************* plot each forecast and compare**********************************
#******************************************************************************
myrunlist=runlist

for(i in 1:11){
    runMP=myrunlist[grep(paste0("MP",i,".I"),names(myrunlist))]#base
    class(runMP)='forecastset'
    savepng(foreplot(runMP,what.y='probCZ',by = c('OM','IE')),wdIMG,paste0('/MSE/HalifaxSept_probCZ_MP',i),c(30,20))
}


#subset for certain MPs/OMs
toMatch1 <- c("14")
toMatch1 <- c("MP5", "MP6", "MP7",'MP8','MP9')
toMatch2 <- c("OMbase")
toMatch3 <- c("IEnorm1")

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
matches <- unique (grep(paste(toMatch2,collapse="|"), 
                        matches, value=TRUE))
matches <- unique (grep(paste(toMatch3,collapse="|"), 
                        matches, value=TRUE))
myrunlist=runlist[-which(names(runlist) %in% matches)]
class(myrunlist)='forecastset'

runnofish=runlistfull[grep(c('OMbase.MP1.IEnothing'),names(runlistfull))][[1]] #base


#### with no fishing at all (runlist F0) ##############################################################################################################
dir <- paste0(wdRdata,'F0',"/")
filenames <- dir(dir, pattern = ".Rdata")
files <- paste0(dir,filenames)
runlist0 <- lapply(files, function(x) {print(x);get(load(x))})
names(runlist0)=gsub(pattern = ".Rdata",replacement = "",x = filenames)
class(runlist0)='forecastset'
myrun0=runlist0

legnam=c('OMbase','OMcore1','OMcore2','OMcore3','OMstress1','OMstress2','OMstress3')

savepng(ssbplot(myrun0,ci=FALSE,linesize=1,legendnames=legnam)+geom_vline(xintercept=2016,color='grey',linetype='dashed')+ggtitle('F0'),wdIMG,'/MSE/HalifaxSept/F0_SSB',c(20,10))
savepng(foreplot(myrun0,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = legnam),wdIMG,'/MSE/HalifaxSept/F0_probCZ',c(20,10))
df <- foreplot(myrun0,what.y='probCZ',data=TRUE)
rebuild <- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))

savepng(ssbplot(myrun0,ci=TRUE,linesize=1,legendnames=legnam)+geom_vline(xintercept=2016,color='grey',linetype='dashed')+ggtitle('F0'),wdIMG,'/MSE/HalifaxSept/F0_SSB_CI',c(20,10))

#### TAC0, un6000 and US as usual ##############################################################################################################
toMatch1 <- c("MP1.IEindep600")

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
myrun=runlist[which(names(runlist) %in% matches)]
class(myrun)='forecastset'

legnam=c('OMbase','OMcore1','OMcore2','OMcore3','OMstress1','OMstress2','OMstress3')

savepng(ssbplot(myrun,ci=FALSE,linesize=1,legendnames=legnam)+geom_vline(xintercept=2016,color='grey',linetype='dashed')+ggtitle('F0'),wdIMG,'/MSE/HalifaxSept/TAC0_undecCAN6000_SSB',c(20,10))
savepng(foreplot(myrun,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = legnam),wdIMG,'/MSE/HalifaxSept/TAC0_undecCAN6000_probCZ',c(20,10))
df <- foreplot(myrun,what.y='probCZ',data=TRUE)
rebuild <- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))


#### TAC0, uncan steep decrease, US as usual ##############################################################################################################
toMatch1 <- c("MP1.IEnothing")

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
myrun=runlist[which(names(runlist) %in% matches)]
class(myrun)='forecastset'

legnam=c('OMbase','OMcore1','OMcore2','OMcore3','OMstress1','OMstress2','OMstress3')

savepng(ssbplot(myrun,ci=FALSE,linesize=1,legendnames=legnam)+geom_vline(xintercept=2016,color='grey',linetype='dashed')+ggtitle('F0'),wdIMG,'/MSE/HalifaxSept/TAC0_undecCAN0_SSB',c(20,10))
savepng(foreplot(myrun,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = legnam),wdIMG,'/MSE/HalifaxSept/TAC0_undecCAN0_probCZ',c(20,10))
df <- foreplot(myrun,what.y='probCZ',data=TRUE)
rebuild <- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))

#### TAC0, uncan steep decrease, US as usual ##############################################################################################################
toMatch1 <- c("MP1.IEindepdecrsteep")

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
myrun=runlist[which(names(runlist) %in% matches)]
class(myrun)='forecastset'

legnam=c('OMbase','OMcore1','OMcore2','OMcore3','OMstress1','OMstress2','OMstress3')

savepng(ssbplot(myrun,ci=FALSE,linesize=1,legendnames=legnam)+geom_vline(xintercept=2016,color='grey',linetype='dashed')+ggtitle('F0'),wdIMG,'/MSE/HalifaxSept/TAC0_undecCANsteepdecr_SSB',c(20,10))
savepng(foreplot(myrun,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = legnam),wdIMG,'/MSE/HalifaxSept/TAC0_undecCANsteepdecr_probCZ',c(20,10))
df <- foreplot(myrun,what.y='probCZ',data=TRUE)
rebuild <- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))


#### constant TAC for first 5 years or so for some OM and IE 6000 ##############################################################################################################
toMatch1 <- c("MP3.IEindep6000","MP6.IEindep6000", "MP7.IEindep6000", "MP8.IEindep6000",'MP9.IEindep6000','MP10.IEindep6000','MP11.IEindep6000')
toMatch2 <- c("OMbase",'OMstress1')

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
matches <- unique (grep(paste(toMatch2,collapse="|"), 
                        matches, value=TRUE))

myrun=runlist[which(names(runlist) %in% matches)]
class(myrun)='forecastset'

legnam=c(10000,15000,0,2000,4000,6000,8000)
savepng(foreplot(myrun,what.y='ssb',ylab='SSB (t)',by=c('OM'),ci=FALSE,year=2016:2022,legendnames=legnam),wdIMG,'/MSE/HalifaxSept/TAC0-15000_undecCAN6000_SSB',c(20,10))
savepng(foreplot(myrun,what.y='catch',ylab='Catch (t)',by=c('OM'),ci=FALSE,year=2016:2022,legendnames=legnam),wdIMG,'/MSE/HalifaxSept/TAC0-15000_undecCAN6000_catch',c(20,10))
savepng(foreplot(myrun,what.y='TAC',ylab='TAC (t)',by=c('OM'),ci=FALSE,year=2016:2022,legendnames=legnam),wdIMG,'/MSE/HalifaxSept/TAC0-15000_undecCAN6000_TAC',c(20,10))

#### MP 2 and 5, some OM and IE 6000 ##############################################################################################################
toMatch1 <- c("MP2.IEindep6000","MP5.IEindep6000")
toMatch2 <- c("OMbase",'OMstress1')

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
matches <- unique (grep(paste(toMatch2,collapse="|"), 
                        matches, value=TRUE))

myrun=runlist[which(names(runlist) %in% matches)]
class(myrun)='forecastset'

legnam=c('MPeggsimple','MPeggtargetramp')
savepng(foreplot(myrun,what.y='ssb',ylab='SSB (t)',by=c('OM'),ci=FALSE,year=2016:2022,legendnames=legnam),wdIMG,'/MSE/HalifaxSept/MP2.MP5_undecCAN6000_SSB',c(20,6))
savepng(foreplot(myrun,what.y='catch',ylab='Catch (t)',by=c('OM'),ci=FALSE,year=2016:2022,legendnames=legnam),wdIMG,'/MSE/HalifaxSept/MP2.MP5_undecCAN6000_catch',c(20,6))
savepng(foreplot(myrun,what.y='TAC',ylab='TAC (t)',by=c('OM'),ci=FALSE,year=2016:2022,legendnames=legnam),wdIMG,'/MSE/HalifaxSept/MP2.MP5_undecCAN6000_TAC',c(20,6))

#### diamond plot with year out of CZ/HZ on y axis ##############################################################################################################
IEnam =c('4800','6000','7200','decreasing','steep decreasing','no Canadian undeclared')
savepng(diamondplot(runlist,what='probCZ',ylab='Years until SSB > LRP (75%)',year='threshold',threshold=0.75,IE=1,IEnames=IEnam,hline=5)+ 
            scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_shape_manual(values=c(17,16,20)),wdIMG,'/MSE/HalifaxSept/obj1a_CZ_diamond_nyears',c(25,14))
savepng(diamondplot(runlist,what='probHZ',ylab='Years until SSB > URP (75%)',year='threshold',threshold=0.75,IE=1,IEnames=IEnam,hline=10)+ 
            scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_shape_manual(values=c(17,16,20)),wdIMG,'/MSE/HalifaxSept/obj1b_HZ_diamond_nyears',c(25,14))

write.table(diamondplot(runlist,what='probCZ',year=2022,hline=0.75,IE=1,IEnames=IEnam,data=TRUE),
            'probCZ.2022.txt', dec = ".")
write.table(diamondplot(runlist,what='probCZ',year=2027,IE=1,IEnames=IEnam,data=TRUE),
            'probCZ.2027.txt', dec = ".")
write.table(diamondplot(runlist,what='probHZ',year=2027,hline=0.75,IE=1,IEnames=IEnam,data=TRUE),
            'probCZ.2027.txt', dec = ".")
write.table(diamondplot(runlist,what='probHZ',year=2037,IE=1,IEnames=IEnam,data=TRUE),
            'probCZ.2037.txt', dec = ".")
write.table(diamondplot(runlist,what='probCZ',year='threshold',threshold=0.75,hline=0.75,IE=1,IEnames=IEnam,data=TRUE),
            'probCZ.byYear.txt', dec = ".")
write.table(diamondplot(runlist,what='probHZ',year='threshold',threshold=0.75,IE=1,IEnames=IEnam,data=TRUE),
            'probHZ.byYear.txt', dec = ".")


#### diamond plot with year out of growth on y axis ##############################################################################################################
IEnam =c('4800','6000','7200','decrease','steep decrease','no CAN undeclared')
IE=1
savepng(diamondplot(runlist,what='probGrowth',ylab='Median annual growth',year='zone',IE=1,IEnames=IEnam)+ 
            scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_shape_manual(values=c(17,16,20)),wdIMG,'/MSE/HalifaxSept/obj2_growth_diamond_zones',c(25,10))

write.table(diamondplot(runlist,what='probGrowth',year='zone',IE=1,IEnames=IEnam,data=TRUE),
            'probGrowth.txt', dec = ".")


#### diamond plot with % of years the have high chance of above LRP (>75%) ##############################################################################################################
IEnam =c('4800','6000','7200','decrease','steep decrease','no CAN undeclared')
IE=1
savepng(diamondplot(runlist,what='probCZ',ylab='% of next 25 years with >75% chance SSB>LRP',year='yearperc',IE=1,IEnames=IEnam,threshold=0.75)+ 
            scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_shape_manual(values=c(17,16,20)),wdIMG,'/MSE/HalifaxSept/obj2_growth_diamond_yearperc',c(25,10))

write.table(diamondplot(runlist,what='probCZ',year='yearperc',threshold=0.75,IE=1,IEnames=IEnam,data=TRUE),
            'PercYearsAboveLRP.txt', dec = ".")

#### diamond plot with ratio SSB/LRP ##############################################################################################################
IEnam =c('4800','6000','7200','decrease','steep decrease','no CAN undeclared')
IE=1

savepng(diamondplot(runlist,what='ssb',year=2016+5,IE=1,IEnames=IEnam,ylab='SSB/LRP',ratio=TRUE)+ 
            scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_shape_manual(values=c(17,16,20)),wdIMG,'/MSE/HalifaxSept/obj1a_CZ_diamond_SSB.LRP_median_5y',c(25,10))

savepng(diamondplot(runlist,what='ssb',year=2016+10,IE=1,IEnames=IEnam,ylab='SSB/LRP',ratio=TRUE)+ 
            scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_shape_manual(values=c(17,16,20)),wdIMG,'/MSE/HalifaxSept/obj1a_CZ_diamond_SSB.LRP_median_10y',c(25,10))


write.table(diamondplot(runlist,what='ssb',year=2016+5,IE=1,IEnames=IEnam,data=TRUE,ratio=TRUE),
            'SSB.LRP_5y.txt', dec = ".")
write.table(diamondplot(runlist,what='ssb',year=2016+10,IE=1,IEnames=IEnam,data=TRUE,ratio=TRUE),
            'SSB.LRP_10y.txt', dec = ".")

toMatch1 <- c("IEindep6000",".IEindepdecrsteep")
toMatch2 <- c("OMbase",'OMstress1')

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
matches <- unique (grep(paste(toMatch2,collapse="|"), 
                        matches, value=TRUE))

myrun=runlist[which(names(runlist) %in% matches)]
class(myrun)='forecastset'

legnam=c('6000','steep decrease')
savepng(foreplot(myrun,what.y='ssb',ylab='SSB/LRP',by=c('MP','OM'),ci=FALSE,year=2016:2026,ratio=TRUE,legendnames = legnam)+
            geom_hline(yintercept = 1,linetype='dashed'),wdIMG,'/MSE/HalifaxSept/obj1a_CZ_trajectories_SSB.LRP_median_10y',c(40,6))

write.table(foreplot(myrun,what.y='ssb',ylab='SSB/LRP',by=c('MP','OM'),ci=FALSE,year=2016:2026,ratio=TRUE,legendnames = legnam,data=TRUE),
            'SSB.LRP_trajectories.txt', dec = ".")



#### Example of US undeclared catch ##############################################################################################################
un <- attr(myrunlist[[1]],'IE')[[2]]
dimnames(un)[[2]]<- 2017:(2016+25)
savepng(boxplot.matrix(un,ylab='Undeclared catch (t)'),wdIMG,"/HCR/IE_USAbox",c(15,11))
savepng(prettymatplot(t(un[20:29,]),ylab = 'Undeclared catch (t)')+theme(legend.position='none'),wdIMG,"/HCR/IE_USAline",c(11,6))


#### extionction ##############################################################################################################
toMatch1 <- c('IEindepdecrsteep.IEdep2550')
toMatch2 <- c("OMbase")

matches <- unique (grep(paste(toMatch1,collapse="|"), 
                        names(runlist), value=TRUE))
matches <- unique (grep(paste(toMatch2,collapse="|"), 
                        matches, value=TRUE))

myrun=runlist[which(names(runlist) %in% matches)]
class(myrun)='forecastset'
length(myrun)

foreplot(myrun,what.y='probExtinct',ylab='Extinction risk',ci=TRUE,by='MP')

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

### all together
savepng(ssbplot(myrunlist,ci=FALSE)+scale_y_continuous(limits=c(0,800000)),wdIMG,'/MSE/SSB_TAC8000',c(25,15))
savepng(fbarplot(myrunlist,ci=FALSE),wdIMG,'/MSE/Fbar_TAC8000',c(25,15))
savepng(foreplot(myrunlist,what.y='probCZ',rect=0.75),wdIMG,'/MSE/CZ_TAC8000',c(17,10))
savepng(recplot(myrunlist,ci=FALSE)+scale_y_continuous(limits=c(0,1000000)),wdIMG,'/MSE/rec_TAC8000',c(25,15))
savepng(foreplot(myrunlist,what.y='probHZ',rect=0.75),wdIMG,'/MSE/HZ_TAC8000',c(17,10))
savepng(foreplot(myrunlist,what.y='ssbmsyratio',ylab='ssb/ssbmsy',hline=1),wdIMG,'/MSE/SSBmsy_TAC8000',c(17,10))
savepng(foreplot(myrunlist,what.y='fmsyratio',ylab='F/Fmsy',hline=1),wdIMG,'/MSE/Fmsy_TAC8000',c(17,10))
savepng(foreplot(myrunlist,what.y='Umsyratio',ylab='U/Umsy',hline=1),wdIMG,'/MSE/Umsy_TAC8000',c(17,10))


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


### Objective 1: rebuild out of critical zone  with 75% prob
IEnam =c('4800','6000','7200','decreasing','steep decreasing','no Canadian undeclared')
savepng(foreplot(runlist,what.y='probCZ',ylab='Probability out of the CZ',by=c('OM','MP'),vline=c(5,10)+2016,rect=0.75),wdIMG,'/MSE/HalifaxSept/obj1a_CZ',c(40,25))
savepng(diamondplot(runlist,what='probCZ',ylab='Probability out of the CZ',year=2022,hline=0.75,IE=1,IEnames=IEnam)+ scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_y_continuous(limits=c(0,1),expand=c(0,0)),wdIMG,'/MSE/HalifaxSept/obj1a_CZ_diamond_2022',c(25,14))
savepng(diamondplot(runlist,what='probCZ',ylab='Probability out of the CZ',year=2027,hline=0.75,IE=1,IEnames=IEnam)+ scale_color_manual(values=c('orange','grey25','grey60'))+
            scale_y_continuous(limits=c(0,1),expand=c(0,0)),wdIMG,'/MSE/HalifaxSept/obj1a_CZ_diamond_2027',c(25,14))


mydf <- diamondplot(runlist,what='probCZ',year=2022,IE=1,IEnames=IEnam,data=TRUE)
names(mydf)[6]='LRP5'
mydf$LRP10 <- diamondplot(runlist,what='probCZ',year=2027,IE=1,IEnames=IEnam,data=TRUE)$y
write.table(mydf,'probLRP_5y.10y.txt', dec = ".")

mydf$diff=mydf$LRP10-mydf$LRP5
hist(mydf$diff,50)
abline(v=0,col='red')
ggplot(mydf,aes(x=diff))+geom_histogram()+facet_grid(OM~MP)+geom_vline(xintercept = 0,col='red')

### Objective 1b: rebuild into healthy zone with 75% prob
savepng(foreplot(runlist,what.y='probHZ',ylab='Probability in HZ',by=c('OM','MP'),vline=c(5,10)+2016,rect=0.75),wdIMG,'/MSE/HalifaxSept/obj1b_HZ',c(40,25))
savepng(diamondplot(runlist,what='probHZ',ylab='Probability into the HZ',year=2027,hline=0.75,IE=1,IEnames=IEnam)+ scale_color_manual(values=c('orange','grey25','grey60')),wdIMG,'/MSE/HalifaxSept/obj1b_HZ_diamond_2027',c(25,14))
savepng(diamondplot(runlist,what='probHZ',ylab='Probability into the HZ',year=2037,hline=0.75,IE=1,IEnames=IEnam)+ scale_color_manual(values=c('orange','grey25','grey60')),wdIMG,'/MSE/HalifaxSept/obj1b_HZ_diamond_2037',c(25,14))


## Objective 2: maintain a positive growth trajectory
savepng(foreplot(runlist,what.y='probgrowth',by=c('OM','MP'),IE=1,ylab='Probability of growth'),wdIMG,'/MSE/HalifaxSept/obj2_growth',c(40,25))
foreplot(runlist,what.y='probgrowth30',by='OM')

# stuff should be added here (Numberof years prob of growth below x%)


## Objective 3: Maximize annual catches
savepng(foreplot(runlist,what.y='catch',by=c('OM','MP'),IE=1,ci=FALSE),wdIMG,'/MSE/HalifaxSept/obj3_catch',c(40,25))
savepng(diamondplot(runlist,what='catch',ylab='Average catch',IE=1,IEnames=IEnam)+ scale_color_manual(values=c('orange','grey25','grey60')),wdIMG,'/MSE/HalifaxSept/obj3_Caver_diamond_ally',c(25,14))
savepng(diamondplot(runlist,what='catchcumul',ylab='Cumulative catch',IE=1,IEnames=IEnam,year=2016+25)+ scale_color_manual(values=c('orange','grey25','grey60')),wdIMG,'/MSE/HalifaxSept/obj3_TACaver_diamond_ally',c(25,14))

diamondplot(runlist,what='TAC',ylab='Average TAC',IE=1,IEnames=IEnam)+ scale_color_manual(values=c('orange','grey25','grey60'))

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


## all objectives
savepng(MSEplot(runlist),wdIMG,'/MSE/HalifaxSept/objectives_ALL',c(40,25))




