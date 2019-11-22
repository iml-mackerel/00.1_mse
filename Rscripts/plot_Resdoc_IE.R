#################################################################################################################
#*** Mackerel MSE
#*** Implemenation Errors (Missing Canadian catch scenarios)
#################################################################################################################

# pre-assessment (2019)
ny <- 25
IEmeans <- list(IE1 = rep(6000,100),
             IE2 = rep(7200,100),
             IE3 = c(Reduce(function(v, x) .8*v , x=numeric(3),  init=6000, accumulate=TRUE)[-1],rep(3000,97)),
             IE4 = c(Reduce(function(v, x) .75*v , x=numeric(6),  init=6000, accumulate=TRUE)[-1],rep(1000,94)),
             IE5 = rep(6000*0.8,100),
             IE6 = rep(0,100)
)
IEmeans <- lapply(IEmeans,function(x) x[1:ny])
IEmeans <- lapply(IEmeans,function(x) c(6000,x))
IEsds <- lapply(IEmeans,'/',4)

p <- IEplot(IEmeans,IEsds)+ scale_x_continuous(limits = c(0,ny+1),expand = c(0,0))
saveplot(p,name="IE_can",dim=c(11,6),wd='img/resdoc')

# post-assessment (2019)
ny <- 10
IEmeans <- c(Reduce(function(v, x) .83*v , x=numeric(3),  init=5500, accumulate=TRUE)[-1],rep(3000,97))[1:ny]
IEsds <- IEmeans/8
df<- data.frame(Year=1:ny,mean=IEmeans, sd=IEsds)
p <- ggplot(df,aes(x=Year,y=mean))+geom_line(size=1.2)+
    geom_ribbon(aes(ymin = mean - sd*2, ymax = mean + sd*2), alpha = 0.2)+
    labs(y='Missing Catch: Canada (t)')+
    scale_x_continuous(expand=c(0,0),limits=c(0,ny),breaks = 1:ny,labels=1:ny)+
    scale_y_continuous(expand=c(0,0),limits=c(0,6000))

saveplot(p,name="IE_can_BASE",dim=c(11,6),wd='img/resdoc')


