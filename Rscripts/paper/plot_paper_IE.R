#################################################################################################################
#*** Mackerel MSE
#*** Implemenation Errors plot (Missing Canadian catch scenarios)
#################################################################################################################

# for paper
ny <- 25
IEmeans <- list(IE0 = rep(0,ny),
             IE1 = c(Reduce(function(v, x) .83*v , x=numeric(3),  init=5500, accumulate=TRUE)[-1],rep(3000,97))[1:ny],
             IE2 = c(Reduce(function(v, x) .93*v , x=numeric(3),  init=5000, accumulate=TRUE)[-1],rep(4000,97))[1:ny],
             IE3 = c(Reduce(function(v, x) .87*v , x=numeric(5),  init=4100, accumulate=TRUE)[-1],rep(2000,97))[1:ny],
             IE4 = c(Reduce(function(v, x) .81*v , x=numeric(5),  init=3000, accumulate=TRUE)[-1],rep(1000,97))[1:ny],
             IE5 = c(Reduce(function(v, x) .76*v , x=numeric(5),  init=2000, accumulate=TRUE)[-1],rep(500,97))[1:ny]
)
IEsds <- lapply(IEmeans,'/',8)

myIE <- cbind(melt(do.call("rbind", IEmeans)), sd = melt(do.call("rbind", IEsds))[, 3])
txt <- data.frame(x=25,y=unlist(lapply(IEmeans,tail,1)),lab=c('0 t',paste0('decrease to ~',unlist(lapply(IEmeans,tail,1))[-1],' t')))
pie <- ggplot(myIE, aes(x = Var2+2018, y = value))+ 
    geom_ribbon(aes(ymin = value -  sd*2, ymax = value + sd*2, fill = Var1), alpha = 0.2)+ 
    geom_line(aes(col = Var1),size=1)+ 
    theme(legend.position = "none")+ 
    labs(y='Missing Catch: Canada (t)', x="Year")+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    geom_text(data=txt,aes(x=x+2018,y=y,label=lab),vjust=-0.4,hjust = 1.2,size=3)+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))

savepng(pie,wdimg,"IE_canada",c(11,8))
pdf("img/paper/pdf/Fig1.pdf", width=8.5/2.54, height=6/2.54)
pie
dev.off()


