#################################################################################################################
#*** Mackerel MSE
#*** Plot examples of IE
#################################################################################################################

load(file='Rdata/runlist_WG_April2019.Rdata')

#--------------------------------------------------------------------------------
#### Example of US missing catch ################################################
#--------------------------------------------------------------------------------

submis <- subMSE(runlist,MP=11,OM=c('base','core4','stress2','stress3'))
perc <- c('median','min','max','q3','q1')

ies <- do.call('rbind',lapply(1:length(submis),function(i) {ie <- attr(submis[[i]],'IE')
                do.call('rbind',lapply(1:length(ie),function(x){y=ie[[x]][1:15,]
                dimnames(y)=list(var=c(perc,1:10),year=1:ncol(y))
                y <- melt(y)
                y$ie <- names(ie)[[x]]
                y$OM <- gsub('.MP11','',names(submis)[[i]])
                y}))}))


IE_names <- c(IEdep0025="OMstress2 (US)\n0-25%",
                IEdep2550="OMbase (US)\n25-50%",
                IEdep5075="OMcore4 (US)\n50-75%",
                IEdepcopy="OMstress3 (US)\n25-50% - identical TAC",
                IEindep2019="OMbase (Canada)"
)
ies$ie = factor(ies$ie,unique(ies$ie))

pie <- ggplot(ies[ies$var %in% c(1:10),])+
    geom_ribbon(data=dcast(ies,year+ie+OM~var,value.var = 'value'),aes(x=year,ymin=min,ymax=max),fill='grey85')+
    geom_ribbon(data=dcast(ies,year+ie+OM~var,value.var = 'value'),aes(x=year,ymin=q1,ymax=q3),fill='grey65')+
    geom_line(aes(x=year,y=value,group=var),size=0.7)+
    facet_wrap(ie~.,labeller=as_labeller(IE_names))+
    theme(strip.background = element_blank(),axis.line.x = element_line(colour = 'black'))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+labs(y='Catch (t)',x='Year')

saveplot(pie,name="IE_examples",dim=c(18,12),wd='img/resdoc')

pie2 <- ggplot(ies[ies$var %in% c(1:10),])+
    geom_ribbon(data=dcast(ies,year+ie+OM~var,value.var = 'value'),aes(x=year,ymin=min,ymax=max),fill='grey85')+
    geom_ribbon(data=dcast(ies,year+ie+OM~var,value.var = 'value'),aes(x=year,ymin=q1,ymax=q3),fill='grey65')+
    geom_line(aes(x=year,y=value,group=var),size=0.7)+
    facet_wrap(ie~.,labeller=as_labeller(IE_names),ncol=1,scale='free_y')+
    theme(strip.background = element_blank(),axis.line.x = element_line(colour = 'black'))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+labs(y='Missing Catch (t)',x='Year')

saveplot(pie2,name="IE_examples_hori",dim=c(6,16),wd='img/resdoc')
