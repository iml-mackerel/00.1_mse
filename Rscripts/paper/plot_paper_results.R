load(file=paste0(wdRdata,'runs.stochastic.Rdata'))
load(file=paste0(wdRdata,'runs.deterministic.Rdata'))
class(det) <- 'forecastset'
class(sto) <- 'forecastset'
threshold <-  0.75

cols <- viridis(4,direction = -1)
cols[1] <-'orange'
library(dplyr)

#------------------------------------------------------------------------------
# --------------------- PLOTS AFTER PUNT REVIEW -------------------------------
#------------------------------------------------------------------------------

#### stochastic new -----------------------------------------------------------
dia1 <- diamondplot(sto,what='probCZ',year='threshold',threshold=0.75,IE='IEindep',IEnames=legscan,legnames=legsus,data = T)
dia1[dia1$MP %in% 4:7,'MP'] = '4-7'
dia1$MP <- paste0('HCR',dia1$MP)
dia1$id <- NULL
dia1 <- distinct(dia1)

p1 <- ggplot(dia1,aes(x=factor(IE),y=y,col=type,shape=type))+
    geom_hline(yintercept=10,linetype='dashed',col='darkgrey')+
    geom_jitter(height=0,width=0.2)+
    facet_wrap(~MP)+
    scale_color_manual(values = cols)+
    scale_shape_manual(values=c(16,17,3,4))+
    theme(axis.text.x =element_blank(),
          legend.position='top',
          plot.title = element_text(vjust=-4,margin=margin(0,0,0,0)),
          legend.margin = margin(-2,0,0,0))+
    labs(x='',y='Years until SSB > LRP (75%)',title='A)',col='OM',shape='OM')

dia2 <- diamondplot(sto,what='probHZ',year='threshold',threshold=0.75,IE='IEindep',IEnames=legscan,legnames=legsus,data = T)
dia2[dia2$MP %in% 3:9,'MP'] = '3-9'
dia2$MP <- paste0('HCR',dia2$MP)
dia2$id <- NULL
dia2 <- distinct(dia2)

p2 <- ggplot(dia2,aes(x=factor(IE),y=y,col=OM,shape=OM))+
    geom_hline(yintercept=20,linetype='dashed',col='darkgrey')+
    geom_jitter(height=0,width=0.2)+
    facet_wrap(~MP)+
    scale_color_manual(values=cols)+
    scale_shape_manual(values=c(16,17,3,4))+
    theme(axis.text.x = element_text(angle=45,hjust=1),
          legend.position = 'none',
          plot.title = element_text(vjust=4,margin=margin(-5,0,0,0)))+
    labs(x='Missing catch: Canada (t)',y='Years until SSB > USR (75%)',title = 'B)')


pdf("img/paper/pdf/Fig3new.pdf", width=17/2.54, height=17/2.54)
grid.arrange(p1,p2,heights=c(0.6,0.44))
dev.off()

### deterministic new -----------------------------------------------------------
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
p1b <- ggplot(df1[df1$MP %in% paste0('HCR',c(1:4,8,9)),],aes(x=IE,y=y,col=type,group=type))+
    geom_hline(yintercept=10,linetype='dashed',col='darkgrey')+
    geom_line(size=1)+
    labs(col='OM',shape='OM',title='A)')+
    ylab('Years until SSB > LRP (75%)')+xlab('')+
    facet_wrap(~MP)+
    scale_color_manual(values=cols)+
    scale_y_continuous(expand=c(0,0),limits=c(0,26))+
    scale_x_continuous(expand = c(0,0))+
    theme(axis.text.x =element_blank(),
          legend.position='top',
          plot.title = element_text(vjust=-4,margin=margin(0,0,0,0)),
          legend.margin = margin(-2,0,0,0))

    
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
p2b <- ggplot(df2[df2$MP %in% paste0('HCR',c(1:4,8,9)),],aes(x=IE,y=y,col=type,group=type))+
    geom_hline(yintercept=20,linetype='dashed',col='darkgrey')+
    geom_line(size=1)+
    labs(col='OM',shape='OM',y='Years until SSB > USR (75%)',x='Missing catch: Canada + US (t)',title='B)')+
    facet_wrap(~MP)+
    scale_color_manual(values=cols)+
    scale_y_continuous(expand=c(0,0),limits=c(0,26))+
    scale_x_continuous(expand = c(0,0))+
    theme(axis.text.x = element_text(angle=45,hjust=1),
          legend.position = 'none',
          plot.title = element_text(vjust=4,margin=margin(-5,0,0,0)))

pdf("img/paper/pdf/Fig4new.pdf", width=17/2.54, height=17/2.54)
grid.arrange(p1b,p2b,heights=c(0.6,0.6))
dev.off()



