raw <-function (x,what){
    UseMethod("raw")
}

raw.ccamforecast <- function(x,what){
    if(missing(what)) stop('what should be specified')
    if(!what %in% c(names(x[[1]]),'IE'))  stop('what is not available')
    
    if(what!='IE'){
        names(x) <- 1:length(x)
        shape <- function(y, what){
            v <- y[[what]]
            if(is.matrix(v))
                r <- melt(v,varnames = c('nsim','statedim'))
            if(is.vector(v))
                r <- cbind(nsim=1:length(v),statedim = 1, value=v)
            return(r)
        }
        ret <- ldply(x,shape,what,.id='year')
    }else{
        ret <- attr(x,'IE')
        ret <- ldply(ret,function(y){
            y <- t(y[-c(1:5),])
            colnames(y) <- 1:ncol(y)
            rownames(y) <- 2:(nrow(y)+1)
            melt(y,varnames = c('year','nsim'))
        },.id='statedim')
        ret <- ret[,c('nsim', 'year', 'statedim', 'value')]
    }
    names(ret)[ncol(ret)] <- what
    ret$MP <- attr(x,'parameters')$MPlabel
    ret$OM <- attr(x,'parameters')$OMlabel
    return(ret)
}

raw.forecastset <- function(x,what){
    if(is.null(names(x))) names(x) <- 1:length(x)
    ret <- ldply(x,raw,what)
    return(ret)
}
