##' subset forecastset for certain MPs, OMs or IEs.
##' @param x object of class forecastset
##' @param MP MPs
##' @param OMs OMs
##' @param IEs IEs
##' @details subset list more easily
##' @rdname subMSE
##' @export
subMSE <- function(x, MP=NULL,OM=NULL,IE=NULL){
    n <- names(x)
    if(!is.null(MP)){
        MP <- paste0('MP',MP)
        n <- n[which(unlist(lapply(strsplit(n,'[.]'),function(x) any(MP %in% x))))]
    }
    if(!is.null(OM)){
        OM <- paste0('OM',OM)
        n <- n[which(unlist(lapply(strsplit(n,'[.]'),function(x) any(OM %in% x))))] 
    }
    if(!is.null(IE)){
        IE <- paste0('IE',IE)
        n <- n[which(unlist(lapply(strsplit(n,'[.]'),function(x) any(IE %in% x))))]
    }
    
    y <- x[which(names(x) %in% n)]
    if(class(x)=='forecastset') class(y) <- 'forecastset'
    return(y)
}



