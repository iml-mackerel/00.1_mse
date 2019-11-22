#################################################################################################################
#*** Mackerel MSE
#*** Implemenation Errors functions for paper
#################################################################################################################


##' IEindep500
##' @param x number of simulations
##' @param y number of timestep
##' @param seed set.seed
##' @rdname IEindep500
##' @details generates matrix nosim x nyears with implementation errors that will be added to the TAC00
##' @export
IEindep500 <- function(x,y,seed=NULL){
    IEmean=c(Reduce(function(v, x) .76*v , x=numeric(5),  init=2000, accumulate=TRUE)[-1],rep(500,y-5))[1:y]
    IEsd=IEmean/8
    if(!is.null(seed)) set.seed(seed)
    ret=mapply(function(mu,sigma){rnorm(mu,sigma,n=x)},mu=IEmean,sigma=IEsd)
    return(ret)
}
class(IEindep500) <- append(class(IEindep500),"IE")

##' IEindep1000
##' @param x number of simulations
##' @param y number of timestep
##' @param seed set.seed
##' @rdname IEindep1000
##' @details generates matrix nosim x nyears with implementation errors that will be added to the TAC00
##' @export
IEindep1000 <- function(x,y,seed=NULL){
    IEmean=c(Reduce(function(v, x) .81*v , x=numeric(5),  init=3000, accumulate=TRUE)[-1],rep(1000,y-5))[1:y]
    IEsd=IEmean/8
    if(!is.null(seed)) set.seed(seed)
    ret=mapply(function(mu,sigma){rnorm(mu,sigma,n=x)},mu=IEmean,sigma=IEsd)
    return(ret)
}
class(IEindep1000) <- append(class(IEindep1000),"IE")

##' IEindep2000
##' @param x number of simulations
##' @param y number of timestep
##' @param seed set.seed
##' @rdname IEindep2000
##' @details generates matrix nosim x nyears with implementation errors that will be added to the TAC00
##' @export
IEindep2000 <- function(x,y,seed=NULL){
    IEmean=c(Reduce(function(v, x) .87*v , x=numeric(5),  init=4100, accumulate=TRUE)[-1],rep(2000,y-2))[1:y]
    IEsd=IEmean/8
    if(!is.null(seed)) set.seed(seed)
    ret=mapply(function(mu,sigma){rnorm(mu,sigma,n=x)},mu=IEmean,sigma=IEsd)
    return(ret)
}
class(IEindep2000) <- append(class(IEindep2000),"IE")

##' IEindep3000
##' @param x number of simulations
##' @param y number of timestep
##' @param seed set.seed
##' @rdname IEindep3000
##' @details generates matrix nosim x nyears with implementation errors that will be added to the TAC00
##' @export
IEindep3000 <- function(x,y,seed=NULL){
    IEmean=c(Reduce(function(v, x) .83*v , x=numeric(3),  init=5500, accumulate=TRUE)[-1],rep(3000,y-3))[1:y]
    IEsd=IEmean/8
    if(!is.null(seed)) set.seed(seed)
    ret=mapply(function(mu,sigma){rnorm(mu,sigma,n=x)},mu=IEmean,sigma=IEsd)
    return(ret)
}
class(IEindep3000) <- append(class(IEindep3000),"IE")

##' IEindep4000
##' @param x number of simulations
##' @param y number of timestep
##' @param seed set.seed
##' @rdname IEindep4000
##' @details generates matrix nosim x nyears with implementation errors that will be added to the TAC00
##' @export
IEindep4000 <- function(x,y,seed=NULL){
    IEmean=c(Reduce(function(v, x) .93*v , x=numeric(3),  init=5000, accumulate=TRUE)[-1],rep(4000,y-3))[1:y]
    IEsd=IEmean/8
    if(!is.null(seed)) set.seed(seed)
    ret=mapply(function(mu,sigma){rnorm(mu,sigma,n=x)},mu=IEmean,sigma=IEsd)
    return(ret)
}
class(IEindep4000) <- append(class(IEindep4000),"IE")