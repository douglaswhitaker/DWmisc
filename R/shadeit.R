# There is currently a bug where arguments that are non-numeric passed to distf are not evaluated properly

# > shadeit(dnorm,lb=(3-3*.238),ub=(3+3*.238),lbs=-Inf,ubs=3.2,mean=3,sd=((3.2-3)/qnorm(.8)))
# Error in plot.window(...) : need finite 'ylim' values
# In addition: Warning messages:
#   1: In shadeit(dnorm, lb = (3 - 3 * 0.238), ub = (3 + 3 * 0.238), lbs = -Inf,  :
#                   NAs introduced by coercion
#                 2: In min(x) : no non-missing arguments to min; returning Inf
#                 3: In max(x) :
#  Error in plot.window(...) : need finite 'ylim' values

# But the same call with a value substituted for sd works:
# > shadeit(dnorm,lb=(3-3*.238),ub=(3+3*.238),lbs=-Inf,ubs=3.2,mean=3,sd=.2376)
# [1] 0.8

shadeit <- function(distf, lb, ub, lwd=4, col="red", lbs=NULL, ubs=NULL, ...){
  args <- sapply(substitute(list(...))[-1], deparse)
  fargs <- names(formals(distf))
  args.to.pass <- list()
  for (i in 2:length(fargs)){ # we know we want to skip the first argument which we will be passing manually
    ind.match <- which(fargs[i] == names(args))
    if (length(ind.match) > 0){
      args.to.pass[[fargs[i]]] <- as.numeric(args[names(args)[ind.match]])
    }
  }
  arg.str <- c()
  for (i in 1:length(args.to.pass)){
    arg.str <- paste(arg.str,paste(names(args.to.pass)[i],args.to.pass[[i]],sep="="),sep=",")
  } 
  arg.str <- substring(arg.str,first=2)

  xs <- seq(from=lb,to=ub,by=0.01)
  ys <- eval(parse(text=paste(deparse(substitute(distf)),"(xs,",arg.str,")")))
  plot(xs, ys, type="l",lwd=lwd, xlab="x",ylab="f(x)")

  if (!is.null(lbs) & !is.null(ub)){
    xss <- seq(from=ifelse(is.infinite(lbs),lb,lbs),to=ifelse(is.infinite(ubs),ub,ubs),by=0.01)
    yss <- eval(parse(text=paste(deparse(substitute(distf)),"(xss,",arg.str,")")))
    polygon(x=c(lbs,xss,ubs),y=c(0,yss,0),col=col) # not yet on slide
    eval(parse(text=paste("area <- (integrate(",deparse(substitute(distf)),",l=lbs,u=ubs,",arg.str,"))")))
    print(round(area$value,4))
  }
}

