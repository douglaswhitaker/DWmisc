

pmf.plot <- function(x, px, plot.type="line", main="Probability Mass Function", xlab="X",ylab="P(X)",...){
  if (plot.type == "line"){
    plot(x,px,type="h",lwd=3,col="red",ylim=c(0,max(px)*1.1),main=main,xlab=xlab,ylab=ylab,...)
    points(x,px,pch=19,col="red",cex=1.4,...)
    #abline(h=0,...)
    #abline(h=1,...)
  }
  else if (plot.type == "bar"){
    barplot(px,names.arg=x,main="Probability Mass Function",xlab="X",ylab="P(X)",ylim=c(0,max(px)*1.1),...)
  }
}


cdf.plot <- function(x,px,type="mass",...){
  x.cdf <- cumsum(px)
  if (type=="mass"){
    plot(0,0,type="n",xlim=c(min(x)-1,max(x)+1),ylim=c(-.1,1.1),main="Cumulative Distribution Function",xlab="X",ylab="F(X)")
    segments(x0=(min(x)-2),y0=0,x1=x[1],y1=0)
    points(x[1],0)
    for (i in 1:(length(x.cdf)-1)){
      segments(x0=x[i],x1=x[i+1],y0=x.cdf[i],y1=x.cdf[i])
      points(x[i],x.cdf[i],pch=19)
      points(x[i+1],x.cdf[i])
    }
    segments(x0=x[length(x)],y0=1,x1=(max(x)+2),y1=1)
    points(x[length(x)],1,pch=19)
  }
  else if (type=="cdf"){
    cdf.plot(x=x,px=diff(px),type="mass",...)
  }
  else print("Not yet implemented.")
}