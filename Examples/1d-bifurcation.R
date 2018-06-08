#get.fps is find.period but it returns a list of points instead of the amount of points
#currently its some insanely nested list. probably can be cleaned up
#make a 1d bif diagram for 1d mod




in.range = function(x,y, model, rangeMult=0){
  if(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult))
    finite.points(c(x,y))
  else
    all(x < rangeMult*model$range$xlim[[2]] & y < rangeMult*model$range$ylim[[2]])
}

dist.origin <- function(a) sqrt(sqdist(a,c(0,0)))

sqdist <- function(a, b) {
  return((a[[1]]-b[[1]])^2 + (a[[2]]-b[[2]])^2)
}

get.fps = function(self, x, y,
                       initIters=1000, maxPeriod=128, numTries=1,
                       tolerance=sqrt(.Machine$double.eps),epsilon=sqrt(tolerance), #should be using epsilon instead?
                       rangeMult=0){
  if(!(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult)) && is.null(self$range)){
    stop("is.stable with rangeMult!=0 requires range() to have been composed with the model.")
  }
  #moves all the points untill they are either all infinite, fixed, or outside of range*rangeMult
  for(i in 1:numTries) {
    startPoint <- self$apply(x,y,iters=initIters,accumulate=FALSE,crop=FALSE)
    if(!in.range(startPoint$x,startPoint$y,self,rangeMult)){
      #print("no period found, diverged")
      return(list(list(x=NA,y=NA)))
    }
    candidates=self$apply(startPoint[[1]], startPoint[[2]] ,iters=maxPeriod,accumulate=TRUE,crop=FALSE)
    period=FALSE
    i=1
    while(i<maxPeriod && !period){
      ithPoint=candidates[[i+1]]
      if(sqdist(startPoint, ithPoint) < tolerance)
        period=TRUE
      else
        i=i+1
    }
    if(period){
      return(candidates[1:i])
    }
    x=ithPoint$x
    y=ithPoint$y
  }
  warning(paste("Assuming divergance: no period found after",(initIters+maxPeriod)*numTries,"iterations. Consider increasing initIters."))
  return(list(list(x=NA,y=NA)))
}


# bif for    x'=s*x*(1-x)
logistic=FALSE
if(logistic){
fps=mapply(evalPoint,as)

amin=2 #1
amax=4
xmin=0
xmax=1

adisc=.01 #.05
xdisc=.01 #.05
as=seq(amin,amax,by=adisc)
xs=seq(xmin,xmax,by=xdisc)


g = function(s=.5){
  function(x,y){
    a=.5
    b=.5
    r=1
    #list(x=x*exp(r-x-a*y),
    #     y=y*exp(s-b*x-y))
    list(x=s*x*(1-x),y=0)

  }
}


evalPoint=function(a){
  m<-dsmodel(g(a),display = FALSE)
  period=get.fps(m,.5,.5,numTries = 3,maxPeriod = 1024)
  period
}



z=matrix(NA,length(as),length(xs))
#using for for now
for(i in 1:length(fps)){
  #for 2d systems, find dist origin
  #z[i,1/xdisc*mapply(dist.origin,fps[[i]])]=1
  #z[i,-1+1/xdisc*mapply(dist.origin,fps[[i]])]=1
  #z[i,1+1/xdisc*mapply(dist.origin,fps[[i]])]=1

  #for 1d models, just the xval of the fixed point.
  z[i,1+1/xdisc*mapply(function(a)a[[1]],fps[[i]])]=1
  z[i,1/xdisc*mapply(function(a)a[[1]],fps[[i]])]=1
  z[i,-1+1/xdisc*mapply(function(a)a[[1]],fps[[i]])]=1
}

image(as,xs, z)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#bifurcation for
#list(x*exp(r-x-a*y),
#     y*exp(s-b*x-y))
#varying r and s together

#library(dsmodels), library(latex2exp), source paramrange, simmap
competition=TRUE
if(competition){

gen=function(s){
  f=function(x,y,a=.5,b=.5,dummy=0){
    list(x*exp(s-x-a*y),
         y*exp(s-b*x-y))
  }
}
amin=0 #1
amax=3
xmin=0
xmax=7

adisc=.01 #.05
xdisc=.01 #.05
as=seq(amin,amax,by=adisc)
distance.origin=seq(xmin,xmax,by=xdisc)

evalPoint=function(s){
  m<-dsmodel(gen(s),display = FALSE)
  period=get.fps(m,.5,.5)
  period
}




fps=mapply(evalPoint,as)

z=matrix(NA,length(as),length(distance.origin))


get=function(a){
  if(length(a)>0)
    mapply(min,xmax,a[[1]])
  else
    c()
}

list.to.dist=function(x){


}

#using for for now
for(i in 1:length(fps)){
  #for 2d systems, find dist origin
  print(i)
  z[i,1/xdisc*mapply(dist.origin,fps[[i]])]=1
  z[i,-1+1/xdisc*mapply(dist.origin,fps[[i]])]=1
  z[i,1+1/xdisc*mapply(dist.origin,fps[[i]])]=1

  #for 1d models, just the xval of the fixed point.
  #z[i,1+1/xdisc*mapply(get,fps[[i]])]=1
  #z[i,1/xdisc*mapply(get,fps[[i]])]=1
  #z[i,-1+1/xdisc*mapply(get,fps[[i]])]=1
}
r=as
image(r,distance.origin, z)
}


