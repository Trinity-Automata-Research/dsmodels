#find.period but it returns a list of points
#make a 1d bif diagram for 1d mod


amin=2
amax=4
xmin=0
xmax=1

adisc=.001
xdisc=.001
as=seq(amin,amax,by=adisc)
xs=seq(xmin,xmax,by=xdisc)




finite.points = function(points) {
  all(is.finite(unlist(points)))
}

in.range = function(x,y, model, rangeMult=0){
  if(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult))
    finite.points(c(x,y))
  else
    all(x < rangeMult*model$range$xlim[[2]] & y < rangeMult*model$range$ylim[[2]])
}

dist.origin <- function(a) sqdist(a,c(0,0))

sqdist <- function(a, b) {
  return((a[[1]]-b[[1]])^2 + (a[[2]]-b[[2]])^2)
}

find.period = function(model, x, y,
                       initIters=1000, maxPeriod=128, numTries=1,
                       tolerance=sqrt(.Machine$double.eps),epsilon=sqrt(tolerance), #should be using epsilon instead?
                       rangeMult=0){
  if(!(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult)) && is.null(model$range)){
    stop("is.stable with rangeMult!=0 requires range() to have been composed with the model.")
  }
  #moves all the points untill they are either all infinite, fixed, or outside of range*rangeMult
  for(i in 1:numTries) {
    startPoint <- model$apply(x,y,iters=initIters,accumulate=FALSE,crop=FALSE)
    if(!in.range(startPoint$x,startPoint$y,model,rangeMult)){
      #print("no period found, diverged")
      return(list())
    }
    candidates=model$apply(startPoint[[1]], startPoint[[2]] ,iters=maxPeriod,accumulate=TRUE,crop=FALSE)
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
  return(list())
}



g = function(a=.5){
  function(x,y){
    list(x=a*x*(1-x),y=0)
  }
}


evalPoint=function(a){
  m<-dsmodel(g(a),display = FALSE)
  period=find.period(m,.5,.5,numTries = 3,tolerance=.001)
  period
}

fps=mapply(evalPoint,as)

z=matrix(NA,length(as),length(xs))
#using for for now
for(i in 1:length(fps)){
  z[i,1/xdisc*mapply(dist.origin,fps[[i]])]=1
  z[i,-1+1/xdisc*mapply(dist.origin,fps[[i]])]=1
  z[i,1+1/xdisc*mapply(dist.origin,fps[[i]])]=1
}

image(as,xs, z)
