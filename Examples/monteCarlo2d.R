
fun=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}

apply = function(x, y, ..., iters=1, accumulate=TRUE) {
  if(is.null(x) || is.null(y)){
    stop("dsmodel: Please make sure your x and y values are defined in latest object created.")
  }

  if(accumulate) {
    iterAccum = vector("list",iters+1)
    startvals = list(x=x, y=y)

    iterAccum[[1]] <- startvals
    iterSeq = 1:iters
    if(iters==0)
      iterSeq <- NULL
    for(i in iterSeq){
      tmp=fun(x,y,...)
      if(any(is.nan(tmp[[1]])) || any(is.nan(tmp[[2]])))
      {
        warning("dsmodel: model undefined, NaN computed. (Division by zero). Removing points with NaN value and continuing procedure..")
        tmp = NaNRemove(tmp)
      }
      names(tmp) <- c("x","y")
      x = tmp$x
      y = tmp$y
      iterAccum[[i+1]] <- tmp
    }
    iterAccum
  } else {
    for(i in 1:iters){
      tmp=fun(x,y,...)
      names(tmp) <- c("x","y")
      x = tmp$x
      y = tmp$y
    }
    names(tmp) <- c("x","y")
    tmp
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~
#classical, check 1st, 2nd, 4th,... if default is null, is unused
find.period = function( x, y, maxPeriod=128, initIters=1000000, powerOf2=TRUE,
                        epsilon=sqrt(.Machine$double.eps), ...){
  #moves all the points. stops if they are either all infinite, fixed, or if(crop==TRUE), outside of range

  startPoint <- apply(x=x,y=y,...,iters=initIters,accumulate=FALSE)

  x=startPoint$x
  y=startPoint$y

  candidates=apply(x, y, ...,iters=maxPeriod*2-1,accumulate=TRUE)
  last=candidates[[2*maxPeriod]]
  if(has.diverged(last$x,last$y)){  #check if points became inf
    #print("no period found, diverged")
    return(FALSE)
  }
  check124(candidates,epsilon,maxPeriod)
}

check124=function(candidates,epsilon,maxPeriod){
  test=candidates[[1]]
  period=FALSE
  j=1
  while(j<=maxPeriod && !period){ #check for fixed or periodicity
    image=candidates[[(j+1)]]
    dists=sqdist(test,image)
    if(dists < epsilon)
      period=TRUE
    else{
      j=2*j
    }
  }
  if(period){
    return(j)
  }
  return(Inf)
}



aMin=0
aMax=3
bMin=0
bMax=3
aname="s"
bname="r"
testX=.5
testY=.5
numPoints=100


#get grid
range=dsrange(c(aMin,aMax),c(bMin,bMax))
points=range$centers(discretize=(aMax-aMin)/numPoints)
A0=points$X0
B0=points$Y0

numPeriods=10
maxP=2^numPeriods

args=list(FUN=find.period, MoreArgs = list(x=testX, y=testY, initIters=1000000, epsilon=sqrt(.Machine$double.eps), maxPeriod=maxP))
args[[aname]]=A0
args[[bname]]=B0
pers=do.call(mapply,args)


ratio=function(counts){
  l=length(counts)
  counts[-l]/counts[-1]
}

counts=NULL
for(p in 0:numPeriods){
  counts[p+1]=sum(pers==2^p)
}
print(counts)
print(ratio(counts))

#calculate area
print((aMax-aMin)*(bMax-bMin)*counts/length(pers))
