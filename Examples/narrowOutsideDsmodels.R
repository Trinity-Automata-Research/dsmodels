#stability testing without dsmodels. want to find all the bifurcation points
#assumes the function plays nice

#initDiscretize=TRUE breaks it. find.period calls some points divergent and they break up the phases.

#phases overlap? look in narrow
# the actual bifurcation points of the logistic map
actual= c(0,3,3.449490,3.544090,3.564407,3.568750,3.56969,3.56989,3.569934,3.569943,3.5699451,3.569945557)


aMin=0
#aMax=4
aMax=3.569945672 #the start of chaos
find.period.args=list(iters=10000, maxPeriod=128, initIters=1000, numTries=15, convergeCheck=2^10, powerOf2=TRUE, epsilon=sqrt(.Machine$double.eps))
aname="s"
bname="dummy"
testX=.1
testY=.1
numPoints=10
narrowTol=.00000000001

fun=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(s*x*(1-x),
       0)
}
curve=function(s)2 # the curve dummy=2

#run these after the file has been run
frame= phases(narrow(initDiscretize=TRUE, tolerance=narrowTol))
print(frame, digits=15)
print(actual-frame$start, digits=15) #only look at the first couple of terms, up to the first chaotic term.
#print(actual[-1]-frame$stop, digits=15)
print(list(actual=actual, guess=frame$start),digits=10)
#~~~~~~~~~~~~~~~~~~~~~~~~
#helper for narrow
#creates an initial discretized curve, turns it into an approximate phase frame
initFrame=function(){
  sources <- seq(aMin,aMax, length.out = numPoints)
  xValues <-mapply(self$getX,sources)
  yValues <-mapply(self$getY,sources)

  mArgs=c(list(x=testX,y=testY), find.period.args)
  args=list(FUN=find.period, MoreArgs=mArgs)
  args[[aname]]=xValues
  args[[bname]]=yValues
  periods=do.call(mapply,args)
  transitions = rle(periods)
  p = cumsum(transitions$lengths)
  n = length(p)
  starts = c(1,(p+1)[-n])
  ends = p
  phaseFrame = data.frame(start  = sources[starts],
                               period = transitions$values,
                               stop   = sources[ends])

}

#helper for find.period
has.diverged = function(x, y){
  if(length(x)<1 || length(y)<1){    #check for numeric(0)
    return(TRUE)                     #this only works if x and y are not lists. to check has.diverged on multiple
  }else{                                  #points, you would need to either use mapply or change has.diverged.
  return(!finite.points(c(x,y)))}
}

#helper for find.period
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

#helper for find.period
NaNRemove <- function(twoDList){
  tmp <- twoDList
  if(length(tmp[[1]])!=length(tmp[[2]])){
    stop("dsmodel: X and Y of different length. Internal Error.")
  }
  ridFun <- function(x) !is.nan(x)
  toRidX <- boolpos(ridFun,tmp[[1]])
  toRidY <- boolpos(ridFun,tmp[[2]])
  rid <- c(toRidX,toRidY)
  if(!is.null(rid)) {
    list(
      tmp[[1]] <- tmp[[1]][-rid],
      tmp[[2]] <- tmp[[2]][-rid]
    )
  }
}


#find period, for a given x,y,parameter, what is the periodicity of the orbit of (x,y).
#many arguments to tweak how the testing is done
find.period = function( x, y, iters, maxPeriod, initIters, numTries, powerOf2,
                                epsilon, xlim, ylim, convergeCheck, ...){
  #moves all the points. stops if they are either all infinite, fixed, or if(crop==TRUE), outside of range

  startPoint <- apply(x=x,y=y,...,iters=initIters,accumulate=FALSE)

  x=startPoint$x
  y=startPoint$y

  for(i in 1:numTries) {
    startPoint <- apply(x=x,y=y,...,iters=iters,accumulate=FALSE)
    candidates=apply(startPoint$x, startPoint$y, ...,iters=maxPeriod*2-1,accumulate=TRUE)
    convergePoint <- apply(startPoint$x,startPoint$y,...,iters=convergeCheck,accumulate=FALSE)
    compareCandidates= apply(convergePoint$x, convergePoint$y, ...,iters=maxPeriod*2-1,accumulate=TRUE)
    last=compareCandidates[[2*maxPeriod]]
    if(has.diverged(last$x,last$y)){
      #print("no period found, diverged")
      return(FALSE)
    }
    dists=mapply(sqdist,candidates,compareCandidates)
    #check if function has converged. if it has check for periodicity, otherwise go to next pass of this loop. exception:
    if(i==numTries || sum(dists) < epsilon){  # check for periodicity should always happen if i=numTries.i.e. if on last try, continue anyways
      period=FALSE
      j=1
      while(j<=maxPeriod && !period){ #check for fixed or periodicity
        test=candidates[1:j]
        image=candidates[(j+1):(2*j)]
        dists=mapply(sqdist,test,image)
        if(all(dists < epsilon))
          period=TRUE
        else{
          if(powerOf2)
            j=2*j
          else
            j=j+1
        }
      }
      if(period){
        return(j)
      }
    }
    #update x,y to a point further in the orbit

      x=last$x
      y=last$y
  }
  return(Inf)
}

#helper for find.period
sqdist <- function(a, b) {
  return((a[[1]]-b[[1]])^2 + (a[[2]]-b[[2]])^2)
}

self$getX=function(x)x
self$getY=function(x)curve(x)


#helper for narrow
recurNarrow=function(start, startP, stop, stopP, tolerance=sqrt(sqrt(.Machine$double.eps))){
  #narrows the gaps in between periods to within tolerance, returns list of vectors (in case new periods found)
  if(self$distOfSources(start,stop) < tolerance) #calculate xydist. if gap is small enough, we are done.
    return(data.frame(start=start, startP=startP, stop=stop, stopP=stopP))
  #calculate the periodicity of the midpoint
  midPoint=(start+stop)/2
  a=self$getX(midPoint)
  b=self$getY(midPoint)
  args=c(list(x=testX,y=testY), find.period.args)
  args[[aname]]=a
  args[[bname]]=b
  p=do.call(find.period,args)
  if(p==startP)   #gap gets smaller
    return(recurNarrow(midPoint,startP,stop,stopP,tolerance))
  else if(p==stopP)
    return(recurNarrow(start,startP,midPoint,stopP,tolerance))
  else{           #gap splits into two gaps
    g1=recurNarrow(start,startP,midPoint,p,tolerance)
    g2=recurNarrow(midPoint,p,stop,stopP,tolerance)
    return(rbind(g1,g2))
  }
}


#narrow now returns the frame
#initDiscretize controls:
#can either start with a pre-given discretized curve to narrow in on the gaps or
#start with one gap from start to end
narrow= function(tolerance=sqrt(sqrt(.Machine$double.eps)), initDiscretize=TRUE){
  if(initDiscretize){
    pf = initFrame()
    end = nrow(pf)
    firstStart=pf[1,]$start
    lastStop=pf[end,]$stop
    #convert phases into gaps and recursively narrow
    tmp=mapply(recurNarrow,
               pf$stop[-end], pf$period[-end],
               pf$start[-1], pf$period[-1],
               MoreArgs = list(tolerance=tolerance), SIMPLIFY = FALSE)
    gaps=Reduce(rbind,tmp)

    #convert back to phases
    data.frame(
      start=c(firstStart,gaps$stop),
      stop=c(gaps$start, lastStop),
      period = c(gaps$startP[1], gaps$stopP))

  }
  else{
    a=self$getX(aMin)
    b=self$getY(aMin)
    args=c(list(x=testX,y=testY), find.period.args)
    args[[aname]]=a
    args[[bname]]=b
    minp=do.call(find.period,args)

    a=self$getX(aMax)
    b=self$getY(aMax)
    args=c(list(x=testX,y=testY), find.period.args)
    args[[aname]]=a
    args[[bname]]=b
    maxp=do.call(find.period,args)
    gaps=recurNarrow(aMin,minp, aMax, maxp, tolerance = tolerance)
    #convert back into phases
    data.frame(
      start=c(aMin,gaps$start),
      stop=c(gaps$stop, aMax),
      period = c(gaps$startP[1], gaps$stopP))
  }


}

#helper for phases
distOfSources=function(self, pointA, pointB){
  x1=self$getX(pointA)
  y1=self$getY(pointA)
  x2=self$getX(pointB)
  y2=self$getY(pointB)
  sqdist(c(x1,y1),c(x2,y2))
}

#helper for phases
addDistanceToPhase=function(self,inPhase){
  findDist=function(index,phases){
    sqrt(self$distOfSources(phases[index,]$start, phases[index,]$stop))
  }
  dist=mapply(findDist, 1:nrow(inPhase), MoreArgs=list(inPhase))
  withDist=cbind(inPhase, dist)
  findRatio=function(index,phases){
    (phases[index,]$dist)/(phases[index+1,]$dist)
  }
  ratio=append(NA, mapply(findRatio, 1:(nrow(inPhase)-1), MoreArgs=list(withDist)))
  cbind(withDist, ratio)
}


#mostly rearranging data, computing distances
phases=function(phaseFrame, distances=TRUE, sources=TRUE, params=FALSE){  #add or take out columns of phaseFrame according to parameters.
  dsassert(self$bound, "To use this method the curve must be bound to a model")
  dsassert(self$simPeriod, "To use this method the curve be constructed with simPeriod=TRUE")
  ret=phaseFrame
  if(params){ #add the value of the parameters to the data frame
    startA=paste("start",aname)
    stopA=paste("stop",aname)
    startB=paste("start",bname)
    stopB=paste("stop",bname)
    start=ret$start
    stop=ret$stop
    add=data.frame(self$getX(start),self$getY(start),self$getX(stop),self$getY(stop))
    names(add)=c(startA,startB,stopA,stopB)
    ret=cbind(ret,add)[c("period","start",startA,startB,"stop",stopA,stopB)]
  }
  if(distances){ #add the distances of each phase to thte data frame
    ret=self$addDistanceToPhase(ret)
  }
  if(!sources){ #remove the source values from the dataFrame
    ret[,c("start","stop")]=NULL
  }
  ret
}
