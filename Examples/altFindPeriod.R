#different ways to find periodicity
#run narrow outside dsmodel

#~~~~~~~~~~~~~~~~~~~~~~~~
#classical, unique buckets. if default is null, is unused
find.period = function( x, y, iters=NULL, maxPeriod=128, initIters=500, numTries=NULL, powerOf2=TRUE,
                        epsilon=sqrt(.Machine$double.eps), convergeCheck=NULL, ...){
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
  return(numBuckets(candidates,epsilon)) #any period 128 or greater is called 128
  return(Inf)
}

#helper for classical unique bucket
#in 1d could sort first then find in one pass.
#in general, need n! comparisons
numBuckets=function(points, epsilon=sqrt(.Machine$double.eps)){ #removes all elements close to the first, repeats
  bucket=0
  while(length(points)>0){
    test=points[1]
    dists=mapply(sqdist,test,points)
    points[dists < epsilon]=NULL
    bucket=bucket+1
  }
  bucket
}



#~~~~~~~~~~~~~~~~~~~~~~~~
#classical, check 1st, 2nd, 4th,... if default is null, is unused
find.period = function( x, y, iters=NULL, maxPeriod=128, initIters=500, numTries=NULL, powerOf2=TRUE,
                        epsilon=sqrt(.Machine$double.eps), convergeCheck=NULL, ...){
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


#tester
mu=2.9 #1
find.period(.1,.1, s=mu)
mu=3.4 #2
find.period(.1,.1, s=mu)
mu=3.5 #4
find.period(.1,.1, s=mu)
mu=3.56 #8
find.period(.1,.1, s=mu)
mu=3.565 #16
find.period(.1,.1, s=mu)
mu=3.5688 #32
find.period(.1,.1, s=mu)
mu=3.5697 #64
find.period(.1,.1, s=mu)


#pts=pts=apply(.1,.1, s=mu ,iters=127,accumulate=TRUE)[64:127]

#numBuckets(pts)

#find.period(.1,.1, s=mu)
