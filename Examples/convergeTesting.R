#method to take list of points and test if the points diverge
#works with pairs of nubers instead of dspoints
#goals: add a range where if a point leaves, it is considered divergent,
#       add init iters,   periodicity checking

library(dsmodels)

testISA=FALSE
testISO=FALSE
testPeriod=TRUE
#in dspoint
sqdist <- function(a, b) {
  return((a[[1]]-b[[1]])^2 + (a[[2]]-b[[2]])^2)
}
#in dspoint
finite.points = function(points) {
  all(is.finite(unlist(points)))
}
#in dsmodel
has.diverged = function(self,x,y,  rangeMult=0){
  if(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult))
    finite.points(c(x,y))
  else
    all(x < rangeMult*self$range$xlim[[2]] & y < rangeMult*self$range$ylim[[2]])
}


#attractors can be "any", "one","one off axis"
#requires the model to have a range if rangeMult is nonzero.
#maybe altertitively xlim and ylim can be passed as arguments
is.stable = function(model, x, y, attractors="any", stride=8, maxIters=Inf, initIters=0,
                     tolerance=sqrt(.Machine$double.eps),epsilon=sqrt(tolerance),
                     rangeMult=0){
  if(!(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult)) && is.null(model$range)){
    stop("is.stable with rangeMult!=0 requires range() to have been composed with the model.")
  }
  if(initIters!=0){
    points=model$apply(x,y,iters=initIters,accumulate=FALSE,crop=FALSE)
    x=points$x
    y=points$y
  }

  if(attractors=="any")
    is.stableOne(model, x, y, stride, maxIters, tolerance, epsilon, rangeMult, singleAttractor = FALSE)
  else if(attractors=="one")
    is.stableOne(model, x, y, stride, maxIters, tolerance, epsilon, rangeMult)
  else if(attractors=="one off axis" || attractors=="oneOffAxis")
    is.stableOne(model, x, y, stride, maxIters, tolerance, epsilon, rangeMult, filterAxis = TRUE)
  else
    stop("valid attractors for is.stabel are \"any\", \"one\", and \"one off axis\".")
}



#checks that everything converges to a single point
is.stableOne = function(model, x, y, stride, maxIters, tolerance, epsilon,
                         rangeMult, singleAttractor = FALSE, filterAxis=FALSE){
  #moves all the points untill they are either all infinite, fixed, or outside of range*rangeMult
  counter <- 0
  moving <- TRUE
  while (moving &&counter<maxIters) {
    tmp <- model$apply(x,y,iters=stride,accumulate=FALSE,crop=FALSE)
    if(has.diverged(model,tmp[[1]],tmp[[2]],rangeMult))
      return(FALSE)
    if(all(abs((x-tmp[[1]])^2 + (y-tmp[[2]])^2) < stride*tolerance))
      moving <- FALSE
    x <- tmp[[1]]
    y <- tmp[[2]]
    counter = counter+stride
  }
  #periodic checking
  if(counter >maxIters && moving)
    warning("hit maxIters in is.stable")
  if(!moving) {
    noStrideImages = model$apply(x, y, 1, accumulate=FALSE, crop=FALSE)
    if(!all(((noStrideImages$x - x)^2 + (noStrideImages$y - y)^2)<tolerance))
      warning("points are only stable under stride, may have periodic attractors.")
      #tolernace here is the wrong parameter, because we're moving once instead of 8 times.
      #dividing by 8 is probably better(?), but maybe even as extreme as square-rooting.
      #ISSUE number 102
  }

  if(!singleAttractor)
    return(TRUE)

  #take out all points on the axis
  #potentialy having an attractor on the axis should return false.
  #this would check that there is one attractor that is interior,
  #not that there is only one attractor and it is interior.
  #changing is simple- if(any(onAxis)) return(FALSE)
  if(filterAxis){
    onAxis=abs(x)<epsilon | abs(y)<epsilon
    x=x[!onAxis]
    y=y[!onAxis]
  }




  anchorX=x[1]
  anchorY=y[1]
          #checks that all points are at the same attractor
  return(all((x-anchorX)^2+(y-anchorY)^2 < epsilon))

}



#in dsmodel's find period
getxy = function(x=NULL,y=NULL){
  if(!is.null(y))
    return(list(x=x,y=y))
  else if(is.vector(x))
    return(list(x=x[[1]],y=x[[2]]))
  else if(is.dspoint(x))
    return(list(x=x$x, y=x$y))
  else
    stop("expected input formats are two scalars, a vector of length two or a dspoint")
}

#in dsmodel
find.period = function(self, x, y,
                       initIters=1000, maxPeriod=128, numTries=1,
                       epsilon=sqrt(sqrt(.Machine$double.eps)), #should be using epsilon instead?
                       rangeMult=0){
  if(!(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult)) && is.null(self$range)){
    stop("is.stable with rangeMult!=0 requires range() to have been composed with the model.")
  }
  #moves all the points untill they are either all infinite, fixed, or outside of range*rangeMult
  for(i in 1:numTries) {
    startPoint <- self$apply(x,y,iters=initIters,accumulate=FALSE,crop=FALSE)
    if(has.diverged(self,startPoint$x,startPoint$y,rangeMult)){
      #print("no period found, diverged")
      return(FALSE)
    }
    candidates=self$apply(startPoint[[1]], startPoint[[2]] ,iters=maxPeriod,accumulate=TRUE,crop=FALSE)
    period=FALSE
    i=1
    while(i<maxPeriod && !period){
      ithPoint=candidates[[i+1]]
      if(sqdist(startPoint, ithPoint) < epsilon)
        period=TRUE
      else
        i=i+1
    }
    if(period){
      return(i)
    }
    x=ithPoint$x
    y=ithPoint$y
  }
  warning(paste("Assuming divergance: no period found after",(initIters+maxPeriod)*numTries,"iterations. Consider increasing initIters."))
  return(FALSE)
}

#testing
if(TRUE){
range=dsrange(3,3, discretize = .01)



#always diverges
f<-function(x,y) {list(x=x*1.9, y=y*1.9)}
divM<-dsmodel(f)
divM+range
#always converges to 0,0
g<-function(x,y) list(x=x*.9, y=y*.9)
convM<-dsmodel(g)
convM+range
#converges to multiple attractors
r1 <- 2.6
r2 <- 2.6
m1 <- 6.45
m2 <- 6.25
s1 <- 4.5
s2 <- 4.5
b <- 0.15
h <- function(X0,Y0) {
  xp <- X0*exp(r1-X0-m1/(1+s1*X0))
  yp <- Y0*exp(r2-Y0-b*X0-m2/(1+s2*Y0))
  list(xp,yp)
}
multConvM <- dsmodel(fun = h)

#periodic
g <- function(x,y){list(1/x,1/y)}
periodicM <- dsmodel(g)



centers=range$corners(discretize=.5)
x=centers$X0
y=centers$Y0

if(testISA){
  #print(is.stable(divM,1,1, maxIters=10, attractors="any"))
  #print(is.stable(convM,1,1, maxIters=10, attractors="any"))
  #print(is.stable(divM,1,1, attractors="any"))
  #print(is.stable(convM,1,1, attractors="any"))

  #i should find a system that converges on some ranges and diverges on others
  print("Testing any stability for divM")
  print(is.stable(divM,x,y, attractors="any"))
  print("Testing any stability for convM")
  print(is.stable(convM,x,y, attractors="any"))
  print("Testing any stability for multConvM")
  print(is.stable(multConvM,x,y, attractors="any"))
}



if(testISO){
  #multConvM+range+dsarrows(discretize=.2)+simattractors(discretize = 1)
  #sb=simbasins(discretize = 1)
  #multConvM+sb

  #print(is.stableOne(divM,1,1))
  #print(is.stableOne(convM,1,1))
  #system.time((is.stable(divM,x,y,attractors="one")))
  #system.time((is.stable(divM,x,y,attractors="one")))
  print("testing single-point stability")
  print(is.stable(divM,x,y,attractors="one",rangeMult = 3))
  print(is.stable(divM,x,y,attractors="one"))
  print(is.stable(convM,x,y,attractors="one"))
  print(is.stable(multConvM,x,y,attractors="one"),initIters=20)
  print(is.stable(multConvM,x,y,attractors="oneOffAxis"))
  print(is.stable(periodicM,c(2),c(2),attractors="one"))
}
}
#testPeriod=FALSE
if(testPeriod){
  print("testing periodicity")

  #fixed point
  print("Convm, 2,2 - should be fixed")
  print(find.period(convM,2,2))
  #inconclusive
  print("Divm, 2,2")
  print(find.period(divM,2,2))
  #limit the range
  print("Divm, 2,2 with range bounding")
  print(find.period(divM,2,2,rangeMult=3))
  #increase the iterations
  print("Divm, 100,100 with 8 tries of 1k iters")
  print(find.period(divM,100,100, initIters=1000, numTries = 8))
  #find a period of 2
  print("periodicM 2,2, should be 2")
  print(find.period(periodicM,2,2))
  #multi periodic
  #try to find systems with periods that arent a power of two
  i=function(x,y) list(1/(x^3-2*x^2-3*x+1),y)
  multiPeriodicM=dsmodel(i)
  print(find.period(multiPeriodicM, initIters=1000,.8,.8))

  i=function(x,y) list(x/(-2*x^3-x^2+x),y)
  multiPeriodicM=dsmodel(i)
  print(find.period(multiPeriodicM, initIters=1000,.8,.8))



  #this function takes a long time to find its period and then dosent agree.
  #i think epsilon and tolerance do weird things with rounding preciscion

  i=function(x,y) list(1/(x^3-2*x^2-2*x+1),y)
  multiPeriodicM=dsmodel(i)
  multiPeriodicM+dsrange(c(-4,4), c(-4,4))
  multiPeriodicM + dspoint(1.11,2,iters=10,col = "yellow", image = "blue")
  multiPeriodicM+dsarrows(discretize=0.5)
  print("Multiperiodicm checking")
  tol=sqrt(sqrt(.Machine$double.eps))*10
  print(find.period(multiPeriodicM,.8,.8, epsilon = tol))
  print(find.period(multiPeriodicM,-0.414963938818,.8, initIters=100000, epsilon = tol))
  print(find.period(multiPeriodicM, initIters=150000,numTries=10,.8,.8, epsilon = tol))
  print(find.period(multiPeriodicM, initIters=100000,numTries=10,.8,.8, epsilon = tol))
  print(find.period(multiPeriodicM, initIters=10000,numTries=10,.8,.8, epsilon = tol))
}
