#method to take list of points and test if the points diverge
#works with pairs of nubers instead of dspoints
#goals: add a range where if a point leaves, it is considered divergent,
#       add init iters,   periodicity checking

library(dsmodels)

testISA=FALSE
testISO=FALSE
testPeriod=TRUE

finite.points = function(points) {
  all(is.finite(unlist(points)))
}

in.range = function(x,y, model, rangeMult=0){
  if(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult))
    finite.points(c(x,y))
  else
    all(x < rangeMult*model$range$xlim[[2]] & y < rangeMult*model$range$ylim[[2]])
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
    if(!in.range(tmp[[1]],tmp[[2]],model,rangeMult))
      return(FALSE)
    if(all(abs((x-tmp[[1]])^2 + (y-tmp[[2]])^2) < tolerance))
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


is.periodic = function(model, x, y,
                       stride=8, initIters=100, maxPeriod=128, numTries=1,
                       tolerance=sqrt(.Machine$double.eps),epsilon=sqrt(tolerance),
                       rangeMult=0){
  #moves all the points untill they are either all infinite, fixed, or outside of range*rangeMult
  tryCounter <- 0
  while(tryCounter < numTries){
    counter <- 0
    moving <- TRUE
    while (moving &&counter<initIters) {
      tmp <- model$apply(x,y,iters=stride,accumulate=FALSE,crop=FALSE)
      if(!in.range(tmp[[1]],tmp[[2]],model,rangeMult)){
        print("no period found, diverged")
        return(FALSE)
      }
      if(all(abs((x-tmp[[1]])^2 + (y-tmp[[2]])^2) < tolerance))
        moving <- FALSE
      x <- tmp[[1]]
      y <- tmp[[2]]
      counter = counter+stride
    }
    #periodic checking
    candidates=model$apply(x,y,iters=maxPeriod,accumulate=TRUE,crop=FALSE)
    period=FALSE
    counter=0
    while(counter<maxPeriod && !period){
      if(abs((x-candidates[[counter+2]][[1]])^2 + (y-candidates[[counter+2]][[2]])^2) < tolerance)
        period=TRUE
      counter=counter+1
    }
    if(period){
      if(counter==1){
        print("found a fixed point")
        return(FALSE)
      }
      else{
        print(paste("found a period of length", counter))
        #print(candidates[1:counter+1])
        return(TRUE)
      }
    }
    tryCounter=tryCounter+1
    x=candidates[[maxPeriod+1]]$x
    y=candidates[[maxPeriod+1]]$y
  }
  print("no period found, still moving after initIters*stride*numTries applications") #caculate that and give them an actual number?
  FALSE
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
  print(is.stable(divM,x,y, attractors="any"))
  print(is.stable(convM,x,y, attractors="any"))
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
  print(is.stable(divM,x,y,attractors="one",rangeMult = 3))
  print(is.stable(divM,x,y,attractors="one"))
  print(is.stable(convM,x,y,attractors="one"))
  print(is.stable(multConvM,x,y,attractors="one"),initIters=20)
  print(is.stable(multConvM,x,y,attractors="oneOffAxis"))
  print(is.stable(periodicM,c(2),c(2),attractors="one"))
}
}

if(testPeriod){
  is.periodic(convM,2,2)
  is.periodic(divM,2,2)
  is.periodic(divM,100,100, initIters=1000, numTries = 8)
  is.periodic(periodicM,2,2)
  #multi periodic
  i=function(x,y) list(1/(x^3-2*x^2-3*x+1),y)
  multiPeriodicM=dsmodel(i)
  is.periodic(multiPeriodicM, initIters=10000,.8,.8)
  #this function gives a problem. it dosent stop, but also dosent have a period
  #i=function(x,y) list(1/(x^3-2*x^2-2*x+1),y)
  #multiPeriodicM=dsmodel(i)
  #is.periodic(multiPeriodicM, initIters=100000,.8,.8)
  i=function(x,y) list(x/(-2*x^3-x^2+x),y)
  multiPeriodicM=dsmodel(i)
  is.periodic(multiPeriodicM, initIters=100000,.8,.8)

}

