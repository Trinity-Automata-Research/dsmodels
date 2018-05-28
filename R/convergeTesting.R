#method to take list of points and test if the points diverge
#works with pairs of nubers instead of dspoints
library(dsmodels)

testISA=FALSE
testATFOI=FALSE
testISO=TRUE

#attractors can be "any", "one","one off axis"
is.stable = function(model, x, y, attractors="any", stride=8, maxIters=Inf, tolerance=sqrt(.Machine$double.eps),epsilon=100*tolerance){
  if(attractors=="any")
    is.stableAny(model, x, y, stride, maxIters, tolerance)
  else if(attractors=="one")
    is.stableOne(model, x, y, stride, maxIters, tolerance)
  else if(attractors=="one off axis" || attractors=="oneOffAxis")
    is.stableOne(model, x, y, stride, maxIters, tolerance, epsilon = epsilon, filterAxis = TRUE)
  else
    stop("valid attractors for is.stabel are \"any\", \"one\", and \"one off axis\".")
}


#modification of find fixed points. returns false if any point diverges. dosent handle periodic orbits
is.stableAny = function(model, x, y, stride=8, maxIters=Inf, tolerance=sqrt(.Machine$double.eps)){
  xp <- x
  yp <- y
  counter <- 0
  while (counter<maxIters) {
    tmp <- model$apply(xp,yp,iters=stride,accumulate=FALSE,crop=FALSE)
    if(any(is.infinite(unlist(tmp))))
      return(FALSE)
    else if(all(abs((xp-tmp[[1]])^2 + (yp-tmp[[2]])^2) < tolerance))
      return(TRUE)
    xp <- tmp[[1]]
    yp <- tmp[[2]]
    counter = counter+stride
  }
  "inconclusive" #probably not the best way
}

#dosent handle periodic orbits
applyTillFixedOrInf = function(model, x, y, stride=8, maxIters=Inf, tolerance=sqrt(.Machine$double.eps)){
  xp <- x
  yp <- y
  counter <- 0
  moving <- TRUE
  while (moving &&counter<maxIters) {
    tmp <- model$apply(xp,yp,iters=stride,accumulate=FALSE,crop=FALSE)
    if(any(is.infinite(unlist(tmp))) || all(abs((xp-tmp[[1]])^2 + (yp-tmp[[2]])^2) < tolerance))
      moving <- FALSE
    xp <- tmp[[1]]
    yp <- tmp[[2]]
    counter = counter+stride
  }
  #mapply(c,xp,yp,SIMPLIFY = FALSE)
  list(x=xp,y=yp)
}

is.stableOne = function(model, x, y, stride=8, maxIters=Inf, tolerance=sqrt(.Machine$double.eps), epsilon=100*tolerance, filterAxis=FALSE){
  points=applyTillFixedOrInf(model,x,y,stride,maxIters,tolerance)
  x=points$x
  y=points$y
  #take out all points on the axis
  if(filterAxis){
    onAxis=abs(x)<tolerance | abs(y)<tolerance
    x=x[!onAxis]
    y=y[!onAxis]
  }
  anchorX=x[1]
  anchorY=y[1]
  all(is.finite(unlist(points)))&& all((x-anchorX)^2+(y-anchorY)^2 < epsilon)
}



#always diverges
f<-function(x,y) list(x=x*1.1, y=y*1.1)
divM<-dsmodel(f)

#always converges to 0,0
g<-function(x,y) list(x=x*.9, y=y*.9)
convM<-dsmodel(g)

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


range=dsrange(3,3, discretize = 1)
centers=range$corners(discretize=.5)
x=centers$X0
y=centers$Y0

if(testISA){
  print(is.stable(divM,1,1, maxIters=10, attractors="any"))
  print(is.stable(convM,1,1, maxIters=10, attractors="any"))
  print(is.stable(divM,1,1, attractors="any"))
  print(is.stable(convM,1,1, attractors="any"))

  #i should find a system that converges on some ranges and diverges on others
  print(is.stable(divM,x,y, attractors="any"))
  print(is.stable(convM,x,y, attractors="any"))
  print(is.stable(multConvM,x,y, attractors="any"))
}

if(testATFOI){
  print(applyTillFixedOrInf(divM,1,1))
  print(applyTillFixedOrInf(convM,1,1))
  print(applyTillFixedOrInf(multConvM,1,1))
}

if(testAGTSA){
  multConvM+range+dsarrows(discretize=.2)+simattractors(discretize = .1)

  sb=simbasins(discretize = 100)
  multConvM+sb
  multConvM+dspoint(1,1)

  #print(is.stableOne(divM,1,1))
  #print(is.stableOne(convM,1,1))
  print(is.stable(divM,x,y,attractors="one"))
  print(is.stable(convM,x,y,attractors="one"))
  print(is.stable(multConvM,x,y,attractors="one"))
  print(is.stable(multConvM,x,y,attractors="oneOffAxis"))
}

