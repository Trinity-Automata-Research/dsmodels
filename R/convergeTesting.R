#method to take list of points and test if the points diverge
#works with pairs of nubers instead of dspoints

testIS=FALSE
testATFOI=FALSE
testAGTSA=TRUE

#modification of find fixed points. returns false if any point diverges. dosent handle periodic orbits
is.stable = function(model, x, y, stride=8, maxIters=Inf, tolerance=sqrt(.Machine$double.eps)){
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

allGoToSameAttractor = function(model, x, y, stride=8, maxIters=Inf, tolerance=sqrt(.Machine$double.eps)){
  points=applyTillFixedOrInf(model,x,y,stride,maxIters,tolerance)
  anchorX=points$x[1]
  anchorY=points$x[1]
  #if two points are tolerance away from a fixed point, they can be at most 2*tolerance away from each other
  all(is.finite(unlist(points))& ((points$x-anchorX)^2+(points$y-anchorY)^2 <tolerance*2.1))
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


range=dsrange(3,3, discretize = .5)
centers=range$centers()
x=centers$X0
y=centers$Y0

if(testIS){
  print(is.stable(divM,1,1, maxIters=10))
  print(is.stable(convM,1,1, maxIters=10))
  print(is.stable(divM,1,1))
  print(is.stable(convM,1,1))

  #i should find a system that converges on some ranges and diverges on others
  print(is.stable(divM,x,y))
  print(is.stable(convM,x,y))
  print(is.stable(multConvM,x,y))
}

if(testATFOI){
  print(applyTillFixedOrInf(divM,1,1))
  print(applyTillFixedOrInf(convM,1,1))
  print(applyTillFixedOrInf(multConvM,1,1))
}

if(testAGTSA){
  #print(allGoToSameAttractor(divM,1,1))
  #print(allGoToSameAttractor(convM,1,1))
  print(allGoToSameAttractor(divM,x,y))
  print(allGoToSameAttractor(convM,x,y))
  print(allGoToSameAttractor(multConvM,x,y))
}

