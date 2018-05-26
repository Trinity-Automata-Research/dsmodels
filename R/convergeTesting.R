#method to take list of points and test if the points diverge

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

f<-function(x,y) list(x=x*1.1, y=y*1.1)
m<-dsmodel(f)
g<-function(x,y) list(x=x*.9, y=y*.9)
n<-dsmodel(g)

print(is.stable(m,1,1, maxIters=10))
print(is.stable(n,1,1, maxIters=10))
print(is.stable(m,1,1,))
print(is.stable(n,1,1,))

range=dsrange(3,3, discretize = .5)
centers=range$centers()
x=centers$X0
y=centers$Y0


#i should find a system that converges on some ranges and diverges on others
#print(is.stable(m,x,y))
#print(is.stable(n,x,y))
