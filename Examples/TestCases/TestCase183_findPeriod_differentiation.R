#hacky way to do 1d systems, y variable is always 0


f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(s*x*(1-x),
       0)
}

mod=dsmodel(f)

mod + paramrange(4,4,discretize = .1, paramNames = c(s,dummy),renderCount = 20)
mod + sim.map.period(maxPeriod = 8, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE)

#not parametric
c=dscurve(2, xlim=c(0,3.582394), simPeriod = TRUE, find.period.args=list(iters=10000, maxPeriod=128, initIters=1000, numTries=5 ),col="black")
mod+c
print(c$narrow(tolerance=.00001)) #refine the ranges
print(c$phases(distances=TRUE))

##
#  testing what the orbits look like near the bifurcation points
#  hopefully we can find a way to differentiate between fixed points and 
#  cycles whose orbits dont move very far (i.e. {c, c+delta} as a 2-cycle
#  where delta<epsilon)
##

#s<3,  is fixed point
sval=2.998
its=1000
last=8
pts=mod$apply(.1,.1, s=sval, iters=its, crop=FALSE)[(its-last+1):its ] 
mapply(1:last,FUN=function(i){pts[[i]]$x})
#looks very periodic, is actually converging eventually

its=4000
pts=mod$apply(.1,.1, s=sval, iters=its, crop=FALSE)[(its-last+1):its ] 
mapply(1:last,FUN=function(i){pts[[i]]$x})



if(FALSE){
h=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*x-s,
       0)
}


mod=dsmodel(h)

mod + paramrange(4,4,discretize = .1, paramNames = c(s,dummy),renderCount = 20)
mod + sim.map.period(maxPeriod = 8, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE)

#not parametric
c=dscurve(2,simPeriod = TRUE, find.period.args=list(iters=10000, maxPeriod=128, initIters=100, numTries=5 ),col="black")
mod+c
print(c$narrow(tolerance=.0000001)) #refine the ranges
print(c$phases(distances=TRUE))

}







