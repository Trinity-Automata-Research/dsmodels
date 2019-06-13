#hacky way to do 1d systems, y variable is always 0


f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(s*x*(1-x),
       0)
}

mod=dsmodel(f)

mod + paramrange(4,4,discretize = .1, paramNames = c(s,dummy),renderCount = 20)
mod + sim.map.period(maxPeriod = 8, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE)

#not parametric
c=dscurve(2, xlim=c(0,4), simPeriod = TRUE, find.period.args=list(iters=10000, maxPeriod=128, initIters=1000, numTries=30 ),col="black")
mod+c
print(c$narrow(tolerance=.0000001)) #refine the ranges
print(c$phases(distances=TRUE),digits=10)

##
#  testing what the orbits look like near the bifurcation points
#  hopefully we can find a way to differentiate between fixed points and
#  cycles whose orbits dont move very far (i.e. {c, c+delta} as a 2-cycle
#  where delta<epsilon)
##

#s<3,  is fixed point
sval=2.998
its=4000
last=8
pts=mod$apply(.1,.1, s=sval, iters=its, crop=FALSE)[(its-last+1):its ]
print(mapply(1:last,FUN=function(i){pts[[i]]$x}),digits=22)
mapply(sqdist,pts[1:(last/2)],pts[(last/2+1):last])
mapply(sqdist,pts[1:(last/4)],pts[(last/4+1):(last/2)])
mapply(sqdist,pts[1],pts[2])

convDist=2^7
converge=mod$apply(pts[[last]]$x, pts[[last]]$y, s=sval, iters=convDist, crop=FALSE)[(convDist-last+1):convDist ]
print(mapply(1:last,FUN=function(i){converge[[i]]$x}),digits=22)
mapply(sqdist,pts,converge)
#wholePeriodCheck(pts,.Machine$double.eps,4,TRUE)
#looks very periodic, is actually converging eventually

its=4000
pts=mod$apply(.1,.1, s=sval, iters=its, crop=FALSE)[(its-last+1):its ]
mapply(1:last,FUN=function(i){pts[[i]]$x})


sval=3.564407215 #just past period 16 bif point, find.period calls it period 8.
its=1000000
last=32
pts=mod$apply(.1,.1, s=sval, iters=its, crop=FALSE)[(its-last+1):its ]
print(mapply(1:last,FUN=function(i){pts[[i]]$x}),digits=20)
mapply(sqdist,pts[1:(last/2)],pts[(last/2+1):last])

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

f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*(1-x)*s,0)
}

mod=dsmodel(f)

mod + paramrange(4,4,discretize = .1, paramNames = c(s,dummy),renderCount = 20)
mod + sim.map.period(maxPeriod = 8, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE)

#not parametric
c=dscurve(2,simPeriod = TRUE, find.period.args=list(numTries=5),col="black")
mod+c
print(c$narrow(tolerance=.000001, redisplay=FALSE)) #refine the ranges
print(c$phases(distances=TRUE))





