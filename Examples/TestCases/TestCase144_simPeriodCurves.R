#param curve tester

f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}

mod=dsmodel(f)

mod + paramrange(3,3,discretize = .1, paramNames = c(s,r),renderCount = 20)
mod + sim.map.period(.5,.5,maxPeriod = 8, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE)

#not parametric
c=dscurve(x/2,simPeriod = TRUE)
mod+c
print(c$narrow(tolerance=.001)) #refine the ranges
print(c$phases(distances=TRUE))



#parametric

c=dscurve(1*t,3*t,simPeriod = TRUE)
mod+c
print(c$narrow())
print(c$phases(distances=TRUE,params=TRUE,source=TRUE))


#TODO: test with different parameters
