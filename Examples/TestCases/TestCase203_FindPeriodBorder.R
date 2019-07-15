#test case for isue #203
#differentiate between stable points/cycles in the positive cone and ones that are on the axes.
#when flag ignoreExtinction is set to FALSE, simmap period should only have results in a slice


f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}


#does ignore, should color whole thing
model = dsmodel(f)

model + paramrange(3,3,discretize = .1, paramNames = c(s,r))

model + sim.map.period(.5,.5,maxPeriod = 16, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE, ignoreExtinction = TRUE)

#dont specify, should default to does ignore, color whole thing
model = dsmodel(f)

model + paramrange(3,3,discretize = .1, paramNames = c(s,r))

model + sim.map.period(.5,.5,maxPeriod = 16, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE, ignoreExtinction = TRUE)



#doent ignore, should make triangle surrounded by blue
model = dsmodel(f)

model + paramrange(3,3,discretize = .1, paramNames = c(s,r))

model + sim.map.period(.5,.5,maxPeriod = 16, epsilon=.0001, iters = 100, numTries = 1, powerOf2=TRUE, ignoreExtinction = FALSE)
