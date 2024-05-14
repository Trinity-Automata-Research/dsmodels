#Tests the functionality of sim.is.stable. Expected output is: [1] TRUE [1] FALSE
#gives 50 or more warnings. simattractors gives a warning when a point excedes the max value and becomes nan
library(dsmodels)

#stable model
f <-function(x,y){list(x*.9,y*.9)}

model <- dsmodel(f)
model + dsrange(1,1,discretize=.05)
model + simattractors()
model +dsarrows(discretize=.2)
model + simbasins(iters=1000)
print(model$sim.is.stable())

#unstable model
g <-function(x,y){list(x*1.01,y*1.01)}

model <- dsmodel(g)
model + dsrange(2,2,discretize=.05)
model +dsarrows(discretize=.2)
model + simattractors(iters=1000)
model + simbasins(iters=1000)
print(model$sim.is.stable())
