#testing sim.map.period. some of the parameter and function names are subjectto change

#expected outputs: ~~~note red and blue are probably different. as of now it is yellow and magenta
#red(r) and blue(b)-
# rrrr
# bbrr
# bbrr
#only blue-
# bbbb
# bbbb
# bbbb
#warnings should be "Assuming divergance: no period found after 1128 iterations. Consider increasing initIter"

#test cases

#add range first, no defaults in sim (should give no warnigs about assuming divergence, red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
#m+paramrange(alim=2,blim=2,xlim=2,ylim=2,discretize = 1)
m+paramrange(alim=2,blim=2,xlim=3,ylim=3,discretize = .1)
m+sim.map.period(1,1,crop=TRUE)

#FIXME part of issue #139------- if model has a range, x/ylims dont get overidden. not that important.
#add range first, defaults in sim (should give warnigs about assuming divergence, only show blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(alim=2,blim=2,xlim=2,ylim=2,discretize = 1)
m+sim.map.period(1,1,alim=1,blim=1,xlim=Inf,ylim=Inf,discretize = .5)

#dont add range, no defaults in sim (should crash)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
#m+sim.map.period(1,1)

#dont add range, param defaults in sim, but no x,y defaults (red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+sim.map.period(1,1,alim=2,blim=2,discretize = 1)

#dont add range, param defaults and x,y defaults in sim (should give no warnigs about assuming divergence, red and blue)
#also no key for this one
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+sim.map.period(1,1,alim=2,blim=2, xlim=2,ylim=2,discretize = 1, crop = 3, key=FALSE)

f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}
model = dsmodel(f)
#add a range of parameters, set discretize, specify that I want to vary s and r
model + paramrange(3,3,discretize = .02, paramNames = c(s,r))
#generate an image based on periodicity tested at the point (.5,.5). Takes a bit of time.
#maxperiodicity=8 makes every periodicity above 8 count as divergent or 0.
model + sim.map.period(.5,.5,maxPeriod = 8, epsilon=.0001, iters = 1000, numTries = 1)
#varying only one variable can be done by using a dummy variable.
#create a model with the function
model = dsmodel(f)
#add a range and image. set blim very small because it dosent matter. we want to vary s and dummy
model + sim.map.period(.5,.5,alim=3,blim=.05, discretize=.05, paramNames=c(s,dummy), maxPeriod = 8)
