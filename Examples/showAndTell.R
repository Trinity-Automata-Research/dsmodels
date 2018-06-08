#

f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}


#trying to replicate figure 1
#create a model with the function
model = dsmodel(f)
#add a range of parameters, set discretize, specify that I want to vary s and r
model + paramrange(3,3,discretize = .02, paramNames = c(s,r))
#generate an image based on periodicity tested at the point (.5,.5). Takes a bit of time.
#maxperiodicity=8 makes every periodicity above 8 count as divergent or 0.
model + sim.map.period(.5,.5,maxPeriod = 8, epsilon=.0001, initIters = 1000, numTries = 1)


#varying only one variable can be done by using a dummy variable.
#also see 1d-bifurcation
#create a model with the function
model = dsmodel(f)
#add a range and image. set blim very small because it dosent matter. we want to vary s and dummy
model + sim.map.period(.5,.5,alim=3,blim=.05, discretize=.05, paramNames=c(s,dummy), maxPeriod = 8)


#logistic competition model
logistic = function(x,y,a=3,b=3,c=.5,d=.5){
  list(x=(a*x*(1-x))/(1+c*y),
       y=(b*y*(1-y))/(1+d*x))
}




