library(dsmodels)
f<- function(x,y){
  list(atan(2*x-2.42920367323)+1.5,
       exp(0.8-x)*y+1)
}
#f<-function(x,y){c(x,y)}
mod<-dsmodel(f)
mod+dsrange(3,3,discretize = .4)
#print(mod$apply(.7145983,1,accum=FALSE,iters=400000,crop=FALSE))
mod+simattractors(discretize =3)

mod+dsarrows()
#moves very slowly to infinity, sim basins takes a very long time
mod+simbasins(discretize =.6)

