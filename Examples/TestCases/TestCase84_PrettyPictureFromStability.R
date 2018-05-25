#creates an image based on the value of sim.is.stable.
#each point, (a,b), has a color determined by the stability of a model with function f(a,b)
#gives 50 or more warnings. simattractors gives a warning when a point excedes the max value and becomes nan

f = function(a=.5,b=.5){
  function(x,y){
    list(x=a*x,y=b*y)
    #list(x=(a*b)*x,y=(a*b)*y)
    #list(x=(a*b+.2)*x,y=((a+.2)*b)*y)
  }
}
withLoop=FALSE #change the value of this to switch between for loops and mapply
amin=0
amax=2
bmin=0
bmax=2
discretization=.1

as=seq(amin,amax,by=discretization)
bs=seq(bmin,bmax,by=discretization)
points=expand.grid(a=as,b=bs)

testPoint = function(a,b){
  m<-dsmodel(f(a,b),display = FALSE)
  m+dsrange(2,2,discretize = .4)+simattractors(iter=1000)+simbasins(iters=10)
  if(m$sim.is.stable()){1}
  else{0}
}

if(withLoop){
  size=length(points[,1])
  z=seq(1,length=size)
  for(i in 1:length(points[,1])){
    z[i]=testPoint(points[i,1],points[i,2])
  }
} else{
  z=mapply(testPoint,points[,1],points[,2])
}

z=mapply(testPoint,points[,1],points[,2])
z=matrix(z,length(as))


image(as,bs, z,  col=c("blue","green"))
