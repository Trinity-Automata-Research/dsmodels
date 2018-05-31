#uses some functions from converge testing. probably need to run converge testing first

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
stabilityPic=FALSE
if(stabilityPic){
  print("making stability picture")

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
}

bifuricationPic=TRUE
if(bifuricationPic){
  print("making bifurification picture")

  amin=2
  amax=4
  bmin=0
  bmax=.02
  discretization=.02
  bmax=discretization
  as=seq(amin,amax,by=discretization)
  bs=seq(bmin,bmax,by=discretization)
  points=expand.grid(a=as,b=bs)

  g = function(a=.5,b=.5){
    function(x,y){
      list(x=a*x*(1-x),y=y)
    }
  }


  countPoint=function(a,b){
    m<-dsmodel(g(a,b),display = FALSE)
    m+dsrange(2,2,discretize = .4)
    period=find.period(m,.5,.5,numTries = 3,epsilon=.001)
    if(FALSE){ #should I count fixedPoints+period, or just period
      if(period==1)
        period=0
      m+simattractors(discretize = .4)
      ength(m$points()$x)+period
    }
    else
      period+1
  }

  z=mapply(countPoint,points[,1],points[,2])
  numCol=max(z) #length(unique(z))
  cols=rainbow(numCol)
  z=matrix(z,length(as))
  image(as,bs, z,  col=cols)
}

simpleBi=FALSE
if(simpleBi){
  Sys.sleep(5)
  print("making second bifurication picture")
  amin=0
  amax=2
  bmin=0
  bmax=2
  discretization=.5
  as=seq(amin,amax,by=discretization)
  bs=seq(bmin,bmax,by=discretization)
  points=expand.grid(a=as,b=bs)

  g = function(a=.5,b=.5){
    function(x,y){
      list(x=3-a*b*x,y=y)
    }
  }


  countPoint=function(a,b){
    m<-dsmodel(g(a,b),display = FALSE)
    m+dsrange(2,2,discretize = .4)
    period=find.period(m,.5,.5,numTries = 3,epsilon=.001)
    if(FALSE){ #should I count fixedPoints+period, or just period
      if(period==1)
        period=0
      m+simattractors(discretize = .4)
      length(m$points()$x)+period
    }
    else
      period
  }

  z=mapply(countPoint,points[,1],points[,2])
  numCol=length(unique(z))
  cols=rainbow(numCol)
  z=matrix(z,length(as))
  image(as,bs, z,  col=cols)

}
