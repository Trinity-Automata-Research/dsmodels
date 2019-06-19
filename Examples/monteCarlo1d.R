#monte carlo in 1d

# the actual bifurcation points of the logistic map
actual= c(0,3,3.44948974278,3.54409035955192285361596598660480454058309984544457367545781253030584294285,3.5644072660954325977735575865289824,3.568750,3.56969,3.56989,3.569934,3.569943,3.5699451,3.569945557)

aMin=0
#aMax=4
aMax=3.569945672 #the start of chaos for logistic

testX=.5
maxP=5

f=function(x,a=.5){
  a*x*(1-x)
}

maxIts=1e6
#stride=2^20
tol=1.0e-8
#cTol=.Machine$double.eps^(2/3)


iter=function(x,a,its){
  for (i in 1:its){
    x=f(x,a)
  }
  x
}
#
# #test iter
# iter(x,a,its+1)
# for(i in 1:6-3){print(iter(x,a,its+i))}

eq=function(x1,x2,tol){
  abs(x1-x2)<tol
}

findPer=function(x,a, maxIts,tol,maxP){

  x=iter(x,a,maxIts)
  p=Inf
  for(i in 0:maxP){
    if(eq(x,iter(x,a,2^i),tol)){
      p=2^i
      break
    }
  }
  p
}

#counts of points in each interval to fieg
ratio=function(counts){
  l=length(counts)
 counts[-l]/counts[-1]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#uniform sampling
numPoints=10000
as <- seq(aMin,aMax, length.out = numPoints)
pers=mapply(findPer, a=as, MoreArgs = list(x=testX, maxIts=1e4, tol=tol, maxP=maxP))
print(pers)

counts=NULL
for(p in 0:maxP){
  counts[p+1]=sum(pers==2^p)
}
print(counts)
print(ratio(counts))

#varied discretization seems usefull-
#to get better numbers we want more points, but we dont need to recompute all the fixed ones
#recover what the last point before the first 2 was (the "safe" period 1 reigon) and  only look past that point
#repeat for last point before the first 4 and so on

#we need some way to remember which a had which period so we can find the "safe" period one reigon


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#random points


numPoints=10000
as <- runif(numPoints,aMin,aMax)
pers=mapply(findPer, a=as, MoreArgs = list(x=testX, maxIts=1e4, tol=tol, maxP=maxP))
print(pers)

counts=NULL
for(p in 0:maxP){
  counts[p+1]=sum(pers==2^p)
}
print(counts)
print(ratio(counts))








