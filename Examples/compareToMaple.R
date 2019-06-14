#500 ir 1000 its is not enough
#try 200000 or something?

f=function(x,a) a*x*(1-x)

x=.5
a=1
its=1000
tol=1.1e-8
narrowtol=1.1e-8

iter=function(x,a,its){
  for (i in 1:its){
    x=f(x,a)
  }
  x
}

#test iter
iter(x,a,its+1)
for(i in 1:6-3){print(iter(x,a,its+i))}

eq=function(x1,x2,tol){
  (x1-x2)^2<tol
}

findPer=function(x,a,its,tol,maxP){
  n=iter(x,a,its)
  p=Inf
  for(i in 0:maxP){
    if(eq(n,iter(n,a,2^i),tol)){
      p=2^i
      break
    }
  }
  p
}


#test findPer
findPer(x,a,its,tol,5)
for(i in seq(2.5,3.7,by=.01)){
  print(paste(i," ", findPer(x,i,its,tol,5)))
}



narrow=function(starta,startp,enda,endp, narrowTol){
  if(abs(starta-enda)<narrowTol){
    c(starta,enda)
  }else{
    mid=(starta+enda)/2
    midp=findPer(x,mid,its,tol,5)
    if(midp==startp){
      narrow(mid,startp,enda,endp,narrowTol)
    }else{
      narrow(starta,startp,mid,endp,narrowTol)
    }
  }
}

print(narrow(2.9,1,3.1,2,narrowTol),digits=15)
print(narrow(3.4,2,3.5,4,narrowTol),digits=15)
print(narrow(3.53,4,3.55,8,narrowTol),digits=15)
print(narrow(3.55,8,3.566,16,narrowTol),digits=15)
print(narrow(3.566,16,3.569,32,narrowTol),digits=15)

# with a=1, should converge to 0   this is close to a critical point
iter(x,a,its)
iter(x,a,its*5)
iter(x,a,its*10)
orbit=iter(x,a,100000)

for(i in 1:20){
  print(orbit)
  orbit=iter(orbit,a,1)
}

# with a=2, should converge to .5   this is far from a critical point
a=2
iter(x,a,its)
iter(x,a,its*5)
iter(x,a,its*10)
orbit=iter(x,a,100000)

for(i in 1:20){
  print(orbit)
  orbit=iter(orbit,a,1)
}


# with a=3, should converge to 2/3   this is a critical point
a=3
iter(x,a,its)
iter(x,a,its*5)
iter(x,a,its*10)
orbit=iter(x,a,100000)

for(i in 1:20){
  print(orbit,digits=15)
  orbit=iter(orbit,a,1)
}


#go out way further-  1 billion iterations. this isnt far enough?
#who cares about rounding error- all points converge to the same orbit
#we need a way to tell the difference between eventually fixed and periodic
#without iterating untill its fixed
orbit=iter(x,a,1000000000)
longOrbit=orbit
for(i in 1:20){
  print(orbit,digits=15)
  orbit=iter(orbit,a,1)
}
#1bil more
orbit=iter(orbit,a,1000000000)



a=2.999
iter(x,a,its)
iter(x,a,its*5)
iter(x,a,its*10)
orbit=iter(x,a,100000)
print(orbit,digits=20)

for(i in 1:20){
  print(orbit,digits=15)
  orbit=iter(orbit,a,1)
}


a=3.000001
iter(x,a,its)
iter(x,a,its*5)
iter(x,a,its*10)
orbit=iter(x,a,100000)
print(orbit,digits=20)

for(i in 1:20){
  print(orbit,digits=15)
  orbit=iter(orbit,a,1)
}



#for a's close to 3, how fast does a converge
a=3
its=10000000
mu=a
orbit=iter(x,mu,its)
orbit2=iter(orbit,mu,2)
# print(orbit,digits=15)
#  print(orbit2,digits=15)
print(round(log(abs(orbit-orbit2),10)))


for(i in 1:8){
  #print(a-.1^i,digits=15)
  mu=a-.1^i
  orbit=iter(x,mu,its)
  orbit2=iter(orbit,mu,2)
 # print(orbit,digits=15)
#  print(orbit2,digits=15)
    print(round(log(abs(orbit-orbit2),10)))
}
