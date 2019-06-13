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
