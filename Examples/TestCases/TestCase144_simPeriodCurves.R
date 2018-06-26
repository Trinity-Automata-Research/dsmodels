#param curve tester
f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}
model=dsmodel(f)
prange = paramrange(alim=3,blim=3,paramNames = c(s,r))
model + prange
c=dscurve(x/2,simPeriod = TRUE)
model+c
print(c$narrow(model))
print(c$phases(distances=TRUE))


#TODO: add parametric dscurve, test with different parameters
