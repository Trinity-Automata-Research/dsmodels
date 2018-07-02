library(dsmodels)

a=2

f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}

mod=dsmodel(f)

mod + paramrange(3,3,discretize = .1, paramNames = c(s,r),renderCount = 20)

#not parametric
c=dscurve(x*a,simPeriod = TRUE, find.period.args=list(numTries=5),col="black")
mod+c
c$narrow()

c1=dscurve(s,col ="red")
mod+c1

#crashes, cant find x, but should.
#if testing this case, you should clean the enviornment first.s
#c2=dscurve(x*s,col="blue")
#mod+c2
x=.8
#warning about varying s
c3=dscurve(x*s,col="magenta")
mod+c3
s=.6
#warning about varying s, dosent use the .6 value of s
c4=dscurve(x*s*2,col="green")
mod+c4

fooBar=.2
c5=dscurve(s*fooBar,col="orange")
mod+c5

c6=dscurve(1,col="purple")
mod+c6

c7=dscurve(function(a){1.5*a},col="yellow")
mod+c7

#If neitehr aname nor x are in the function, the formals are replaced with aname.
#in this case that results in and error.
c7=dscurve(function(barFoo){.3*barFoo},col="blue")
mod+c7

#parametric
c8=dscurve(t,t*t,tend=1.6)
mod+c8

#crashes, cant find fizzBuzz
#c9=dscurve(fizzBuzz*buzzFizz)
#mod+c9

