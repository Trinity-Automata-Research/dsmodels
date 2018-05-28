#tests everything that uses range$corners or range$centers

f<-function(x,y) list(x=x*1.1, y=y*1.1)
m<-dsmodel(f)
m+dsrange(2,2,discretize = 1)
m+dsarrows(discretize = .2)
m+simattractors(initIters=100, discretize = .1)
m+simbasins(iters=Inf, discretize=.5)
m+dspoint(1,1,"hi")
m+dsdots(iters=6, col=rainbow(7))
m+dsdots(discretize = .2)


