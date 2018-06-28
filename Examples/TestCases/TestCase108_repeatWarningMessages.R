
#test case for issue 108.
#should only give a single warning about 'model is potentially unstable' and potentialy
#other different warnings
m=dsmodel(function(x,y)list(10000*x,10000*y))+dsrange(10,10,discretize = .5)
m+simattractors(iters=Inf)
