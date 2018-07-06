#test case for #176


model=dsmodel(function(x,y)list(1,1))
model+dsrange(1,1)
#fails, needs discretize, leaves an unbound "ghost" simattractors in model$facade
try(model+simattractors())
model+simattractors(discretize = 1)
#throws a stop "attempting to render dsmodels object before it is bound. Please notify developers."
#because model tries to redisplay everything, including the unbound simattractors.
model+simbasins(discretize = 1)
