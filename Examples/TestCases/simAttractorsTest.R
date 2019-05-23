library(dsmodels)

r1 <- 2.6
r2 <- 2.6
m1 <- 6.45
m2 <- 6.25
s1 <- 4.5
s2 <- 4.5
b <- 0.15

#The two-dimenstional model expects a two-dimensional function that outputs a list of the x and y value(s).
def <- function(X0,Y0) {
  xp <- X0*exp(r1-X0-m1/(1+s1*X0))
  yp <- Y0*exp(r2-Y0-b*X0-m2/(1+s2*Y0))
  list(xp,yp)
}

#A model contains a function and a title.
model <- dsmodel(fun = def, title="Four interior fixed points")

#The field is the graph area. We could change the name if you like, for instance to dsplot
field <- dsrange(0:3,0:3,discretize = .1)

#By default the arrows will be scaled to the discretization parameter, and blue.
#They can be set manually with parameters.
model+field
model + dsarrows(head.length=0.15, discretize=.2)
model + simattractors(discretize=0.05) + simbasins(discretize=0.02)
