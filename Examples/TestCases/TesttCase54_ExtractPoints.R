#library(dsmodels)


#The two-dimenstional model expects a two-dimensional function that outputs a list of the x and y value(s).
def <- function(X0,Y0) {
  xp <- X0*exp(2.6-X0-6.45/(1+4.5*X0))
  yp <- Y0*exp(2.6-Y0-0.15*X0-6.25/(1+4.5*Y0))
  list(xp,yp)
}

#A model contains a function and a title.
model <- dsmodel(fun = def, title="Four interior fixed points")

#The field is the graph area. We could change the name if you like, for instance to dsplot
field <- dsrange(0:3,0:3,discretize = .1)

#By default the arrows will be scaled to the discretization parameter, and blue.
#They can be set manually with parameters.
model+field
#model + dsarrows(head.length=0.15)
model + simattractors(discretize=0.05)
model + dspoint(1,1,col="darkblue")
roundPairs = function(lst) Map(function(p) c(round(p[[1]], 3), round(p[[2]], 3)), lst)
pairs = roundPairs(model$points("pairs"))
stopifnot(setequal(pairs, list(c(0,0), c(1.936, 0), c(0,1.965), c(1.936, 1.506), c(1,1))))
apairs = roundPairs(model$points("pairs","attractor"))
stopifnot(setequal(apairs, list(c(0,0), c(1.936, 0), c(0,1.965), c(1.936, 1.506))))

