library(dsmodels)

func <- function(x,y) {
  xp = y-y^3
  yp = -x-y^2
  list(xp,yp)
}

model <- dsmodel(fun = func)

field <- dsrange(-3:3, -3:3, discretize = .5)
model + field
#model + dsarrows(head.length = .15, col = "black")
model + dsdots(col = "black", image = "magenta", iters = 1)
