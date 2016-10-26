library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)
# Test case for issue #47 "Arrow Iterations"
f <- function(x,y) list(-2*x,y)
model <- dsmodel(f)
model + dsrange(3,3,.1)
model + dsarrows(iters = 9)
