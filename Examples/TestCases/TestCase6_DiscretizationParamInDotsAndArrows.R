library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)

# # Test case for issue #6 Discretization parameter in dots or arrows
model + dsrange(3,3, 0.01)
model + dsarrows(crop = TRUE, discretize = 0.9)
model + dsdots(discretize = 1)
model + dsdots(image = "red", discretize = 0.1)
model + dsarrows(col = "magenta", discretize = 2)
model + dsdots(image="orange", discretize = 2.5, size = 3)
