library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)

# # Test case for improvement #57 Xlim in curve
model + dsrange(2,2,0.09)
model + dscurve(
  function(x) 2-x^2,
  xlim = c(.3,1), image = "red", discretize = TRUE)
