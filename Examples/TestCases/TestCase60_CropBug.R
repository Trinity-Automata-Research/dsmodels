library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)

# Test case for issue #60 "Crop Bug"
model + dsrange(2,2, 0.03)
model + dsdots(image = c("red","green"))

