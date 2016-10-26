library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)

# Test case for issue #53 Display = FALSE not working on points
model + dsrange(1,1,0.5)
model + dspoint(0,.5, label = "hi", display = FALSE)
model + dspoint(1,1, label = "Yay")
