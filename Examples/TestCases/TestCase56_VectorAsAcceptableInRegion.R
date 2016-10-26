library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)

# # Test case for issue #56 c(x,y) instead of point for ddsregion
model + dsrange(3,3,.09)
model + dsregion(c(0,0),c(1,0),c(1,1),c(0,1))
