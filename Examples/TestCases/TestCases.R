library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)
# # Test case for issue #47 "Arrow Iterations"
# f <- function(x,y) list(-2*x,y)
# model <- dsmodel(f)
# model + dsrange(3,3,.1)
# model + dsarrows(iters = 9)

# # Test case for issue #60 "Crop Bug"
# model + dsrange(2,2, 0.03)
# model + dsdots(image = c("red","green"))

# model + dsrange(2,2,0.09)
# model + dsdots(image = c("green", "red"), crop = FALSE)
# model + dscurve(
#   function(x) 2-x^2,
#   discretize = TRUE, image = c("green"), iters = 5
# )

# # Test case for issue #56 c(x,y) instead of point for ddsregion
# model + dsrange(3,3,.09)
# model + dsregion(c(0,0),c(1,0),c(1,1),c(0,1))

# # # Test case for improvement #57 Xlim in curve
# model + dsrange(2,2,0.09)
# model + dscurve(
#   function(x) 2-x^2,
#   xlim = c(.3,1), image = "red", discretize = TRUE)

# # Test case for issue #53 Display = FALSE not working on points
# model + dsrange(1,1,0.5)
# model + dspoint(0,.5, label = "hi", display = FALSE)

# # # Test case for issue #6 Discretization parameter in dots or arrows
model + dsrange(3,3)
model + dsarrows(discretize = 1)
model + dsdots(discretize = 1)
model + dsdots(image = "red", discretize = 0.1)
model + dsarrows(col = "magenta", discretize = 2)
model + dsdots(image="orange", discretize = 2.5, size = 3)

# # # Test case for issue #25 axes labels objects
# model +dsrange(5,5,.5)
# model + xlabel("This is the x-axis") + ylabel("This is the y-axis")
