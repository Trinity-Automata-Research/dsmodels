library(dsmodels)
f <- function(x,y) {
  list(
    x = 1/((x+1)*(y+1)),
    y = x/(y+1))
}
model <- dsmodel(f)

# Test case for issue #25 axes labels objects
model +dsrange(5,5,.5)
model + xlabel("This is the x-axis") + ylabel("This is the y-axis")
