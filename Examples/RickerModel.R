# user defines K, L, a, b, R
library(dsmodels)

K <- 1.9
L <- 1.7
a <- 0.5
b <- 0.5

functions <- function(x,y) {
  xp <- x*exp(K-x-a*y)
  yp <- y*exp(L-y-b*x)
  list(xp,yp)
}

model <- dsmodel(functions, title = "2D Ricker Model") # competition model

#define the size of the rendering field. Field may not be the best name, if you can think of a better one.
xlim <- max(1,exp(K-1),exp(L-1))
ylim <- xlim
field <- dsrange(xlim, ylim)


#This doesn't draw anything yet, just sets up the field.
model + field

#The two critical curves: it looks like your x and y were flipped in the original code? Or maple is backwards? Who knows.
cl_1y <- function(t) t
cl_1x <- function(t) (1-t)/((1-t)+a*b*t)
cl_2y <- function(t) 1/((1-a*b)*t)
cl_2x <- function(t) ((1-a*b)*t-1)/((1-a*b)*(t-1))

#draw the line in red, and the image of the line under the model in blue
model + dscurve(cl_2x, cl_2y, col="pink", image = "blue", discretize = TRUE)

#We can also define the curves separately and add them to the model.
#In thise case, we decided to draw the line with 300 points so it looks better.
cl1_crv <- dscurve(cl_1x, cl_1y, col="red", image = "green", n = 300, discretize = TRUE)
model + cl1_crv

#non-parameterized curves are also supported. we're working on being able to leave out the function(x) part
model + dscurve(function(x) -b*x+L, col="black")
model + dscurve(function(x) (-x+K)/a, col="black")

#Add single points and observe their image
#Each shade further from magenta and closer to black represents
# a later iteration from the original magenta point.
model + dspoint(1,1, col = "magenta", image = "black", iters = 5)
