#library(dsmodels)
rm(list=ls())


K <- 2.05
L <- 2.05
a <- 0.5
b <- 0.5

fun <- function(x,y) {
  xp <- x*exp(K-x-a*y)
  yp <- y*exp(L-y-b*x)
  list(xp,yp)
}

xstar = (K - a*L)/(1-a*b)
ystar = (L - b*K)/(1-a*b)

xlim = 5
ylim = xlim

model <- dsmodel(fun, title = "Hello, $\\LaTeX$") + dsrange(xlim,ylim, discretize = 1)
model + xlabel("$\\alpha$ vs $\\beta$", col = "green")
model + ylabel("$\\heartsuit$", col = "red")
model + dspoint(xlim/4,ylim/2, label="$x$")
model + dspoint(xlim/2,ylim/2, label="$x^{-x+1}$")
