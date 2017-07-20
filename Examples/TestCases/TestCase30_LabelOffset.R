library(latex2exp)
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

dsmodel(fun) + dsrange(2,1.1, discretize = 1000) +
  dspoint(xstar, ystar, attractor = TRUE, col="green", label="$\\int_{0}^{\\infty}x^{x^2+1}$") +
  dspoint(1,1, label = "$\\alpha_{\\beta}$")
  guessregions(iters = 5000)
