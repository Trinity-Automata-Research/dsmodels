

#' Determine the attractors of a model through simulation
#'
#' Attempts to determine the attractors of a model. The space is discretized into initial points, and
#' repeated iteration of the model's function is used to approximate the attractors. It is possible
#' that non-attractor fixpoints will be found by accident. The function
#' is iterated until the points move less than \code{tolerance} (default
#' \code{sqrt(.Machine$double.eps)}) between iterations.
#' The color of each point is drawn from the \code{col} parameter. If the number of points exceeds
#' the size of \code{col}, or \code{col} is not defined, then reasonable defaults are used instead.
#' The attractors are \code{dspoint}s that are added to the model.
#'
#' @include dsproto.R dspoint.R
#' @param discretize The space between initial points. If not set, the discretization of the range
#'  is used. May be set separately from the discretization of the range without overwriting.
#' @param xlim The range of x values to search for attractors. Defaults to the limits of the range.
#' @param ylim The range of y values to search for attractors. Defaults to the limits of the range.
#' @param iters The maximum number of iterations to use. Points that still move greater than
#'  \code{tolerance} will result in a warning and will be discarded. Can be disabled by setting to
#'  \code{Inf} or 0. Default \code{1e+18}.
#' @param stride The number of times the function is applied before movement is checked: in essence
#'  finding the attractors for \code{f^stride}. For non-periodic dynamical systems, this is merely
#'  an efficiency concern. For systems with periodic attractors with a period that is a factor of
#'  that is a factor of \code{stride}, this may identify each of the points in the orbit. A warning
#'  will be issued if a point is only stable in \code{f^stride}, and not in \code{f}. Default 8.
#' @param epsilon The distance at which two points are considered to be the same attractor. Defaults to \code{discretize^2}.
#' @param tolerance A, usually smaller, distance at which a point is considered to have stopped moving. Defaults to \code{sqrt(.Machine$double.eps)}.
#' @param cols The colors of the attractors. If insufficient not provided, reasonable defaults are used. Generally (but not always) proceeds left to right, then bottom to top.
#' @import graphics
#' @seealso \code{\link{dspoint}}
#' @seealso \code{\link{dsregion}}
#' @seealso \code{\link{dspolygon}}
#' @seealso \code{\link{simbasins}}
#' @examples
#' model <- dsmodel(function(X0,Y0) {
#' list(X0*exp(2.6-X0-6.45/(1+4.5*X0)),
#'      Y0*exp(2.6-Y0-0.15*X0-6.25/(1+4.5*Y0)))
#' })
#'
#' model + dsrange(5,5,0.09) + simattractors(discretize=0.02)
#'@export
simattractors <- function(discretize=NULL, xlim=NULL, ylim=NULL, stride=8, iters=1e+18, initIters=0,
                         epsilon=NULL, tolerance=sqrt(.Machine$double.eps), cols= NULL){
  epsilon <- boolSelector(is.null(epsilon), NULL, epsilon^2)
  dsproto(
    `_class` = "simattractors", `_inherit` = facade,
    discretize=discretize, xlim=xlim, ylim=ylim,
    stride=stride,
    iters = iters,
    initIters=initIters,
    bound = FALSE,
    attractors = list(),
    attractorCoords = NULL,
    cols = cols,
    epsilon = epsilon,
    tolerance=tolerance,
    grid = NULL,
    attractors = NULL,
    on.bind = function(self, model) {
      self$grid=model$range$corners(discretize=self$discretize, xlim=self$xlim, ylim=self$ylim)
      if(self$iters == 0)
        self$iters = Inf
      if(is.null(self$epsilon)){
        self$epsilon <- (model$range$getDiscretize(self$discretize))^2
      }
      if(self$bound){
        warning("Determining attractors twice: this may result in duplicate points.")
      }
      self$bound <- TRUE
      self$calculate.attractors(model)
    },
    calculate.attractors = function(self, model) {
      moved <- TRUE
      images <- applyTillFixed(model, self$grid$X0, self$grid$Y0, self$stride, self$iters, self$initIters, self$tolerance)
      found <- 1
      points <- mapply(c,images$x, images$y, SIMPLIFY=FALSE)
      attractorCoords <- list(x=c(images$x[1]), y=c(images$y[1]))
      discardFlag=FALSE
      for(p in points) {
        dists  <- (attractorCoords$x - p[1])^2 + (attractorCoords$y-p[2])^2
        if(min(dists) > self$epsilon) {
          pointImage = model$apply(p[1],p[2], iters=self$stride, accumulate=FALSE, crop=FALSE)
          imageDist = (pointImage$x - p[1])^2 + (pointImage$y - p[2])^2
          if(imageDist < self$tolerance) {
            found <- found + 1
            attractorCoords$x[found] <- p[1]
            attractorCoords$y[found] <- p[2]
          }
          else {
            discardFlag=TRUE
          }
        }
      }
      if(discardFlag)
          warning("simattractors: non-fixed point(s) discarded due to maximum iteration.")
      if(length(self$cols) < found) {
        if (found <= 6)
          self$cols <- c("yellow", "magenta", "orange", "green", "red", "blue")
        else if (found <= 28)
          self$cols <- c("#00119c","#cdff50","#8d00a9","#00b054","#ff40dd","#01f9be","#ff1287","#2a73ff","#d99b00","#f5ff84","#3e004a","#91fffa","#ff455a","#00a5f3","#850f00","#9897ff","#0e2100","#e2b5ff","#005238","#ffa287","#12002c","#e2ffe0","#620045","#ffd3e1","#2b0a00","#0068b0","#5f1800","#00376f")
        else
          self$cols <- rainbow(found)
      }
      self$attractorCoords <- attractorCoords
      for(i in seq_len(found)) {
        point = dspoint(attractorCoords$x[i], attractorCoords$y[i], artificial=TRUE, col=self$cols[[i]], attractor=TRUE)
        self$attractors[[i]] <- point
        model+point
      }
    }
  )
}

applyTillFixed <- function(model, x, y, stride, maxIters, initIters=0, tolerance) {
  moved <- TRUE
  prev <- list(x=x, y=y)
  iters = 0
  if(initIters>0){
    prev <- model$apply(prev$x, prev$y, iters=initIters, accumulate=FALSE, crop=FALSE)
  }
  unstableFlag=FALSE
  while(moved && iters < maxIters) {
    images <- model$apply(prev$x, prev$y, iters=stride, accumulate=FALSE, crop=FALSE) #crop=TRUE?
    if (length(prev$x) != length(images$x)){
    }
    dists = (images$x - prev$x)^2 + (images$y - prev$y)^2
    m <- max(dists[is.finite(dists)])
    if(is.nan(m)){
      stop("dssimulation: Model not well defined: NaN")
    }
    if (m < tolerance) {
      moved <- FALSE
      if(!all(is.finite(dists))){
        unstableFlag=TRUE
      }
    }
    else {
      prev <- images
      iters <- iters+1 #should probably be +stride. Issue #104
    }
  }
  if(iters == maxIters && moved)
    warning("dssimulation: hit iteration threshhold in simattractors.")
  if(!moved) {
    noStrideImages = model$apply(images$x, images$y, iters=1, accumulate=FALSE, crop=FALSE)
    dists = (noStrideImages$x - images$x)^2 + (noStrideImages$y - images$y)^2
    if(!all(is.finite(dists))){
      unstableFlag=TRUE
    }
    m <- max(dists[is.finite(dists)])
    if (m > tolerance) {
      warning("dssimulation: points are only stable under stride, may have periodic attractors.")
      #tolernace here is the wrong parameter, because we're moving once instead of 8 times.
      #dividing by 8 is probably better(?), but maybe even as extreme as square-rooting.
      #ISSUE number 102
    }
  }
  if(unstableFlag)
    warning("dssimulation: Model is potentially unstable. Points move very large distances.")
  x <- images$x
  y <- images$y
  valids <- is.finite(x) & is.finite(y)
  res <- list(x=x[valids], y=y[valids])
  res
}

boolSelector <- function(b, t, f) { if(b) t else f }

