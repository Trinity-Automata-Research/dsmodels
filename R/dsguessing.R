#' Guess regions associated with attractors.
#'
#' Attempts to guess which areas of the range are drawn to which attractors. The attractors
#' must be added to the model before \code{guessregions()} is added. The space is discretized
#' into squares, and
#' repeated iteration of the model's function is used to determine which attractor the middle
#' of each square tends towards. The square is then given the color of that attractor. The model
#' is assumed to be well behaved in that every point will eventually move within \code{epsilon}
#' of an attractor. \strong{There is absolutely no guarantee that all regions will be captured
#' by this approach}, even with a fine-grained discretization. It is possible to blur
#' boundaries, crop regions, or miss entire regions.
#'
#' All attractors should be \code{dspoints} with the \code{attractor} flag set to \code{TRUE},
#' and should already be composed with the model. Attractors may
#' have \code{display=FALSE} set to avoid displaying the attractor itself. Their color (or region
#' color, if defined) will be
#' used as the color of the region. If there are no points with the \code{attractor} flag set,
#' then all points are used as possible attractors. This is not recommended.
#'
#' If \code{iters} is not set, or is set to \code{NULL}, then each point will be individually
#' iterated until within \code{epsilon} distance of an attractor, or until it moves less
#' than \code{stable} between iterations. Points that stop moving further
#' than \code{epsilon} of an attractor, are colored \code{missingCol}, default
#' "lightgoldenrod3".
#'
#' If \code{iters} is given a numeric value, each point is iterated exactly \code{iters} time,
#' and the closest attractor is chosen regardless of distance. In this case, \code{epsilon} has
#' no effect. This will take bounded time, but may give a poorer guess.
#'
#' If \code{iters} is given an infinite value, the points are iterated until they move less than
#' \code{stable} distance. An attractor is chosen only if it  falls within \code{epsilon} distance,
#' otherwise the point is colored \code{missingCol}.
#'
#' The \code{image} function is used to display the results.
#'
#' @include dsproto.R
#' @param discretize The size of each square.  If not set, the discretization of the range
#'  is used. May be set separately from the discretization of the range without overwriting.
#' @param xlim The range of x values to calculate regions over. Defaults to the limits of the range.
#' @param ylim The range of y values to calculate regions over. Defaults to the limits of the range.
#' @param cols The colors to use for the various regions. The colors will be used in the order the attractors were added to the model.
#' @param iters If not set, each point will be iterated indvidually. If set as a number, exactly
#'  that many iterations will be used. If set as \code{Inf}, will iterate until points stabilize (see \code{stable}). See details.
#' @param epsilon When \code{iters} is not set, the distance at which a point is considered to
#'  have reached an attractor. Defaults to \code{discretize^2}.
#' @param stable A, usually smaller, distance at which a point is considered to have stopped moving. Defaults to \code{sqrt(.Machine$double.eps)}.
#' @param behind Forces this item to be a background object for the purposes of layering
#' @param missingCol When \code{iters} is not set, the color given to points that stop moving
#'  before reaching an attractor. Defaults to "lightgoldenrod3".
#' @param ... Extra graphical parameters for \code{image}.
#' @import graphics
#' @import grDevices
#' @seealso \code{\link{dspoint}}
#' @seealso \code{\link{dsregion}}
#' @seealso \code{\link{dspolygon}}
#' @seealso \code{\link{guessattractors}}
#' @examples
#' library(dsmodels)
#'
#' model <- dsmodel(function(X0,Y0) {
#' list(X0*exp(2.6-X0-6.45/(1+4.5*X0)),
#'      Y0*exp(2.6-Y0-0.15*X0-6.25/(1+4.5*Y0)))
#' })
#' model+dsrange(0:3,0:3,discretize = .08)+
#'       dspoint(1.9358, 1.5059, attractor=TRUE, col="green",  label = "K12")+
#'       dspoint(1.9358, 0,      attractor=TRUE, col="magenta",label = "K1")+
#'       dspoint(0,      1.9649, attractor=TRUE, col="orange", label = "K2")+
#'       dspoint(0,      0,      attractor=TRUE, col="blue",   display=FALSE)+
#'       dspoint(0.4419, 0.4416, col="yellow", label="A11")+
#'       guessregions(discretize=0.02)
#'@export
guessregions <- function(discretize=NULL, xlim=NULL, ylim=NULL, iters=NULL,
                         epsilon=NULL, behind=TRUE, stable=sqrt(.Machine$double.eps),  cols = NULL, missingCol="lightgoldenrod3",...) {
  if(is.null(epsilon))
    epsilon = NULL
  else
    epsilon = epsilon^2
  dsproto(
    `_class` = "image", `_inherit` = boolSelector(behind, background, feature),
    discretize=discretize, xlim=xlim, ylim=ylim,
    iters=iters,
    regionsCalculated = FALSE,
    grid = NULL,
    colMatrix = NULL,
    numCols = NULL,
    cols = cols,
    fps = NULL,
    epsilon = epsilon,
    stable = stable,
    X0 = NULL,
    Y0 = NULL,
    missingCol = missingCol,
     ... = ...,
    bindWithModel = function(self, model) {
      attractors <- Filter(
        function(x) {
          if(is.dspoint(x))
            x$attractor
          else
            FALSE
        }, model$feature)
      if(!is.range(model$range)) {
        stop("Can only guess a point's region on a model defined in a range")
      }
      if(length(attractors) == 0){
        attractors <- Filter(function(x) {is.dspoint(x)}, model$feature)
        warning("No points are specified as attractors, which may result in unintended behavior when guessing regions.")
      }
      self$fps <- pointsToList(attractors)
      self$numCols <- length(self$fps$col)
      if(is.null(self$cols)) {
        self$cols <- c(self$missingCol, self$fps$col)
      } else {
        if(length(self$cols) < length(self$fps$col))
        {
          warning(paste("Guessregions: number of colors (", length(self$cols), ") provided is smaller than number of attractors (", length(self$fps$col), "). Using attractor colors."))
          self$cols <- c(self$missingCol, self$fps$col)
        } else {
          self$cols <- c(self$missingCol, self$cols)
        }
      }
      if(is.null(self$discretize))
        self$discretize <- model$range$discretize
      if(is.null(self$epsilon))
        self$epsilon <- (self$discretize)^2 #May as well work in squared distances.
      if(is.null(self$xlim))
        self$xlim <- model$range$xlim
      if(is.null(self$ylim))
        self$ylim <- model$range$ylim
      if(is.null(self$grid)) {
        mx = seq(min(self$xlim)+(self$discretize/2),max(self$xlim), by = self$discretize)
        my = seq(min(self$ylim)+(self$discretize/2),max(self$ylim), by = self$discretize)
        self$grid <- list(x=mx, y=my)
        N = as.matrix(expand.grid(mx,my))
        self$X0 = N[,1]
        self$Y0 = N[,2]
      }
    },

    recalculate = function(self, model) {
      self$bindWithModel(model)
      if(is.null(self$iters)) {
        colsMap <- mapply(findFixedPoint, self$X0, self$Y0,
                       MoreArgs=list(fun=model$fun, points=self$fps, stable=self$stable, eps=self$epsilon))
      } else if (is.infinite(iters)) {
        images <- applyTillFixed(model, self$X0, self$Y0, 5, self$stable)
        colsMap <- mapply(findNearestPoint, images$x, images$y,
                          MoreArgs=list(points=self$fps, eps=0, deep=FALSE))
      }
      else {
        images <- model$apply(self$X0, self$Y0, self$iters, crop=FALSE, accumulate=FALSE)
        colsMap <- mapply(findNearestPoint, images$x, images$y,
                       MoreArgs=list(points=self$fps, eps=self$epsilon))
      }
      self$colMatrix <- matrix(unlist(colsMap), nrow=length(self$grid$x))
      self$regionsCalculated <- TRUE
    },

    render = function(self, model) {
      if(!self$regionsCalculated)
        self$recalculate(model)
      image(x=self$grid$x, y=self$grid$y, z=self$colMatrix,
            zlim=c(0,self$numCols), #length($cols)),
            col=self$fps$col,
            add=TRUE)#, ... = self$...)
    }
  )
}


#' Reports whether x is a dsimage
#' @param x An object to test.
# @rdname guessregions
#' @keywords internal
#' @export
is.dsimage <- function(x) inherits(x,"image")

#' Guess the attractors of a model.
#'
#' Attempts to guess the attractors of a model. The space is discretized into initial points, and
#' repeated iteration of the model's function is used to guess the attractors. It is possible
#' that non-attractor fixpoints will be found by accident. The function
#' is iterated until the points move less than \code{stable} (default
#' \code{sqrt(.Machine$double.eps)}) between iterations.
#' The color of each point is drawn from the \code{col} parameter. If the number of poitns exceeds
#' the size of $\code{col}$, or $\code{col}$ is not defined, then reasonable defaults are used instead.
#' The attractors are \code{dspoint}s that are added to the model.
#'
#' @include dsproto.R dspoint.R
#' @param discretize The space between initial points. If not set, the discretization of the range
#'  is used. May be set separately from the discretization of the range without overwriting.
#' @param xlim The range of x values to search for attractors. Defaults to the limits of the range.
#' @param ylim The range of y values to search for attractors. Defaults to the limits of the range.
#' @param iters The number of times the function is applied before checking the distance points have moved. Default 10.
#' @param epsilon The distance at which two points are considered to be the same attractor. Defaults to \code{discretize^2}.
#' @param stable A, usually smaller, distance at which a point is considered to have stopped moving. Defaults to \code{sqrt(.Machine$double.eps)}.
#' @param cols The colors of the attractors. If insufficient not provided, reasonable defaults are used. Generally (but not always) proceeds left to right, then bottom to top.
#' @import graphics
#' @seealso \code{\link{dspoint}}
#' @seealso \code{\link{dsregion}}
#' @seealso \code{\link{dspolygon}}
#' @seealso \code{\link{guessregions}}
#' @examples
#' model <- dsmodel(function(X0,Y0) {
#' list(X0*exp(2.6-X0-6.45/(1+4.5*X0)),
#'      Y0*exp(2.6-Y0-0.15*X0-6.25/(1+4.5*Y0)))
#' })
#'
#' model + dsrange(5,5,0.09) + guessattractors(discretize=0.02)
#'@export
guessattractors <- function(discretize=NULL, xlim=NULL, ylim=NULL, iters=10,
                         epsilon=NULL, stable=sqrt(.Machine$double.eps), cols= NULL){
  epsilon <- boolSelector(is.null(epsilon), NULL, epsilon^2)
  dsproto(
    `_class` = "guessattractors", `_inherit` = background,
    discretize=discretize, xlim=xlim, ylim=ylim,
    iters=iters,
    attractorsCalculated = FALSE,
    attractors = list(),
    attractorCoords = NULL,
    grid = NULL,
    cols = cols,
    epsilon = epsilon,
    stable=stable,
    X0 = NULL,
    Y0 = NULL,
    attractors = NULL,
    bindWithModel = function(self, model) {
      if(!is.range(model$range)) {
        stop("Can only guess attractors within a defined range.")
      }
      if(is.null(self$discretize))
        if(!is.null(model$range) && !is.null(model$range$discretize))
          self$discretize <- model$range$discretize
        else
          stop("Need a discretized range or an epsilon to find attractors.")
      if(is.null(self$epsilon))
        self$epsilon <- (self$discretize)^2 #May as well work in squared distances.
      if(is.null(self$xlim))
        self$xlim <- model$range$xlim
      if(is.null(self$ylim))
        self$ylim <- model$range$ylim
      if(is.null(self$grid)) {
        mx = seq(min(self$xlim),max(self$xlim), by = self$discretize)
        my = seq(min(self$ylim),max(self$ylim), by = self$discretize)
        self$grid <- list(x=mx, y=my)
        N = as.matrix(expand.grid(mx,my))
        self$X0 = N[,1]
        self$Y0 = N[,2]
      }
    },

    recalculate = function(self, model) {
      if(self$attractorsCalculated)
        warning("Guessing attractors twice: this may result in duplicate points.")
      self$bindWithModel(model)
      moved <- TRUE
      images <- applyTillFixed(model, self$X0, self$Y0, self$iters, self$stable)
      found <- 1
      points <- mapply(c,images$x, images$y, SIMPLIFY=FALSE)
      attractorCoords <- list(x=c(images$x[1]), y=c(images$y[1]))
      for(p in points) {
        dists  <- (attractorCoords$x - p[1])^2 + (attractorCoords$y-p[2])^2
        if(min(dists) > self$epsilon) {
          found <- found + 1
          attractorCoords$x[found] <- p[1]
          attractorCoords$y[found] <- p[2]
        }
      }
      if(length(self$cols) < found) {
        if (found <= 6)
          self$cols <- c("yellow", "magenta", "orange", "green", "red", "blue")
        else if (found <= 28)
          self$cols <- c("#00119c","#cdff50","#8d00a9","#00b054","#ff40dd","#01f9be","#ff1287","#2a73ff","#d99b00","#f5ff84","#3e004a","#91fffa","#ff455a","#00a5f3","#850f00","#9897ff","#0e2100","#e2b5ff","#005238","#ffa287","#12002c","#e2ffe0","#620045","#ffd3e1","#2b0a00","#0068b0","#5f1800","#00376f")
        else
          self$cols <- rainbow(found)
      }
      self$attractorCoords <- attractorCoords
      self$attractorsCalculated <- TRUE
      for(i in seq_len(found)) {
        point = dspoint(attractorCoords$x[i], attractorCoords$y[i], artificial=TRUE, col=self$cols[[i]], attractor=TRUE)
        self$attractors[[i]] <- point
        model+point
      }
    },
    render = function(self, model) {
      if(!self$attractorsCalculated)
        self$recalculate(model)
    }
  )
}

applyTillFixed <- function(model, x, y, iterStep, stable) {
  moved <- TRUE
  prev <- list(x=x, y=y)
  iters = 0
  while(moved && iters < 1000000000000000000) {
    images <- model$apply(prev$x, prev$y, iterStep, accumulate=FALSE, crop=FALSE)
    dists = (images$x - prev$x)^2 + (images$y - prev$y)^2
    m <- max(dists)
    if(is.nan(m)){
      stop("dsguessing: Model not well defined: NaN")
    }
    if (max(dists) < stable) {
      moved <- FALSE
    }
    else {
      prev <- images
      iters <- iters+1
    }
  }
  images
}

boolSelector <- function(b, t, f) { if(b) t else f }

