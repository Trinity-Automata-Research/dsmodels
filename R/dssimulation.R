#' Find basins of attraction by simulation
#'
#' Attempts to determine which areas of the range are drawn to which attractors by simulation.
#' The attractors must be added to the model before \code{simbasins()} is added. The space
#' is discretized into squares, and
#' repeated iteration of the model's function is used to determine which attractor the middle
#' of each square tends towards. The square is then given the color of that attractor. The model
#' is assumed to be well behaved in that every point will eventually move within \code{epsilon}
#' of an attractor. \strong{There is absolutely no guarantee that all basins will be captured
#' by this approach}, even with a fine-grained discretization. It is possible to blur
#' boundaries, crop basins, or miss entire regions.
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
#' than \code{tolerance} between iterations. Points that stop moving further
#' than \code{epsilon} of an attractor, are colored \code{missingCol}, default
#' "NA".
#'
#' If \code{iters} is given a numeric value, each point is iterated exactly \code{iters} times, and \code{tolerance} will have no effect.
#' If the final image is within \code{epsilon} of an attractor, then the square is colored appropriately.
#' If not, then the point is colored \code{missingCol}, default "NA".
#' This will take bounded time, but may give a poorer result.
#'
#' If \code{iters} is given an infinite value, the points are iterated until they move less than
#' \code{tolerance} distance. An attractor is chosen only if it  falls within \code{epsilon} distance,
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
#'  that many iterations will be used. If set as \code{Inf}, will iterate until points stabilize (see \code{tolerance}). See details.
#' @param epsilon The distance at which a point is considered to have reached an attractor. Defaults to \code{discretize^2}. Not used if \code{iters} has a numeric value.
#' @param tolerance The distance distance at which a point is considered to have stopped moving. Defaults to \code{sqrt(.Machine$double.eps)}.
#' @param behind Forces this item to be a background object for the purposes of layering
#' @param stride The number of times the function is applied before movement is checked: in essence finding the basins for \code{f^stride}.
#'  For non-periodic dynamical systems, this is merely an efficiency concern. For points that move to a periodic attractor with a period
#'  that is a factor of \code{stride}, this may color the basins by their parity or rank. Only used when \code{iters} has a non-numeric value. Defaults to 8.
#' @param missingCol The color given to points that stop outside of \code{epsilon} of an attractor. Defaults to "NA".
#' @param ... Extra graphical parameters for \code{image}.
#' @import graphics
#' @import grDevices
#' @seealso \code{\link{dspoint}}
#' @seealso \code{\link{dsregion}}
#' @seealso \code{\link{dspolygon}}
#' @seealso \code{\link{simattractors}}
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
#'       simbasins(discretize=0.05)
#'@export
simbasins <- function(discretize=NULL, xlim=NULL, ylim=NULL, iters=NULL,
                         epsilon=NULL, behind=TRUE, tolerance=sqrt(.Machine$double.eps), stride=8,  cols = NULL, missingCol="NA",...) {
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
    stride = stride,
    epsilon = epsilon,
    tolerance = tolerance,
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
        stop("simbasins: Can only guess basins a model defined in a range")
      }
      if(length(attractors) == 0){
        attractors <- Filter(function(x) {is.dspoint(x)}, model$feature)
        warning("simbasins: No points are specified as attractors, which may result in unintended behavior when simulating basins.")
      }
      self$fps <- pointsToList(attractors)
      self$numCols <- length(self$fps$col)
      if(is.null(self$cols)) {
        self$cols <- c(self$missingCol, self$fps$col)
      } else {
        if(length(self$cols) < length(self$fps$col))
        {
          warning(paste("simbasins: number of colors (", length(self$cols), ") provided is smaller than number of attractors (", length(self$fps$col), "). Using attractor colors."))
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
        model$warnPeriodic = FALSE
        colsMap <- mapply(findFixedPoint, self$X0, self$Y0,
                          MoreArgs=list(model=model, points=self$fps, tolerance=self$tolerance, eps=self$epsilon, stride=self$stride))
        if(model$warnPeriodic)
          warning("simbasins: some points appear to be periodic, or attractors not set properly.")
      } else if (is.infinite(iters)) {
        images <- applyTillFixed(model, self$X0, self$Y0, self$stride, self$tolerance)
        colsMap <- mapply(findNearestPoint, images$x, images$y,
                          MoreArgs=list(points=self$fps, eps=0))
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
            col=self$cols,
            add=TRUE)#, ... = self$...)
    }
  )
}


#' Reports whether x is a dsimage
#' @param x An object to test.
# @rdname simbasins
#' @keywords internal
#' @export
is.dsimage <- function(x) inherits(x,"image")

#' Determine the attractors of a model through simulation
#'
#' Attempts to determine the attractors of a model. The space is discretized into initial points, and
#' repeated iteration of the model's function is used to approximate the attractors. It is possible
#' that non-attractor fixpoints will be found by accident. The function
#' is iterated until the points move less than \code{tolerance} (default
#' \code{sqrt(.Machine$double.eps)}) between iterations.
#' The color of each point is drawn from the \code{col} parameter. If the number of points exceeds
#' the size of $\code{col}$, or $\code{col}$ is not defined, then reasonable defaults are used instead.
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
simattractors <- function(discretize=NULL, xlim=NULL, ylim=NULL, stride=8, iters=1e+18,
                         epsilon=NULL, tolerance=sqrt(.Machine$double.eps), cols= NULL){
  epsilon <- boolSelector(is.null(epsilon), NULL, epsilon^2)
  dsproto(
    `_class` = "simattractors", `_inherit` = background,
    discretize=discretize, xlim=xlim, ylim=ylim,
    stride=stride,
    iters = iters,
    attractorsCalculated = FALSE,
    attractors = list(),
    attractorCoords = NULL,
    grid = NULL,
    cols = cols,
    epsilon = epsilon,
    tolerance=tolerance,
    X0 = NULL,
    Y0 = NULL,
    attractors = NULL,
    bindWithModel = function(self, model) {
      if(!is.range(model$range)) {
        stop("Can only simulate attractors within a defined range.")
      }
      if(self$iters == 0)
        self$iters = Inf
      if(is.null(self$discretize))
        if(!is.null(model$range) && !is.null(model$range$discretize))
          self$discretize <- model$range$discretize
        else
          stop("Need a discretized range or a discretize parameter to find attractors.")
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
        warning("Determining attractors twice: this may result in duplicate points.")
      self$bindWithModel(model)
      moved <- TRUE
      images <- applyTillFixed(model, self$X0, self$Y0, self$stride, self$iters, self$tolerance)
      found <- 1
      points <- mapply(c,images$x, images$y, SIMPLIFY=FALSE)
      attractorCoords <- list(x=c(images$x[1]), y=c(images$y[1]))
      for(p in points) {
        dists  <- (attractorCoords$x - p[1])^2 + (attractorCoords$y-p[2])^2
        if(min(dists) > self$epsilon) {
          pointImage = model$apply(p[1],p[2], self$stride, accumulate=FALSE, crop=FALSE)
          imageDist = (pointImage$x - p[1])^2 + (pointImage$y - p[2])^2
          if(imageDist < self$tolerance) {
            found <- found + 1
            attractorCoords$x[found] <- p[1]
            attractorCoords$y[found] <- p[2]
          }
          else {
            warning("simattractors: non-fixed point discarded due to maximum iteration.")
          }
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

applyTillFixed <- function(model, x, y, stride, maxIters, tolerance) {
  moved <- TRUE
  prev <- list(x=x, y=y)
  iters = 0
  while(moved && iters < maxIters) {
    images <- model$apply(prev$x, prev$y, stride, accumulate=FALSE, crop=FALSE)
    dists = (images$x - prev$x)^2 + (images$y - prev$y)^2
    m <- max(dists)
    if(is.nan(m)){
      stop("dssimulation: Model not well defined: NaN")
    }
    if (max(dists) < tolerance) {
      moved <- FALSE
    }
    else {
      prev <- images
      iters <- iters+1
    }
  }
  if(iters == maxIters && moved)
    warning("dssimulation: hit iteration threshhold in simattractors.")
  if(!moved) {
    noStrideImages = model$apply(images$x, images$y, 1, accumulate=FALSE, crop=FALSE)
    dists = (noStrideImages$x - images$x)^2 + (noStrideImages$y - images$y)^2
    m <- max(dists)
    if(is.nan(m)){
      warning("dssimulation: Model not well defined: NaN")
    }
    if (max(dists) > tolerance) {
      warning("dssimulation: points are only stable under stride, may have periodic attractors.")
    }
  }
  images
}

boolSelector <- function(b, t, f) { if(b) t else f }

