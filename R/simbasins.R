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
    bound = FALSE,
    grid = NULL,
    colMatrix = NULL,
    numCols = NULL,
    cols = cols,
    fps = NULL,
    stride = stride,
    epsilon = epsilon,
    tolerance = tolerance,
    missingCol = missingCol,
    ... = ...,
    on.bind = function(self, model) {
      self$grid <- model$range$centers(discretize=self$discretize,xlim=self$xlim, ylim=self$ylim)
      attractors <- Filter(
        function(x) { (is.dspoint(x)) && (x$attractor) },
        model$feature)
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
      if(is.null(self$epsilon)){
        self$epsilon <- (model$range$getDiscretize(self$discretize))^2
      }
      self$bound <- TRUE
      self$calculate.basins(model)
    },

    calculate.basins = function(self, model) {
      if(is.null(self$iters)) {
        model$warnPeriodic = FALSE
        colsMap <- mapply(findFixedPoint, self$grid$X0, self$grid$Y0,
                          MoreArgs=list(model=model, points=self$fps, tolerance=self$tolerance, eps=self$epsilon, stride=self$stride))
        if(model$warnPeriodic)
          warning("simbasins: some points appear to be periodic, or attractors not set properly.")
      } else if (is.infinite(iters)) {
        images <- applyTillFixed(model, self$grid$X0, self$grid$Y0, self$stride, self$iters, initIters = 0, self$tolerance)
        colsMap <- mapply(findNearestPoint, images$x, images$y,
                          MoreArgs=list(points=self$fps, eps=self$epsilon))
      }
      else {
        images <- model$apply(self$grid$X0, self$grid$Y0, self$iters, crop=FALSE, accumulate=FALSE)
        colsMap <- mapply(findNearestPoint, images$x, images$y,
                          MoreArgs=list(points=self$fps, eps=self$epsilon))
      }
      self$colMatrix <- matrix(unlist(colsMap), nrow=length(self$grid$x))
    },
    render = function(self, model) {
      if(!self$bound)
        stop("Critical Error: Attempting to render basins before bound to model. Please notify developers.")
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
