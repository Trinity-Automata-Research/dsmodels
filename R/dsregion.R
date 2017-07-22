#' Colored polygonal region
#'
#' Friendly function to create a polygon corresponding with given values of the polygon's corners.
#' The polygon is then colored automatically.
#'
#' @section Calling dspolygon Correctly:
#'
#' \code{\link{dspolygon}} takes the x and y points similar to the default \code{polygon} function.
#' The \code{x} parameter takes a numeric vector containing the x-values of each corner.
#' The \code{y} parameter takes a numeric vector containing the y-values of each corner.
#' The \code{x} and \code{y} coordinates of the corners of the polygon
#' will be the pairs made from the \code{x} and \code{y} parameters with equal indices.
#'
#' @section Calling dsregion Correctly:
#'
#' The \code{...} parameter in \code{dsregion} can take multiple
#' \code{\link{dspoint}}s, \code{pnt}s,
#' or simply vectors each containing two points \code{c(x,y)}.
#' See the examples if clarification is needed.
#'
#' @include dsproto.R
#' @param ... Takes points which will act as corners. See example and details for usage.
# @param ... Extra graphical parameters passed to \code{polygon}.
#' @seealso \code{\link{dspoint}}
#' @seealso \code{\link{simattractors}}
#' @seealso \code{\link{simbasins}}
#' @examples
#' library(dsmodels)
#'
#' fun <- function(X,Y) {
#'   list(
#'     X/exp(Y),
#'     Y/exp(X)
#'   )
#' }
#' model <- dsmodel(fun, title = "Regions!")
#' range <- dsrange(3, 3, discretize = .09)
#' model+range
#'
#' # dspolygon usage
#' model + dspolygon(x = c(-.05,3,3),
#'                   y = c(0,0,3),
#'                   col = "yellow")
#' a <- dspoint(3,3)
#' b <- dspoint(2.5,3)
#' c <- dspoint(2,2)
#'
#' # Different inputs for dsregion shown below
#'
#' model + dsregion(a, b, c, col = "orange") +
#' 	dsregion(pnt(0,0),pnt(1,0),pnt(1,1),pnt(0,1), col = "green") +
#' 	dsregion(c(1,1),c(2,1),c(2,2),c(1,2), c(1.8,1.5), col = "magenta")
#'@export
dsregion = function(..., col = "yellow", border = NA, behind = TRUE) {
  points <- list(...) #either fixed points or vector of points
  len <- length(points)
  xc <- vector(length=len)
  yc <- vector(length=len)
  i <- 1
  #sapply(points,function(o) {o$x})
  for(p in points) {
    if(is.dspoint(p)) {
      xc[i] <- p$x
      yc[i] <- p$y
      i <- i+1
    }
    else if(is.numeric(p)){
      xc[i] <- p[1]
      yc[i] <- p[2]
      i <- i+1
    }
    else {
      sprintf("Can't add %s to graph.", i)
      stop()
    }
  }
  dspolygon(xc,yc,col=col,border = border, behind = behind)
}


#' @include dsproto.R
#' @param x A numeric vector containing the x-values of each corner.
#' @param y A collection (for example: \code{c(1,2,3)}) of points on the y-axis
#'  which correspond to the corners of the polygon.
#'  Each member of the \code{y} parameter corresponds with a member of the \code{x}
#'  parameter with the same index.
#' @param col The color of the polygon
#' @param border The color of the border of the polygon.
#' @param behind Forces the polygon to be a background object for the purposes of layering.
#' @rdname dsregion
#'@export
dspolygon <- function(x, y, col = "yellow", border = NA, behind = TRUE) {

  if(behind)
    parent <- background
  else
    parent <- feature

  dsproto(
    `_class` = "dsregion", `_inherit` = parent,
    x = x, y = y, col = col,
    render = function(self, model) {
      polygon(x = self$x,
              y = self$y,
              col =self$col,
              border = self$border, ... = self$...)
    },
    recalculate = function(self, model) {
      warning("dspolygon: Nothing here")
    }
  )
}

#regionByPoints <- function(col = "yellow", ...)

#' Reports whether x is a dsregion object.
#' @param x An object to test.
# @rdname dsregion
#' @keywords internal
#'@export
is.dsregion <- function(x) inherits(x,"dsregion")
