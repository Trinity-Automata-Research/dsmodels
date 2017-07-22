#' Individual points and their images
#'
#' \code{pnt} and \code{dspoint} are the same function.
#' This function takes a single point and creates an object displaying the point, and optionally
#' it's behavior throughout iterations of the system.
#'
#' @section Images of the point:
#'
#' The \code{dspoint} object begins with an initial point. Images of the point may be displayed in three ways.
#'
#' If the \code{image} parameter is a single color and \code{iters} is not set, then \code{dspoint} will calculate and display
#' the image of the point under the model's function in that color.
#'
#' If the \code{image}  parameter is a vector of k colors, then \code{dspoint} calculates and
#' displays k successive images of the point using those colors.
#' The string "NA" may be used to avoid displaying an iteration.
#'
#' If the \code{image} parameter is a single color and \code{iters} is defined, then \code{iters}
#' successive images are displayed, using a gradient between \code{col} and \code{image}.
#'
#' In most cases, rather than specifying \code{col} and \code{image} separately, they may be
#' combined into a single vector.
#'
#'
#' @include dsproto.R
#' @param x The x-coordinate of the point.
#' @param y The y-coordinate of the point.
#' @param label A string label. Text can be input in the form of pseudo-LaTeX code within quotes.
#'  See \code{\link[latex2exp]{TeX}} for more details.
#'  Text will appear above the dot by default.
#'  Please see the \code{offset} parameter to adjust.
#' @param col A string color for the point.
#'  Use "NA" or "" to hide the point. See also \code{display}.
#' @param offset This will offset the label. Enter as \code{c(x, y)}. Defaults to an automatic scale dependent on the \code{dsrange}'s \code{y} axis size.
#' @param size Determines the size of the point.
#' @param display Set display = FALSE to hide the dot, but still add to your system.
#'  Mostly useful for \code{\link{simbasins}()}.
#' @param image A single color as a string, or a vector of colors as a string.
#'  See details for more information.
#' @param fixed A flag to declare a fixed point. The image of any fixed
#'  point is should be the original point.
#' @param attractor A flag to delcare a point as an attractor: a fixed point for the function that
#' other points converge to. Used in \code{\link{simbasins}()}.
#' @param regionCol An alternate color used to define the color of the region for
#'  \code{\link{simbasins}()}. Defaults to \code{col} or \code{col[1]}.
#' @param iters Determines the number of iterations of the function when making a color gradient.
#' Use \code{col = color1, image = color2, iters = n} to create a gradient of colors between
#' color1 and color2. See details for more information.
#' @param crop If \code{crop==TRUE} points outside of defined range will be cropped,
#' and no further images will be calculated.
#' @param artificial For internal use.
#' @param pch Plotting 'character' or symbol to use, default is 21 (filled circle). See \code{help(pch)} for details.
#' @param ... Extra graphical parameters to be sent through \code{points}
#' @import latex2exp
#' @examples
#' library(dsmodels)
#'
#' fun <- function(X,Y) {
#'   list(
#'     X/exp(Y),
#'     Y/exp(X)
#'   )
#' }
#'
#' model <- dsmodel(fun, title = "A Single Point")
#' model + dsrange(3,3, discretize = .09) +
#' 	dspoint(1,2, col = "magenta")
#'
#' dsmodel(fun, title = "A Point and a Label") +
#'  dsrange(3,3, discretize = .09) +
#'  dspoint(2.2, 2.1, label = "$x^{\\alpha}$", col = "green")
#'
#' dsmodel(fun, title = "A Point and Iterations of that Point") +
#'  dsrange(3,3, discretize = .09) +
#' 	dspoint(1,1, col = "red", image = c("orange","yellow"))
#'
#' dsmodel(fun, title = "Iterations of a Point over a Color Gradient") +
#'  dsrange(3,3, discretize = .09) +
#' 	dspoint(0.2, 0.5, image = "pink", iters = 3, col = "grey")
#' @export
dspoint <- function(x, y, label = "", pch = 21, size = 2,
                       col = "blue", regionCol=NULL, image = "", offset=NULL,
                    display = TRUE, fixed = FALSE, iters = 0,
                    attractor=FALSE, crop = TRUE, artificial=FALSE,
                    ...) {

  col <- colorVector(col, image, iters)
  iters <- length(col)
  if(is.null(regionCol))
    regionCol <- col[1]

  texLabel <- TeX(label)

  dsproto(
    `_class` = "dspoint", `_inherit` = feature,
    x = x,
    y = y,
    label = texLabel,
    col = col,
    pch = pch,
    cex = size,
    regionCol = regionCol,
    display = display,
    attractor=attractor,
    fixed=fixed || attractor,
    toPlot = NULL,
    iters = iters,
    artifical=artificial,
    offset=offset,
    crop = crop,
    render = function(self, model) {
      self$calculateImage(model)
      if(self$display) {
        for(i in 1:(self$iters)) {
          tmp <- self$toPlot[[i]]
          points(
            tmp$x, tmp$y,
            bg = self$col[[i]],
            pch = self$pch,
            cex = self$cex,
            ... = self$...)
        }
        self$displayLabel(model$range)
      }
    },
    displayLabel = function(self, range) {
      if(is.null(self$offset)) {
          scale <- 0.08*(abs(max(range$ylim) - min(range$ylim)))
          self$offset=c(0,scale)
      }
      xloc <- self$x + self$offset[1]
      yloc <- self$y + self$offset[2]
      text(xloc,yloc, labels = self$label)
    },
    calculateImage = function(self, model) {
      if(iters == 0)
        self$toPlot <- model$apply(self$x, self$y, length(self$col), crop = self$crop)
      else
        self$toPlot <- model$apply(self$x, self$y, self$iters, crop = self$crop)
    },
    recalculate = function(self, model) {
      self$calculateImage(model)
    }
  )
}

#' Alias to make constructing points for regions easier.
# @param x An object to test.
#' @keywords internal
# @rdname dsregion
# @usage NULL
#' @export
pnt <- dspoint

#' Reports whether x is a dspoint
#' @param x An object to test.
# @rdname dspoint
#' @keywords internal
#' @export
is.dspoint <- function(x) inherits(x, "dspoint")


#' Converts a list of points to a list of x coordinates, y coordinates, region colors, and indexes.
#' @param points A list of dspoints, or anything that supports $x, $y, and $col
#' @keywords internal
# @rdname dspoint
#' @export
pointsToList <- function(points) {
  xs <- sapply(points, function(k) k$x)
  ys <- sapply(points, function(k) k$y)
  cols <- unlist(sapply(points, function(k) k$regionCol))
  inds <- as.list(1:length(points))
  list(x=xs,y=ys,col=cols, inds=inds)
}

#' Finds which fixed-point a point will end up at under a certain function.
# @rdname dspoint
#' @param x A numeric initial x coordinate.
#' @param y A numeric initial y coordinate.
#' @param points A list comprised of x coordinates, y coordinates, such as output by \code{pointsToList}. Should represent the attractors of \code{fun}.
#' @param eps An epsilon, expected to already be squared, used to determine when a point is closed enough to a fixed point.
#' @param tolerance A, usually smaller, value used to determine when a point has stopped moving without finding a nearby fixpoint.
#' @param model A dsmodel encapsulating the function to be applied.
#' @param stride The number of times to apply the function at each step.
#' @keywords internal
#' @export
findFixedPoint <- function (x,y,points,eps,tolerance,model, stride) {
  xp <- x
  yp <- y
  moves <- TRUE
  ind <- findNearestPoint(xp, yp, points, eps)
  while (ind==0 && moves) {
    tmp <- model$apply(xp,yp,iters=stride,accumulate=FALSE,crop=FALSE)
    if( abs((xp-tmp[[1]])^2 + (yp-tmp[[2]])^2) < tolerance)
      moves <- FALSE
    xp <- tmp[[1]]
    yp <- tmp[[2]]
    ind <- findNearestPoint(xp, yp, points, eps)
  }
  ind
}

#' Determines which point in a list is closest to the input. 0 is used if all are further from  epsilon.
#' @export
# @rdname dspoint
#' @param x A numeric  x coordinate.
#' @param y A numeric  y coordinate.
#' @param points A list comprised of x coordinates, y coordinates, such as output by \code{pointsToList}. Should represent the attractors of \code{fun}.
#' @param eps An epsilon, expected to already be squared, used to determine when a point is close enough. Only important if \code{deep} is true.
#' @param deep If true, only return a point if it is within epsilon distance. Otherwise, return the closest point.
#' @keywords internal
findNearestPoint <- function(x, y, points, eps, deep=TRUE) {
  dsq <- abs((x-points$x)^2+(y-points$y)^2)
  if(deep && min(dsq) >= eps)
    0
  else
    points$inds[which.min(dsq)]
}

