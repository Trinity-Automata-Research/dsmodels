#' Adds a visualization of the system using dots.
#'
#' The visualization displays a uniform array of points and their images under the function defined
#' by the model as dots.  Multiple
#' iterations of the function may be visualized.
#' In order to use this visualization, a discretization parameter must be provided in either the
#' range or to the \code{dsdots} object.
#'
#' The \code{col} parameter defines the color of the initial, discretized, points.
#' There are three modes of operation.
#' If the \code{image} parameter
#' parameter is a single color, and \code{iters} is not set, then the image of each point is displayed as a
#' dot of color \code{image}.
#'
#' Alternately, \code{image} can be set as a vector of colors (for example: \code{image =
#' c("red", "NA", "green")}). In this case the function is applied iteratively to the points a
#' number of times equal to the length of the vector. The initial points are displayed using
#' \code{col}. Each iteration is displayed by the
#' corresponding color. \code{"NA"} may be used to not display that iteration.
#' to specify the number of iterations of the function to apply.
#'
#' Finally, if iters is set to a numeric value greater than 1, a color gradient is used to display
#' iterations. In this case both
#' \code{col} and \code{image} should be single colors. The function is applied \code{iters} time,
#' with each iteration being colored along the gradient from \code{col} to \code{image}.
#'
#' In most cases, rather than specifying \code{col} and \code{image} separately, they may be
#' combined into a single vector.

#' @include dsproto.R
#' @param col A string specifying the color of the initial discretized points.
#' @param image Sets the color of the final image of the discretized points. See details.
#' @param iters Determines the number of iterations of the function when making a color gradient.
#' Use \code{col = color1, image = color2, iters = n} to create a gradient of colors between
#' color1 and color2. See details for more information.
#' @param size Determines the display size of each dot.
#' @param discretize Overrides the discretization parameter defined in the range.
#' @param crop If \code{crop==TRUE}, remove points found outside of the range
#'  from being rendered and iterated upon.
#' @param behind Sets the dots as a background object for layering purposes.
#' @param ... Extra graphical parameters
#' @seealso \code{\link{dsarrows}}
#' @seealso \code{\link{dspoint}}
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
#' model <- dsmodel(fun, title = "View of the Discretized Field")
#' range <- dsrange(-2:2,-2:2, discretize = 0.09)
#'
#' # To view the discretized field, simply add dsdots() to your model
#' model + range + dsdots()
#'
#' # To view a gradient with a certain amount of iterations,
#' # specify the image and the amount of iterations in the image
#' # and iters parameters, respectively.
#' dsmodel(fun, title = "Gradient of Iterations from Blue to Red") +
#'    dsrange(-2:2,-2:2, discretize = 0.09) +
#'    dsdots(col = "blue", image = "red", iters = 3)
#'
#' # Set color to "NA" if you wish for the specified iteration to not
#' # appear in the image
#' dsmodel(fun, title = "Display Only the Third Iteration") +
#'    dsrange(-2:2,-2:2, discretize = 0.09) +
#'    dsdots(col = "NA", image = c("NA","blue"), size = 1)

#' @export
dsdots <- function(col = "black",image = "", iters = 1,
                   discretize = NULL, crop = TRUE, size = 0.37,
                   behind = TRUE, ...) {
  col <- colorVector(col, image, iters)
  iters = length(col)

  if(behind)
    parent <- visualization
  else
    parent <- feature

  dsproto(
    `_class` = "dots", `_inherit` = parent,
    ... = ...,
    cex = size,
    X0 = NULL, Y0 = NULL,
    col = col,
    iters = iters,
    dotsComputed = FALSE,
    discretize = discretize,
    toPlot = c(), #when filled should be a list
    crop = crop,
    computeDots = function(self, model) {
      self$dotsComputed <- TRUE
      self$rediscretize(model)
      self$toPlot <- model$apply(self$X0, self$Y0, self$iters, crop = self$crop)
    },
    render = function(self, model) {
      self$rediscretize(model)
	    if(!self$dotsComputed) {
      	self$computeDots(model)
	    }
      for(i in 1:(self$iters)){
        tmp <- self$toPlot[[i]]
        points(
          tmp$x,tmp$y,
          col = self$col[[i]], cex = self$cex,
          ... = self$...
        )
      }
    },
    recalculate = function(self, model) {
      self$computeDots(model)
    },
    rediscretize = function(self, model) { # if recalculate needed, include model
      if(!is.null(self$discretize)) {
      	x <- model$range$xlim
      	y <- model$range$ylim

      	gx = seq(min(x),max(x), by = self$discretize)
      	gy = seq(min(y),max(y), by = self$discretize)
      	N = as.matrix(expand.grid(gx,gy))

      	self$X0 = N[,1]
      	self$Y0 = N[,2]
      }
	  else {
	    if(model$range$discretize == 0)
	      stop("dsdots: Either the dsrange or the dsdots have to have a non-empty discretization parameter.")
        self$X0 = model$range$X0
        self$Y0 = model$range$Y0
        self$discretize = model$range$discretize
	  }
	}
  )
}

#' Reports whether x is a dsdots object.
#' @param x An object to test.
#' @keywords internal
# @rdname dsdots
#' @export
is.dots <- function(x) inherits(x,"dots")
