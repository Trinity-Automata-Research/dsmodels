#' Add a visualization of the system using arrows.
#'
#' The visualization displays the movement of a uniform array of points under the function defined
#' by the model as arrows.
#' The base
#' of the arrow is placed on each discretized point. The tip of the
#' head of the arrow points in the direction of the image of that
#' point.
#' In order to use this visualization, a discretization parameter must be set either on the range or
#' as a parameter.
#'
#' The arrows are scaled according to the discretization.  If the
#' discretization is too fine (small), the arrows may seem crowded.
#' It is suggested to try a coarser (larger) discretization size
#' before modifying the size of the arrows.
#'
#' The most common way to invoke \code{dsarrows} is to simply add
#' \code{model + dsarrows()}, where model is a variable corresponding
#' with the \code{dsmodel} class.
#'
#' @include dsproto.R
#' @param scale Changes the size of the
#'	arrow to a user-specified scale relative to the discretization parameter.
#' @param col Colors the arrows.
#' @param head.length Changes the size of the arrowhead. Passed directly to \code{Arrows}
#'  as arr.length. When the range is large, a smaller value produces reasonable results.
#' @param type Determines type of arrow. Accepted values are identical to
#'	acceptable values of arr.type in the "shape" library.
#' @param length A non-scaled length of arrow tail. Causes \code{scale} to be ignored.
#' @param iters Allows user to point the
#'  head of each arrow towards the result of a specified number of iterations of the function.
#' @param discretize Overrides the discretization parameter defined in the range.
#' @param crop If \code{crop==TRUE} then arrows whose image falls outside the range are not displayed.
#' @param angle Specifies the angle of the head of the arrow.
#'  Passed directly into \code{shape}'s \code{Arrows} function.
#' @param behind Forces the arrows to be a background object for the purposes of layering.
#' @param ... Further graphical parameters passed to \code{Arrows}
#' @import shape
#' @seealso \code{\link{dsdots}}
#' @examples
#' library(dsmodels)
#'
#' fun <- function(X,Y) {
#'   list(
#'     X/exp(Y),
#'     Y/exp(X)
#'   )
#' }
#' model <- dsmodel(fun, title = "Blue Arrows")
#' range <- dsrange(x = -2:3, y = -2:3, discretize = .5)
#' model + range + dsarrows()
#'
#' dsmodel(fun, title = "Spaced Purple Arrows") +
#'  dsrange(x = -2:3, y = -2:3, discretize = .5) +
#'  dsarrows(discretize = 1, col = "magenta")
#' @export

dsarrows <- function(
  scale = 0.9, col = "blue", angle = 30,
  type = "simple", length = NULL,
  iters = 1, head.length = 0.2, discretize = NULL,
  behind = TRUE, crop = FALSE, ...){

  if(behind)
    parent = visualization
  else
    parent = feature

  dsproto(
      `_class` ="dsarrows",
      `_inherit` = parent,
      scale = scale,
      X0 = NULL, Y0 = NULL,
      X1 = NULL, Y1 = NULL,
      X2 = NULL, Y2 = NULL,
      length = length,
      arrowsComputed = FALSE,
      iters = iters,
      col = col,
      type = type,
      angle= angle,
      discretize = discretize,
      crop = crop,
      head.length = head.length,
      ... = ...,
      # getLims = function(self,model) {
      #   if(is.null(self$X0))
      #     self$X0 = model$range$X0
      #   if(is.null(self$Y0))
      #     self$Y0 = model$range$Y0
      # },
      computeArrows = function(self, model) {
        self$rediscretize(model)
        if(!self$arrowsComputed) {
          tmp <- model$apply(self$X0, self$Y0, accumulate=FALSE, self$iters, crop = self$crop)
          self$X1 <- tmp$x
          self$Y1 <- tmp$y
          if((length(self$X0) > 1500 || length(self$Y0) > 1500))
            warning("arrows: We suggest coarser discretization")
          self$arrowsComputed <- TRUE
          if(is.null(self$length))
          {
            self$length <- self$scale*self$discretize
          }
          L = self$length
          a=(self$Y1-self$Y0)/(self$X1-self$X0)
          b=self$Y0-a*self$X0
          self$X2 <- self$X0+sign((self$X1-self$X0))*L/sqrt(a^2+1)
          self$Y2 <- a*self$X2+b
        }
      },
      render = function(self, model) {
        self$rediscretize(model)
        if(!self$arrowsComputed)
          self$recalculate(model)
        if(!is.discretizedrange(model$range) && is.null(self$discretize))
          stop("arrows: dsrange is not discretized. Give range an extra 'discretize' parameter in either dsarrows or dsrange.")
        Arrows(self$X0, self$Y0,
               self$X2, self$Y2,
               col = self$col, arr.length = self$head.length,
               angle = self$angle, arr.type = self$type, ... = self$...)
      },
      recalculate = function(self, model) {
        self$X1 = NULL
        self$X2 = NULL
        self$Y1 = NULL
        self$Y2 = NULL
        self$arrowsComputed = FALSE
        self$computeArrows(model)
      },
      rediscretize = function(self, model) { # if recalculate needed, include model
        if(!is.null(self$discretize)){
        x <- model$range$xlim
        y <- model$range$ylim

        gx = seq(min(x),max(x), by = self$discretize)
        gy = seq(min(y),max(y), by = self$discretize)
        N = as.matrix(expand.grid(gx,gy))

        self$X0 = N[,1]
        self$Y0 = N[,2]
        }
        else{
          if(model$range$discretize == 0)
            stop("dsdots: Either the dsrange or the dsdots have to have a non-empty discretization parameter.")
          self$X0 = model$range$X0
          self$Y0 = model$range$Y0
          self$discretize = model$range$discretize
        }
      }
    )
}

#' Reports whether x is a dsarrows object.
#'
#' @param x An object to test.
#' @keywords internal
#' @export
is.dsarrows <- function(x) inherits(x,"dsarrows")
