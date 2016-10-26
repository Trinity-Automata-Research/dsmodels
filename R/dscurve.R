#' Curve objects
#'
#' This function takes a description of a curve and creates an object displaying the curve, and optionally
#' it's behavior throughout iterations of the system.
#' The curve is defined either by the graph of a single function or a pair of parametric
#' equations. By default, rendered with the \code{lines} function.
#'
#' @section The graph of a function:
#'
#' If the parameter \code{fun} is a function and
#' the parameter \code{yfun} is not provided, then \code{dscurve} contains
#' the curve of points (x,fun(x)). The inputs to \code{fun} are \code{n} points between the maximum
#  and minimum. The maximum and minimum are taken from  the
#' \code{\link{dsrange}}'s x limits, but can be overwritten with the \code{xlim} parameter.
#'
#' @section  Parametric equations:
#'
#' If the parameter \code{fun} and \code{yfun} are both provided,
#' \code{dscurve} contains the parametric curve described by the functions. The function is
#' calculated at \code{n}
#' points ranging from \code{tmin} to \code{tmax}.
#'
#' @section Images of curves:
#'
#' The \code{dscurve} object begins with an initial curve. Images of the curve may be displayed in three ways.
#' If the \code{image} parameter is a single color and \code{iters} is not set, then \code{dscurve} will calculate and display
#' the image of the curve under the model's function in that color.
#'
#' If the \code{image}  parameter is a vector of k colors, then \code{dscurve} calculates and
#' displays k successive images of the curve using those colors.
#' The string "NA" may be used to avoid displaying an iteration.
#'
#' If the \code{image} parameter is a single color and \code{iters} is defined, then \code{iters}
#' successive images are displayed, using a gradient between \code{col} and \code{image}.
#'
#' In most cases, rather than specifying \code{col} and \code{image} separately, they may be
#' combined into a single vector.
#'
#' @include dsproto.R
#' @param fun A function. If \code{yfun} is provided, this is the x-equation of the parametric
#' equations. If not, the function's graph is used.
#' See sections describing graphs and parameteric equations for more info.
#' @param yfun The y-equation of the parameteric equations.
#' See sections describing parametric equations for more info.
#' @param iters Determines the number of iterations of the function when making a color gradient.
#' Use \code{col = color1, image = color2, iters = n} to create a gradient of colors between
#' color1 and color2. See details for more information.
#' @param col The color of the original curve, as a string.
#' @param image A single color as a string, or a vector of colors as a string.
#'  See details for more information.
#' @param lwd Line width expressed as a double. Only used if \code{discretize} is not set.
#' @param n The number of points that will be calculated.
#'	Defaults to the dsrange's \code{renderCount}.
#'	\code{n} is used to interact with \code{discretize}.
#' @param tstart Only used for parametric curves. The minimum input
#'   for both functions. Default 0.
#' @param tend Only used for parametric curves. The maximum input
#'	for the functions. Default 1.
#' @param xlim Only used for the graph of a function. Determines the range of x values for which the function
#' is plotted. Defaults to the x limits of the model's dsrange.
#' @param crop If \code{crop==TRUE}, the original curve and all iterations are cropped to the range.
#' @param discretize Set \code{discretize=TRUE} to display the calculated points, instead of
#' connecting them as a curve: the curve is displayed with \code{points}
#' instead of \code{lines}.
#' @param ... Further graphical parameters passed to \code{lines} or \code{points}.
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
#' model <- dsmodel(fun, title = "Points on a One-Dimensional Curve")
#' range <- dsrange(-2:2,-2:2, discretize = 0.5)
#'
#' # Add a one-dimensional curve and its image in blue.
#' curve1 <- dscurve(function(x) x^2,
#'                 col = "orange",
#'                 image = "blue",
#'                 discretize = TRUE,
#'                 xlim = c(-2,2))
#' model +
#'  range +
#' 	curve1
#'
#' # Create a parametric curve with image iterations red then green.
#' curve2 <- dscurve(function(x) x^2,
#'                 function(x) x,
#'                 image = c("red", "green"),
#'                 tstart = -2, tend = 2)
#' dsmodel(fun, "A Parametric Curve and Iterations of that Curve") +
#'  dsrange(-2:2, -2:2, discretize = 0.5) +
#'  curve2
#'
#' @export
dscurve <- function(fun, yfun = "",
                    col = "black", image = "",
                    lwd = 3, n=NULL, iters = 0,
                    crop = TRUE,  tstart=0, tend=1,
                    discretize=FALSE, xlim = NULL,
                    ...) {

  colors <- colorVector(col, image, iters)
  iters <- length(colors)

  if(is.function(yfun)){
    dscurveParam(xfun = fun, yfun = yfun,
                 colors = colors, lwd = lwd,
                 n = n, iters = iters, crop, discretize = discretize,
                 tstart = tstart, tend = tend,
                 ...)
  } else {
    dscurveGraph(fun = fun, colors = colors,
                     lwd = lwd, n = n, iters = iters, discretize = discretize,
                     crop, xlim = xlim, ...)
  }
}

dscurveParam<- function(xfun, yfun, colors, lwd, n, tstart=0, tend=1,
                        iters, crop = TRUE, discretize = FALSE, ...){
  if(is.null(n))
    renderInputs = NULL
  else
    renderInputs = seq(tstart, tend, length.out=n)
  dsproto(
    `_class` = "curve", `_inherit` = feature,
    xfun = xfun,
    yfun = yfun,
    col = colors,
    iters = iters,
    tstart = tstart,
    tend=tend,
    toPlot = NULL,
    lwd = lwd,
    renderInputs = renderInputs,
    crop = crop,
    discretize = discretize,
        ... = ...,
    calculateImage = function(self, model, tValues) {
      if(is.null(self$toPlot)) {
        self$toPlot <- model$apply(
          self$xfun(tValues),
          self$yfun(tValues),
          self$iters,
          crop = self$crop)
      }
    },
    render = function(self, model) {
      if(is.null(model$range)) stop("dscurve: Add range first")
      if(is.null(self$renderInputs))
        tValues = seq(self$tstart,self$tend,length.out=model$range$renderCount)
      else
        tValues = self$renderInputs
      self$calculateImage(model,tValues)
      if(self$discretize){
        for(i in 1:(self$iters))
          points(self$toPlot[[i]]$x, self$toPlot[[i]]$y,
                col = self$col[[i]], ... = self$...)
      }
      else{
        for(i in 1:(self$iters))
          lines(self$toPlot[[i]]$x, self$toPlot[[i]]$y, lwd = self$lwd,
                col = self$col[[i]], ... = self$...)
      }
    },
    recalculate = function(self, model) {
      if(is.null(self$renderInputs))
        tValues = seq(self$tstart,self$tend,length.out=model$range$renderCount)
      else
        tValues = self$renderInputs
      self$calculateImage(model, tValues)
    }
  )
}


dscurveGraph <- function(fun, colors, lwd, n, iters,
                            crop = TRUE, discretize = FALSE,
                            xlim = NULL, ...){
  dsproto(
    `_class` = "curve", `_inherit` = feature,
    fun = fun,
    col = colors,
    lwd = lwd,
    iters = iters,
    n = n,
    xValues = NULL,
    yValues = NULL,
    toPlot = NULL,
    xlim = xlim,
    discretize = discretize,
    crop = crop,
    ... = ...,
    calculateImage = function(self, model, xValues, yValues) {
      if(is.null(self$toPlot))
        self$toPlot <- model$apply(xValues, yValues, self$iters, crop = self$crop)
    },
    calculateXYValues = function(self,model) {
      if(is.null(self$n))
        numPoints <- model$range$renderCount
      else
        numPoints <- self$n
      self$xValues <-seq(min(model$range$xlim),max(model$range$xlim), length.out = numPoints)
      self$xValues <- self$prune(self$xlim,self$xValues)
      self$yValues <- self$fun(self$xValues)
      self$calculateImage(model, self$xValues, self$yValues)
    },
    render = function(self, model) {
      self$calculateXYValues(model)
      if(self$discretize){
        for(i in 1:(self$iters))
          points(self$toPlot[[i]]$x, self$toPlot[[i]]$y,
                col = self$col[[i]], ... = self$...)
      }
      else{
        for(i in 1:(self$iters))
          lines(self$toPlot[[i]]$x, self$toPlot[[i]]$y, lwd = self$lwd,
                col = self$col[[i]], ... = self$...)
      }
    },
    recalculate = function(self, model) {
      self$calculateImage(model, self$xValues, self$yValues)
    },
    prune = function(self, lim, values) {
      if(!is.null(lim)){
        if(min(values)<min(lim)){
          values = values[values>=min(lim)]
        }
        if(max(values)>max(lim)){
          values = values[values<=max(lim)]
        }
      }
      values
    }
  )
}

#' Reports whether x is a dscurves object.
#' @param x An object to test.
# @rdname dscurve
#' @keywords internal
#' @export
is.curve <- function(x) inherits(x,"curve")

