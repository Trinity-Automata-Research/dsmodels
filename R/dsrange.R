#' Range of inputs for a model.
#'
#' \code{dsrange} creates a discrete or continuous
#' range for the model to be computed over. Points that fall outside the range will be
#' discarded in all features.
#'
#' You may either specify a numeric x and y, in which case 0 is the lower bound and that value is
#' the upper bound; or a range of values, in which case the min and the max of the range will be used.
#' To specify a range from \code{min} to \code{max}, use either \code{c(min,max)} or
#' \code{min:max}.
#' @include dsproto.R
#' @param x Specifies the minimum and maximum for the x axis.
#'  If only one value is specified, it is used as the maximum, and the minimum will default to 0.
#'  If a collection of values are provided, the minimum and maximum are used as the range.
#' @param y Specifies the minimum and maximum for the y axis.
#'  If only one value is specified, it is used as the maximum, and the minimum will default to 0.
#'  If a collection of values are provided, the minimum and maximum are used as the range.
#' @param discretize If a value is provided, the field is discretized into an array of points. The value
#'  specifies the distance between each point.
#'  This becomes the default when displaying \code{\link{dsarrows}} or \code{\link{dsdots}}.
#'  The number of points in the field is defined by:
#'  \deqn{(xmax-xmin+1)(ymax-ymin+1)/discretize.}{ascii}
# @param originOffset Currently not supported. Allows you to place an xlim and ylim
#  for the graph without including extra discretized space.
#' @param renderCount The number of points that a curve will be computed at when being
#'   displayed. Default 101.
#' @param ... Further fields for the dsrange object.
#' @seealso \code{\link{dsmodel}}
#' @seealso \code{\link{dsarrows}}
#' @seealso \code{\link{dsdots}}
#' @export
#' @examples
#' fun <- function(X,Y) {
#'   list(
#'     X/exp(Y),
#'     Y/exp(X)
#'   )
#' }
#' model <- dsmodel(fun, title = "A range with no features!")
#' #Since no features are added, only the area and title are displayed.
#' model + dsrange(3, 3, discretize = .09)
dsrange <- function(x,y,discretize = 0,
                    #originOffset = c(-.1,-.1),
                    renderCount=101, ...){ # Range

  if(length(x) == 1) {
    x <- c(0,x)
  }
  if(length(y) == 1) {
    y <- c(0,y)
  }
  xlim <- x
  ylim <- y
  if(length(xlim)>1) {
    xlim <- c(min(xlim),max(xlim))
  }
  if(length(ylim)>1) {
    ylim <- c(min(ylim),max(ylim))
  }

  if(discretize != 0)
  {

    gx = seq(min(x),max(x), by = discretize)
    gy = seq(min(y),max(y), by = discretize)
    N = as.matrix(expand.grid(gx,gy))

    X0 = N[,1]
    Y0 = N[,2]

    range = dsproto(
      `_class` = "range",
      `_inherit` = NULL,
      X0 = X0, Y0 = Y0,
      grid = list(x=gx, y=gy),
      discretize = discretize,
      dims = 2, #originOffset = originOffset,
      X1= NULL, Y1= NULL,
      appliedFun = list(),
      xlim = xlim, ylim=ylim, renderCount=renderCount,
      rendered = FALSE,
      render = function(self, model) {
        self$rendered = TRUE
         plot(0, type = "l", lwd = 3, axes=T, main = model$title,
             xlab = "", ylab = "", xlim = self$xlim, ylim = self$ylim)
      },

      recalculate = function(self, model) {
        gx = seq(min(self$xlim),max(self$xlim), by = self$discretize)
        gy = seq(min(self$ylim),max(self$ylim), by = self$discretize)
        N = as.matrix(expand.grid(gx,gy))

        self$X0 = N[,1]
        self$Y0 = N[,2]

        newrange = model$apply(self$X0, self$Y0, 1)
        self$X1 = newrange[[2]]$x
        self$Y1 = newrange[[2]]$y

        self$arrowsComputed = TRUE
      }
    )
  }
  else
  {
    range = dsproto(
      `_class` = "range",
      `_inherit` = NULL,
      xlim = xlim, ylim=ylim,
      discretize = 0, renderCount=renderCount,
      rendered = FALSE,
      render = function(self, model) {
        self$rendered = TRUE
        plot(0, type = "l", lwd = 3, axes=T, main = model$title,
             xlab = "", ylab = "", xlim = self$xlim, ylim = self$ylim)
      },
      ...=...)
  }
  range
}

#' Reports whether x is a range.
#' @param x An object to test.
#' @keywords internal
# @describeIn dsrange
#' @export
is.range <- function(x) inherits(x,"range")

#' Reports whether x is a discretized range.
#' @param x An object to test.
#' @keywords internal
# @rdname dsrange
#' @export
is.discretizedrange <- function(x) { inherits(x,"range") && x$discretize != 0 }
