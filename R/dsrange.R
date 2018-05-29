#' Range of inputs for a model
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
#' @param axes If \code{FALSE}, the axes will not be drawn. Defaults to \code{TRUE}.
#' @param frame.plot If \code{FALSE}, the frame of the plot will not be drawn. Defaults to \code{TRUE}.
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
                    renderCount=101, axes = TRUE, frame.plot = TRUE, ...){ # Range
  xlim <- make.lims(x)
  ylim <- make.lims(y)
  dsproto(
    `_class` = "range",
    `_inherit` = facade,
    discretize = discretize,
    dims = 2, #originOffset = originOffset,
    appliedFun = list(),
    xlim = xlim, ylim=ylim, renderCount=renderCount,
    rendered = FALSE,
    axes = axes,
    frame.plot = frame.plot,
    #visualization methods
    render = function(self, model) {
      self$rendered = TRUE
      plot(0, type = "l", lwd = 3, axes=self$axes, main = model$title,
           xlab = "", ylab = "", xlim = self$xlim, ylim = self$ylim,
           frame.plot = self$frame.plot)
    },
    #methods for creating grids
    getDiscretize = function(self, potential) {
      if(is.null(potential)||potential==0){
        if(is.null(self$discretize)||self$discretize==0)
          stop("Either the range or the appropriate object must have a discretization parameter")
        return(self$discretize)
      } else {
        return(potential)
      }
    },
    grid = function(self, discretize=NULL, xlim=NULL, ylim=NULL, center=FALSE){
      disc = self$getDiscretize(discretize)
      if(is.null(xlim)){
        x=self$xlim
      } else {
        x=make.lims(xlim)
      }
      if(is.null(ylim)){
        y=self$ylim
      } else {
        y=make.lims(ylim)
      }
      if(center){
        midX = x[[1]] + (disc/2)
        midY = y[[1]] + (disc/2)
        if((midX > x[[2]]) || (midY > y[[2]])) {
          stop("Discretization parameter larger than the range limits.")
        }
        gx = seq(midX,x[[2]], by = disc)
        gy = seq(midY,y[[2]], by = disc)
      } else{
        gx = seq(x[[1]],x[[2]], by = disc)
        gy = seq(y[[1]],y[[2]], by = disc)
      }
      N = as.matrix(expand.grid(gx,gy))
      list(x=gx, y=gy,X0 = N[,1],Y0 = N[,2])
    },
    corners = function(self, discretize=NULL, xlim=self$xlim, ylim=self$ylim){
      self$grid(discretize=discretize, xlim=xlim, ylim=ylim)
    },
    centers = function(self, discretize=NULL, xlim=self$xlim, ylim=self$ylim){
      self$grid(discretize=discretize, xlim=xlim, ylim=ylim, center=TRUE)
    }

  )
}

make.lims <- function(x){
  if(length(x) == 1)
    lim <- c(0,x)
  else
    lim <- c(min(x),max(x))
  lim
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
