#' Defines a model object encapsulating a dynamical system
#'
#' To begin, define a two dimensional function that outputs
#' a two-dimensional list. For instance:
#' \code{fun <- function(x,y) list(x,y)}. Then
#' \code{dsmodel(fun)} will initialize the model.
#' \strong{Make sure that the function defining your model indeed outputs a \code{list}.}
#' The model is used to hold the data for graphics.
#' To display a desired graphic, add the corresponding
#' feature of type "dsproto" to your model.
#'
#' Models are constructed
#' incrementally using the + operator to add features
#' to the existing dsmodel object. A \code{\link{dsrange}} must be one of the objects added to a model.
#' @family Foundation
#' @param fun Function with two inputs and two outputs which defines the dynamical system. The output should be a list, preferably with field names x and y.
#' @param title A string title for the graph.
#' @import grDevices
#' @include dsproto.R
#' @seealso \code{\link{dsrange}}
#' @export
#' @examples
#' library(dsmodels)
#'
#' fun <- function(X,Y) {
#'   list(
#'     x = X/exp(Y),
#'     y = Y/exp(X)
#'   )
#' }
#' # Add dsRange to see the actual range.
#' model <- dsmodel(fun)
#'
#' dsmodel(function(x,y) {
#'   list(
#'     x = x^2,
#'     y = x/(y+1)
#'   )
#' }, title = "Another function!")
dsmodel <- function(fun, title="") {
  if(length(formals(fun)) != 2)
    stop("dsmodel: Please make sure your function has 2 distinct, variable inputs.")
  dsproto(
    `_class` = "model",
    `_inherit` = NULL,
    fun = fun,
    title = title,
    dim = 2,
    range = NULL,
    background = c(),
    feature = c(),
    visualization = c(),
    dots = c(),
    print = function(self, ...) {
      invisible(self)
    },
    apply = function(self, x, y, iters=1, accumulate=TRUE, crop = TRUE) {
      if(is.null(x) || is.null(y))
        stop("dsmodel: Please make sure your x and y values are defined in latest object created.")
      tmp = self$fun(x[1], y[1])
      if(length(tmp) != 2)
        stop("dsmodel: Please make sure your function outputs a list with 2 values. (for example: list(x+1,y^2)")
      if(is.element("x", names(tmp)) && is.element("y", names(tmp)))
        properNames = TRUE
      else {
        properNames = FALSE
        if(!is.null(names(tmp)))
          warning("dsmodel function has outputs names that are not \"x\" and \"y\". Assuming first output is x, and second is y.")
      }
      if(accumulate) {
        iterAccum = vector("list",iters+1)
        startvals = list(x=x, y=y)
        if(crop){
          startvals <- self$cropframe(startvals)
        }
        iterAccum[[1]] <- startvals
        for(i in 1:iters){
          tmp=self$fun(x,y)
          if(any(is.nan(tmp[[1]])) || any(is.nan(tmp[[2]])))
          {
            warning("dsmodel: model undefined, NaN computed. (Division by zero). Removing points with NaN value and continuing procedure..")
            tmp = NaNRemove(tmp)
          }
          if(crop){
            tmp = self$cropframe(tmp)
          }
          if(!properNames)
            names(tmp) <- c("x","y")
          x = tmp$x
          y = tmp$y
          iterAccum[[i+1]] <- tmp
        }
        iterAccum
      } else {
        for(i in 1:iters){
          tmp=self$fun(x,y)
          if(crop){
            tmp <- self$cropframe(tmp)
          }
          if(!properNames)
            names(tmp) <- c("x","y")
          x = tmp$x
          y = tmp$y
          x = tmp[[1]]
          y = tmp[[2]]
        }
        if(!properNames)
          names(tmp) <- c("x","y")
        tmp
      }
    },
    cropframe = function(self, vals) {
      xin <- vals[[1]]
      yin <- vals[[2]]
      tmp1 <- self$range$xlim
      tmp2 <- self$range$ylim
      filterfunx <- function(x) (x <= max(tmp1) && x >= min(tmp1))
      filterfuny <- function(x) (x <= max(tmp2) && x >= min(tmp2))
      xrid = boolpos(filterfunx, xin)
      yrid = boolpos(filterfuny, yin)
      rid <- c(xrid,yrid)
      if(!is.null(rid)) {
        list(
          x = xin[-rid],
          y = yin[-rid]
        )
      }
      else {
        list(
          x = xin,
          y = yin
        )
      }
    },
    render = function(self, obj = NULL) {
      rerender = FALSE
      if(is.null(obj))
        rerender = TRUE
      if(is.range(obj)) {
        if(!is.range(self$range))
          stop("Range not added properly: severe error. Please notify developers.")
        if(!self$range$rendered)
          rerender = TRUE
      }
      else if (!is.null(self$range)) {
        if(is.feature(obj) ||
          (is.visualization(obj) && is.null(self$feature)) ||
          (is.background(obj) && is.null(self$visualization)  && is.null(self$feature))) {
          obj$render(model = self)
        } else {
          rerender = TRUE
        }
      }
      if(rerender) {
        self$range$render(model = self)
        for(bg in self$background)
          bg$render(model = self)
        for(vi in self$visualization)
          vi$render(model = self)
        for(fe in self$feature)
          fe$render(model = self)
      }
    },
    recalculate = function(self) {
      if(!is.null(self$range)) {
        if(is.range(self$range)) {
          self$range$render(model = self)
        }
        if(!is.null(self$background)) {
          for(ba in self$background)
            ba$recalculate(model = self)
        }
        if(!is.null(self$visualization)) {
          for(vi in self$visualization)
            vi$recalculate(model = self)
        }
        if(!is.null(self$feature)) {
          for(fe in self$feature)
            fe$recalculate(model = self)
        }
      }
    }
  )
}
#' Checks if object is a mode.
#'
#' @param x Object to check
#' @export
#' @keywords internal
#' @usage NULL
is.model <- function(x) inherits(x,"model")

#' Set Color Vector From Parameters
#'
#' Outputs a color vector.
#' If the combined length of \code{col} and \code{image} is  2, and \code{iters} is greather than 1,
#' this function creates a gradient from first specified color to the second. The length of said
#' vector will be equal to \code{iters}.
#' If the combined length of \code{col} and \code{image} is greater than two, then the output will
#' be a vector of whatever was input into \code{col} and \code{image} with output length being
#' \code{min(length(col) + length(image), iters)}.
#' @param col A vector or list of colors
#' @param image A vector or list of colors
#' @param iters A number indicating the number of iterations desired.
#'  If set to greater than 1, \code{iters} becomes the length of the output.
#' @keywords internal
#' @export
colorVector <- function(col, image, iters) {
  if(is.null(image))
    image <- ""
  if((length(col) == 2 && iters > 1 && image == "") # GRADIENT
     || (length(col) == 1 && length(image) == 1 && iters > 1)){
    if(!is.null(image)) {
      if(image != "")
        col <- append(col,image)
    }
    colf <- colorRamp(col)
    colptr <- seq(0,1, length.out = iters+1)
    vect <- vector(mode = "list", iters+1)
    for(i in 1:(iters+1)) {
      tmp <- colf(colptr[[i]])
      vect[[i]] = rgb(red = tmp[1],
                green = tmp[2],
                blue = tmp[3], maxColorValue = 255)
    }
    vect
  }
  else{ # NOT GRADIENT
    if(image != "" || is.null(image))
      col <- append(col, image)
    lengthOfCol <- length(col)
    len <- min(lengthOfCol, iters)
    if( iters <= 1 ) len <- lengthOfCol
    vect <- vector(mode = "list", len)
    for(i in 1:len){
      if(col[[i]]=="")
        col[[i]] = "NA"
      vect[[i]] <- col[[i]]
    }
    vect
  }
}

#' Find Positions of False Elements
#'
#' Returns all positions of elements in \code{v} where \code{fun(v)}
#' returns \code{FALSE}.
#' @param fun A \code{function(x)} which outputs a single boolean
#' @param v A vector or list of elements compatible with \code{fun}
#' @keywords internal
#' @export
boolpos <- function(fun, v) {
  holder <- c()
  tracker <- 1
  if(length(v) <= 0)
    return(holder)
  for(i in 1:length(v)) {
    val <- fun(v[[i]])
    if(!is.na(val) && !is.nan(val)) {
      if(!val) {
        holder[[tracker]] <- i
        tracker <- tracker+1
      }
    }
  }
  holder
}

#' NaNRemove
#'
#' Finds and removes all 2-d points which produce NaNs
#' @keywords internal
#' @param twoDList Data of form \code{list(list(),list())}.
#' @export
NaNRemove <- function(twoDList){
  tmp <- twoDList
  if(length(tmp[[1]])!=length(tmp[[2]])){
    stop("dsmodel: X and Y of different length. Internal Error.")
  }
  ridFun <- function(x) !is.nan(x)
  toRidX <- boolpos(ridFun,tmp[[1]])
  toRidY <- boolpos(ridFun,tmp[[2]])
  rid <- c(toRidX,toRidY)
  if(!is.null(rid)) {
    list(
      tmp[[1]] <- tmp[[1]][-rid],
      tmp[[2]] <- tmp[[2]][-rid]
    )
  }
}
