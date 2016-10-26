#' xlabel
#'
#' Labels the x-axis
#' @export
#' @param label The title of the axis, to be displayed on image.
#' @param ... Extra parameters. These parameters are fed into \code{mtext()}.
#' @include dsproto.R
#' @examples
#'  library(dsmodels)
#'  fun <- function(X,Y) {
#'  list(
#'      X/exp(Y),
#'      Y/exp(X)
#'   )
#' }
#'
#' model <- dsmodel(fun, title = "Cool Function!")
#' model + dsrange(-2:3,-2:3, discretize = .09) +
#'  xlabel("X-Axis!") +
#'  ylabel("Y-Axis!")


xlabel <- function(label = "", ...) {
  dsproto(
    `_class` = "xaxislabel",
    `_inherit` = facade,
    label = label,
    render = function(self, model) {
      mtext(self$label,side=1, ...)
    }
  )
}

#' ylabel
#'
#' Labels the y-axis
#' @param label The title of the axis, to be displayed on image.
#' @param ... Extra parameters. These parameters are fed into \code{mtext()}.
#' @examples
#'  library(dsmodels)
#'  fun <- function(X,Y) {
#'  list(
#'    X/exp(Y),
#'    Y/exp(X)
#'  )
#'}
#' model <- dsmodel(fun, title = "Cool Function!")
#' model + dsrange(-2:3,-2:3, discretize = .09) +
#'  xlabel("X-Axis!") +
#'  ylabel("Y-Axis!")
#' @export
ylabel <- function(label = "", ...) {
  dsproto(
    `_class` = "yaxislabel",
    `_inherit` = facade,
    label = label,
    render = function(self, model) {
      mtext(self$label,side=2, ...)
    }
  )
}

#' Checks to see if object is xlabel.
#'
#' @param x Checks this object.
#' @keywords internal
#' @export
is.xlabel <- function(x) inherits(x,"xaxislabel")

#' Checks to see if object is ylabel.
#'
#' @param x Checks this object.
#' @keywords internal
#' @export
is.ylabel <- function(x) inherits(x,"yaxislabel")

