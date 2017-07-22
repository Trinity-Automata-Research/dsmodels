#' Create a label on the x-axis
#'
#' Labels the x-axis
#' @export
#' @param label The title of the axis, to be displayed on image. Text can be input in the form of pseudo-LaTeX code within quotes.
#'  See \code{\link[latex2exp]{TeX}} for more details.
#' @param line The distance from the axis the text will be displayed.
#' @param ... Extra parameters. These parameters are fed into \code{mtext()}.
#' @import latex2exp
#' @include dsproto.R
#' @seealso \code{\link[latex2exp]{TeX}}
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
#'  xlabel("X-Axis shows $\\alpha$!") +
#'  ylabel("Y-Axis shows $\\beta$!")


xlabel <- function(label = "", line = 3, ...) {
  texLabel <- TeX(label)
  dsproto(
    `_class` = "xaxislabel",
    `_inherit` = facade,
    label = texLabel,
    line = line,
    render = function(self, model) {
      mtext(self$label,side=1, line = self$line, ...)
    }
  )
}

#' Create a label on the y-axis
#'
#' Labels the y-axis
#' @param label The title of the axis, to be displayed on image. Text can be input in the form of pseudo-LaTeX code within quotes.
#'  See \code{\link[latex2exp]{TeX}} for more details.
#' @param line The distance from the axis the text will be displayed.
#' @param ... Extra parameters. These parameters are fed into \code{mtext()}.
#' @seealso \code{\link[latex2exp]{TeX}}
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
#'  xlabel("$X$-Axis shows $\\alpha$!") +
#'  ylabel("$Y$-Axis shows $\\beta$!")
#' @export
ylabel <- function(label = "", line = 3, ...) {
  texLabel <- TeX(label)
  dsproto(
    `_class` = "yaxislabel",
    `_inherit` = facade,
    label = texLabel,
    line = line,
    render = function(self, model) {
      mtext(self$label,side=2, line = self$line, ...)
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

