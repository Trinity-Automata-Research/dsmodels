#' Find and display the isoclines of a model through simulation
#'
#' Attempts to find the isoclines, i.e., the lines at which the change in x or y is 0, for a model. The plane
#' is broken into a grid, and the model's underlying function is used together with one dimensional optimization
#' to attempt to find a point in each section of the grid where change in x or y is within \code{tolerance}.
#' A \code{dsrange} object must be composed to the model before simisoclines.
#' @include dsproto.R shadowtext.R dscurve.R
#' @param xInterval,yInterval The intervals at in which to search for points on the isoclines. Defualts to the limits of the range. Pass in as \code{c(min, maxx)}
#' @param n A vector of integers representing the number of segments that the vertical and horizontal ranges will be split into. Enter as \code{c(xn, yn)}. Defaults to \code{c(200, 200)}. Higher values may degrade performance.
#' @param cols A vector of color given as \code{c(xCol, yCol)} denoting the color of the x isoclines and y isocline respectively. Defaults to black.
#' @param lwd A vector of line widths expressed as doubles. Enter as \code{c(xlwd, ylwd)}
#' @param label A vector of strings representing the labels for each curve. Enter as \code{c(xlabel, ylabel)}
#' @param labelLoc A vector of doubles between 0 and 1 denoting at what fraction of the way through the isoclines the labels should be displayed. Enter as \code{c(xLoc, yLoc)}. Defaults to 0.5.
#' @param labelOffset A vector \code{c(x, y)} offsetting the label from the isocline. Defaults to an automatic scale dependent on the dsrange's y axis size. To enter custom values, enter as \code{list(c(iso1_x_offset, iso1_y_offset), c(iso2_x_offset, iso2_y_offset))}.
#' @param labelCol A vector of string colors denoting the labels' text colors. Defaults to black.
#' @param labelBg A vector of string colors denoting the color of the labels' background shadows. Defaults to white. Use \code{"c(NA, NA)"} or \code{c("", "")} to remove the shadow.
#' @param stretch A vector representing how discontinuities will be broken up. See the "Breaking Discontinuities" section in dscurve
#' @param tolerance The distance at which a point is considered to have stopped moving. Defaults to \code{sqrt(.Machine$double.eps)}.
#' @param ... Further graphical parameters passed to \code{lines} or \code{points}.
#' @import stats
#' @export

simisoclines <- function(xInterval = NULL, yInterval = NULL, n = c(200, 200), cols = c("Black", "Black"),
                         lwd = 3, label = NULL, labelLoc = c(0.5, 0.5), stretch = 0,
                         labelOffset = NULL, labelCol = c("Black", "Black"), labelBg = c("White", "White"),
                         tolerance = 0.0001) {
  texLabel <- sapply(label, TeX)
  dsproto(`_class` = "simisoclines", `_inherit` = feature,
          xInterval = xInterval,
          yInterval = yInterval,
          n = n,
          cols = cols,
          lwd = lwd,
          label = texLabel,
          hasLabel = !is.null(label),
          labelLoc = labelLoc,
          stretch = stretch,
          labelOffset = labelOffset,
          labelCol = labelCol,
          labelBg = labelBg,
          tolerance = tolerance,
          isoclines = function(self, diffFunc, xInterval, yInterval, n) {
            xVals <- seq(xInterval[1], xInterval[2], length.out = n)
            self$xValues <- xVals
            xRanges <- vector("list", n - 1)
            for(i in seq(1, n - 1)) xRanges[[i]] <- c(xVals[i], xVals[i + 1])
            yVals <- seq(yInterval[1], yInterval[2], length.out = n)
            self$yValues <- yVals
            iso <- list(x = vector("list", length = n - 1), y = vector("list", length = n - 1))
            for(i in seq(1, n - 1)) {
              min <- vector(length = n)
              obs <- vector(length = n)
              for(j in seq(1, n)) {
                opt <- optimize(diffFunc, xRanges[[i]], yVals[j])
                min[j] <- opt$minimum
                obs[j] <- opt$objective
              }
              lowest <- Reduce(function(a, b) if(obs[a] <= obs[b]) a else b, seq(1:n))
              if(obs[lowest] <= tolerance) {
                iso$x[[i]] <- min[lowest]
                iso$y[[i]] <- yVals[lowest]
              }
            }
            list(x = unlist(iso$x), y=unlist(iso$y))
          },
          on.bind = function(self, model) {
            self$bound = TRUE
            self$model = model
            if(is.null(xInterval)) self$xInterval <- model$range$xlim
            if(is.null(yInterval)) self$yInterval <- model$range$ylim
            self$xDiffFunc = function(x, y) abs(model$fun(x, y)[[1]] - x)
            self$yDiffFunc = function(x, y) abs(model$fun(x, y)[[2]] - y)
            self$xIsocline = breakDisconts(self$isoclines(self$xDiffFunc, self$xInterval, self$yInterval, self$n[1]),
                                           self$xInterval, self$yInterval, stretch = self$stretch)
            self$yIsocline = breakDisconts(self$isoclines(self$yDiffFunc, self$xInterval, self$yInterval, self$n[2]),
                                           self$xInterval, self$yInterval, stretch = self$stretch)
          },
          displayLabel = function(self, range) {
            if(is.null(labelOffset)) {
              scale <- 0.08*(abs(max(range$ylim) - min(range$ylim)))
              self$labelOffset=list(c(0,scale), c(0, scale))
            }
            xloc <- c(self$xIsocline$x[labelLoc[1] * length(self$xIsocline$x)] + self$labelOffset[[1]][1],
                      self$yIsocline$x[labelLoc[2] * length(self$yIsocline$x)] + self$labelOffset[[2]][1])
            yloc <- c(self$xIsocline$y[labelLoc[1] * length(self$xIsocline$y)] + self$labelOffset[[1]][2],
                      self$yIsocline$y[labelLoc[2] * length(self$yIsocline$y)] + self$labelOffset[[2]][2])
            mapply(shadowtext, xloc, yloc, self$label, self$labelCol, self$labelBg)
          },
          render = function(self, model) {
            mapply(lines, x = list(self$xIsocline$x, self$yIsocline$x), y = list(self$xIsocline$y, self$yIsocline$y), lwd = self$lwd, col = self$cols)
            if(self$hasLabel) self$displayLabel(model$range)
          })
}
