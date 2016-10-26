#' Features
#'
#' Defines a category for the different kinds of non-model dsprotos
#' @keywords internal
#' @param f1 dsproto of type feature
#' @param f2 dsproto of type feature
#' @export
features <- function(f1,f2){
    dsproto(
      "features", NULL,
      features = c(f1,f2),
      addFeature = function(self, obj) {
        if (is.features(obj))
          self$features <- append(self$features, obj$features)
        else
          self$features <- append(self$features, obj)
        self
        }
      )
}

is.features <- function(obj) {
  inherits(obj,"features")
}
