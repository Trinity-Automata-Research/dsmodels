#' Adds a new component to a dsmodel.
#'
#' This operator allows you to add objects to a model, or to collect objects before they are added
#' to a model.
#'
#' The operation is fully commutative, so order does not matter.
#' Models that do not yet have ranges will generally save features until the range is provided.
#'
#' Supported features include dsdots, dsarrows, dscurves, dspoints, and colorRegions
#' The \code{+} operator should be the only one required.
#'
#' @include dsproto.R
#' @param e1 A model or feature, usually a model.
#' @param e2 A model or feature, usually a feature.
#' @keywords internal
#' @usage e1 + e2
#' @export
# @method + dsproto
#' @rdname ds-add
"+.dsproto" <- function(e1, e2) {
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))
  if (is.model(e1))       addToModel(e1, e2)
  else if (is.model(e2))  addToModel(e2, e1)
  else if (is.features(e1)) e1$addFeature(e2)
  else if (is.features(e2)) e2$addFeature(e1)
  else features(e1,e2)
}

#' @keywords internal
#' @rdname ds-add
#' @export
"%+%" <- `+.dsproto`


# Purely internal, not exported.
#' @rdname ds-add
# @param model A model
# @param obj A ds object.
#' @keywords internal
#' @export
addToModel <- function(model, obj)  {
  if (is.null(obj)) return(model)
  else if (!is.dsproto(obj)) stop("ModelConst: Can't add a non-dsproto object to a ds model.")
  else if(is.model(obj)) stop("ModelConst: Can't combine two models (yet).")
  else if(is.features(obj)) {
    for (y in obj$features) {addToModel(model, y)}
  }
  else if (is.range(obj) || is.discretizedrange(obj)){
    if (!is.null(model$range)) stop("ModelConst: Can't combine ranges (yet).")
     model$range <- obj
  }
  else if(is.background(obj))
    model$background <- append(model$background, obj)
  else if(is.visualization(obj))
    model$visualization <- append(model$visualization, obj)
  else if(is.facade(obj))
    model$feature <- append(model$feature, obj)

  else {
    stop("ModelConst: Don't know how to add ", class(obj), " to a plot. If range is not discretized, try discretizing the range.",
         call. = FALSE)
  }
  model$render(obj)
  model
}
