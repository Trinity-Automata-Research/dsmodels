#' Range of parameters for a model
#'
#' \code{paramrange} creates a discrete or continuous
#' range of parameters and inputs for the model to be computed over.
#'
#' You may either specify a numeric a, b, x and y, in which case 0 is the lower bound and that value is
#' the upper bound; or a range of values, in which case the min and the max of the range will be used.
#' To specify a range from \code{min} to \code{max}, use either \code{c(min,max)} or
#' \code{min:max}.
#' @include dsproto.R dsrange.R
#' @param alim Specifies the minimum and maximum for the first parameter.
#'  If only one value is specified, it is used as the maximum, and the minimum will default to 0.
#'  If a collection of values are provided, the minimum and maximum are used as the range.
#' @param blim Specifies the minimum and maximum for the second parameter.
#'  If only one value is specified, it is used as the maximum, and the minimum will default to 0.
#'  If a collection of values are provided, the minimum and maximum are used as the range.
#' @param xlim Specifies the minimum and maximum for the x axis.
#'  If only one value is specified, it is used as the maximum, and the minimum will default to 0.
#'  If a collection of values are provided, the minimum and maximum are used as the range.
#' @param ylim Specifies the minimum and maximum for the y axis.
#'  If only one value is specified, it is used as the maximum, and the minimum will default to 0.
#'  If a collection of values are provided, the minimum and maximum are used as the range.
#' @param paramNames Specifies the names of parameters to be varied. Expects a vetcor of
#'  either the names of the variables, or strings containing the names. If left blank, a geuss will be
#'  made for which arguments of the function are the parameters to vary, usualy the third and fourth arguments.
#' @param discretize If a value is provided, the field is discretized into an array of points. The value
#'  specifies the distance between each point.
#'  This becomes the default when displaying \code{\link{sim-map-period}}
#' @param axes If \code{FALSE}, the axes will not be drawn. Defaults to \code{TRUE}.
#' @param frame.plot If \code{FALSE}, the frame of the plot will not be drawn. Defaults to \code{TRUE}.
#'  \deqn{(xmax-xmin+1)(ymax-ymin+1)/discretize.}{ascii}
# @param originOffset Currently not supported. Allows you to place an xlim and ylim
#  for the graph without including extra discretized space.
#' @param renderCount The number of points that a curve will be computed at when being
#'   displayed. Default 101.
#' @param ... Further fields for the dsrange object.
#' @seealso \code{\link{dsmodel}}
#' @seealso \code{\link{sim-map-period}}
#' @export
#' @examples
#' fun <- function(X,Y,a,b) {
#'   list(
#'     a*X/exp(Y),
#'     b*Y/exp(X)
#'   )
#' }
#' model <- dsmodel(fun, title = "A range with no features!")
#' #Since no features are added, only the area and title are displayed.
#' model + paramrange(3, 3, discretize = .09, paramNames=c(a,b))

paramrange = function(alim,blim,xlim=0,ylim=0, paramNames=NULL,discretize=0, renderCount=101, axes = TRUE, frame.plot = TRUE, ...){
  ds=dsrange(xlim,ylim,discretize, renderCount=renderCount, axes = axes, frame.plot = frame.plot, ...=...)
  alim = make.lims(alim)
  blim = make.lims(blim)
  givenNames = substitute(paramNames)
  if(safe.apply(is.null,paramNames)) {
    aname <- NULL
    bname <- NULL
  } else if(length(givenNames) == 3 && givenNames[1] == substitute(c())) {
    aname <- as.character(givenNames[2])
    bname <- as.character(givenNames[3])
  } else {
    aname <- paramNames[1]
    bname <- paramNames[2]
  }
  dsproto(
    `_class` = "paramrange",
    `_inherit` = ds,
    alim=alim,
    blim=blim,
    aname=aname,
    bname=bname,
    discretize=discretize,
    on.bind=function(self,model){
     if(is.null(self$aname)){ #if aname is null use model$fornals in getparamsfunc() to guess aname
        paramNames <- self$paramsOfFormals(model$funParams)
        if(!is.null(paramNames))
        {
          self$aname <- paramNames[1]
          self$bname <- paramNames[2]
        }
     }
    },
    render = function(self, model) {
      self$rendered = TRUE
      plot(0, type = "l", lwd = 3, axes=self$axes, main = model$title,
           xlab = "", ylab = "", xlim = self$alim, ylim = self$blim,
           frame.plot = self$frame.plot)
    },
    #returns potential or paramdisc or disc, depeding on which one exists.
    #right now paramdisc and disc are the same so this is the same as paramdisc
    #only needed if paramdisc if different from disc
    getParamDiscretize = function(self, potential) {
    #  if(is.null(potential)||potential==0){
    #    if(is.null(self$discretize)||self$discretize==0)
    #      return(self$parent$getDiscretize(potential))
    #    return(self$discretize)
    #  } else {
    #    return(potential)
    #  }
      self$parent$getDiscretize(potential)
    },
    paramgrid = function(self, discretize=NULL, alim=NULL, blim=NULL, center=FALSE){
      if(is.null(alim))
        alim=self$alim
      if(is.null(blim))
        blim=self$blim
      ret=self$grid(self$getParamDiscretize(discretize),alim,blim,center)
      #rename stuff in grid? part of issue #139
      #names(ret)=list("as","bs","A0","B0")
      ret
    },
    paramcorners = function(self, discretize=NULL, alim=self$alim, blim=self$blim){
      self$paramgrid(discretize=discretize, alim=alim, blim=blim)
    },
    paramcenters = function(self, discretize=NULL, alim=self$alim, blim=self$blim){
      self$paramgrid(discretize=discretize, alim=alim, blim=blim, center=TRUE)
    },
    paramsOfFormals = function(fformals) { #returns list of up to 4 parameter names-x,y,a,b
      allparams = names(fformals)
      defaults = unlist(lapply(allparams, function(k) { hasDefault(fformals[[k]]) }))
      openParams = allparams[!defaults]
      if(length(openParams) == 4)
        params = openParams
      else if (length(allparams) >= 4)
        params = allparams
      else
        return(NULL)
      if(any(c("x", "X") %in% params) && any(c("y","Y") %in% params)) {
        modelParams = setdiff(params, c("x", "X", "y", "Y"))
        if(length(modelParams)<2){
          return(NULL)
        }
        modelParams[1:2]
      } else {
        params[3:4]
      }
    }
  )
}

#' Checks if object is a paramrange
# @rdname dsproto
#' @param x An object to test
#' @keywords internal
#' @export
is.paramrange <- function(x) inherits(x,"paramrange")

#' Checks if a formal has a default
#' @param x an element of the return value of formals()
#' @keywords internal
#' @export
hasDefault = function(x) {
  return(!is.name(x) || nzchar(x))
}



