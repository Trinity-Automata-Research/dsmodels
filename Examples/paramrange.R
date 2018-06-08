paramrange = function(a,b,x=0,y=0, paramNames=NULL,discretize=0, renderCount=101, axes = TRUE, frame.plot = TRUE, ...){
  #im not sure how the inherit should work
  ds=dsrange(x,y,discretize, renderCount=renderCount, axes = axes, frame.plot = frame.plot, ...=...)
  alim = make.lims(a)
  blim = make.lims(b)
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
    #returns potential or paramdisc or disc, depeding on which one exists. right now paramdisc and disc are the same so this is the same as paramdisc
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
      #rename stuff in grin
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
        return(NULL)#stop("AAAAA")
      if(any(c("x", "X") %in% params) && any(c("y","Y") %in% params)) {
        modelParams = setdiff(params, c("x", "X", "y", "Y"))
        if(length(modelParams)<2){
          return(NULL)
        }
        #dsassert(length(modelParams)>=2, "AAAAAHMORE")
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



