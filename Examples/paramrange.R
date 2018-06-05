paramrange = function(a,b,paramDiscretize=0, paramNames=NULL,x=0,y=0,xdiscretize=0, renderCount=101, axes = TRUE, frame.plot = TRUE, ...){



  #im not sure how the inherit should work
  ds=dsrange(x,y,xdiscretize, renderCount=renderCount, axes = axes, frame.plot = frame.plot, ...=...)
  alim = make.lims(a)
  blim = make.lims(b)
  dsproto(
    `_class` = "paramrange",
    `_inherit` = ds,
    alim=alim,
    blim=blim,
    paramDiscretize=paramDiscretize,
    on.bind=function(self,model){
      givenNames = substitute(paramNames)
      #for some reason substitute(paramnames) gives the symbol paramnames instead of null now.
      if(is.null(givenNames)||givenNames==quote(paramNames)) {
        paramNames = model$getParamNames()
        self$aname <- paramNames[1]
        self$bname <- paramNames[2]
      } else {
        self$aname <- as.character(givenNames[2])
        self$bname <- as.character(givenNames[3])
      }

    },
    render = function(self, model) {
      self$rendered = TRUE
      plot(0, type = "l", lwd = 3, axes=self$axes, main = model$title,
           xlab = "", ylab = "", xlim = self$alim, ylim = self$blim,
           frame.plot = self$frame.plot)
    },
    getParamDiscretize = function(self, potential) { #returns potential or paramdisc or disc, depeding on which one exists
      if(is.null(potential)||potential==0){
        if(is.null(self$paramDiscretize)||self$paramDiscretize==0)
          return(self$parent$getDiscretize(potential))
        return(self$paramDiscretize)
      } else {
        return(potential)
      }
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
    }
  )
}

is.paramrange <- function(x) inherits(x,"paramrange")



