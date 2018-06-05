paramrange = function(x=0,y=0,xdiscretize=0,a,b,paramdiscretize=0, renderCount=101, axes = TRUE, frame.plot = TRUE, ...){

  #im not sure how the inherit should work
  ds=dsrange(x,y,xdiscretize, renderCount=renderCount, axes = axes, frame.plot = frame.plot, ...=...)
  alim = make.lims(a)
  blim = make.lims(b)
  dsproto(
    `_class` = "range",
    `_inherit` = ds,
    alim=alim,
    blim=blim,
    paramdiscretize=paramdiscretize,
    getParamDiscretize = function(self, potential) {
      if(is.null(potential)||potential==0){
        if(is.null(self$paramdiscretize)||self$paramdiscretize==0)
          stop("Either the range or the appropriate object must have a discretization parameter")
        return(self$paramdiscretize)
      } else {
        return(potential)
      }
    },
    paramgrid = function(self, discretize=NULL, alim=NULL, blim=NULL, center=FALSE){
      ret=self$parent$grid(discretize,alim,blim,center)
      #rename stuff in grin
      #names(ret)=list("as","bs","A0","B0")
      ret
    },
    paramcorners = function(self, discretize=NULL, alim=self$alim, blim=self$blim){
      self$grid(discretize=discretize, alim=alim, blim=blim)
    },
    paramcenters = function(self, discretize=NULL, alim=self$alim, blim=self$blim){
      self$grid(discretize=discretize, alim=alim, blim=blim, center=TRUE)
    }
  )


}

dsrange <- function(x,y,discretize = 0,
                    renderCount=101, axes = TRUE, frame.plot = TRUE, ...){ # Range
  xlim <- make.lims(x)
  ylim <- make.lims(y)
  dsproto(
    `_class` = "range",
    `_inherit` = facade,
    discretize = discretize,
    dims = 2,
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
      self$paramgrid(discretize=discretize, xlim=xlim, ylim=ylim)
    },
    centers = function(self, discretize=NULL, xlim=self$xlim, ylim=self$ylim){
      self$paramgrid(discretize=discretize, xlim=xlim, ylim=ylim, center=TRUE)
    }

  )
}
