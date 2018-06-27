#' Parametric curves or a graph of functions
#'
#' This function takes a description of a curve and creates an object displaying the curve, and optionally
#' it's behavior throughout iterations of the system. Functions can be provided as expressions of \code{x},
#'for graphing curves, or \code{t}, for parametric curves.
#' The curve is defined either by the graph of a single function or a pair of parametric
#' equations. By default, rendered with the \code{lines} function.
#'
#' @section The graph of a function:
#'
#' If the parameter \code{yfun} is not provided, then \code{dscurve} contains
#' the curve of points (x,fun(x)). The inputs to \code{fun} are \code{n} points between the maximum
#' \code{\link{dsrange}}'s x limits, but can be overwritten with the \code{xlim} parameter.
#' \code{fun} can either be any function of a single parameter, or an expression with exactly \code{x} as the free variable.
#'
#' @section  Parametric equations:
#'
#' If the parameter \code{fun} and \code{yfun} are both provided,
#' \code{dscurve} contains the parametric curve described by the functions. The function is
#' calculated at \code{n}
#' points ranging from \code{tmin} to \code{tmax}.
#' \code{fun} and \code{yfun} can either be any function of a single parameter, or an expression with exactly \code{t} as the free variable.

#'
#' @section Images of curves:
#'
#' The \code{dscurve} object begins with an initial curve. Images of the curve may be displayed in three ways.
#' If the \code{image} parameter is a single color and \code{iters} is not set, then \code{dscurve} will calculate and display
#' the image of the curve under the model's function in that color.
#'
#' If the \code{image}  parameter is a vector of k colors, then \code{dscurve} calculates and
#' displays k successive images of the curve using those colors.
#' The string "NA" may be used to avoid displaying an iteration.
#'
#' If the \code{image} parameter is a single color and \code{iters} is defined, then \code{iters}
#' successive images are displayed, using a gradient between \code{col} and \code{image}.
#'
#' In most cases, rather than specifying \code{col} and \code{image} separately, they may be
#' combined into a single vector.
#'
#' @include dsproto.R
#' @param fun A function. If \code{yfun} is provided, this is the x-equation of the parametric
#' equations. If not, the function's graph is rendered.
#' See sections describing graphs and parameteric equations for more info.
#' @param yfun The y-equation of the parameteric equations.
#' See sections describing parametric equations for more info.
#' @param iters Determines the number of iterations of the function when making a color gradient.
#' Use \code{col = color1, image = color2, iters = n} to create a gradient of colors between
#' color1 and color2. See details for more information.
#' @param col The color of the original curve, as a string.
#' @param image A single color as a string, or a vector of colors as a string.
#'  See details for more information.
#' @param lwd Line width expressed as a double. Only used if \code{discretize} is not set.
#' @param n The number of points that will be calculated.
#'	Defaults to the dsrange's \code{renderCount}.
#'	\code{n} is used to interact with \code{discretize}.
#' @param tstart Only used for parametric curves. The minimum input
#'   for both functions. Default 0.
#' @param tend Only used for parametric curves. The maximum input
#'	for the functions. Default 1.
#' @param xlim Only used for the graph of a function. Determines the range of x values for which the function
#' is plotted. Defaults to the x limits of the model's dsrange.
#' @param crop If \code{crop==TRUE}, the original curve and all iterations are cropped to the range.
#' @param discretize Set \code{discretize=TRUE} to display the calculated points, instead of
#' connecting them as a curve: the curve is displayed with \code{points}
#' instead of \code{lines}.
#' @param ... Further graphical parameters passed to \code{lines} or \code{points}.
#' @seealso \code{\link{dspoint}}
#' @import pryr
#' @examples
#' library(dsmodels)
#'
#' fun <- function(X,Y) {
#'   list(
#'     X/exp(Y),
#'     Y/exp(X)
#'   )
#' }
#'
#' model <- dsmodel(fun, title = "Points on a One-Dimensional Curve")
#' range <- dsrange(-2:2,-2:2, discretize = 0.5)
#'
#' # Add the graph of a function and its image in blue.
#' graphcrv <- dscurve(function(x) x^2,
#'                     col = "orange",
#'                     image = "blue",
#'                     discretize = TRUE,
#'                     xlim = c(-2,2))
#' model + range +	graphcrv
#' # Add the graph of expression of x.
#' model + dscurve(x^2+1, col="yellow")
#'
#' # Create a parametric curve with image iterations red then green.
#' paramcrv <- dscurve(function(t) t^2, function(t) t,
#'                     image = c("red", "green"),
#'                     tstart = -2, tend = 2)
#' dsmodel(fun, "A Parametric Curve and Iterations of that Curve") +
#'   dsrange(-2:2, -2:2, discretize = 0.5) +
#' # A parametic curve defined by expressions of t.
#'   paramcrv + dscurve(4*t-2,4*t-2,col="blue")
#'
#' @export
dscurve <- function(fun, yfun = NULL,
                    col = "black", image = NULL,
                    lwd = 3, n=NULL, iters = 0, simPeriod=FALSE,
                    testX=.1, testY=.1,
                    crop = FALSE,  tstart=0, tend=1,
                    discretize=FALSE, xlim = NULL, display=TRUE,
                    ...) {

  colors <- colorVector(col, image, iters)
  iters <- length(colors)-1



  if(!safe.apply(is.null,yfun)){
    xfunc <- ensureFunction(substitute(fun), TRUE)
    yfunc <- ensureFunction(substitute(yfun), TRUE)
    if(simPeriod){
      dscurveSim(getX = xfunc, getY = yfunc,
               colors = colors, testX=testX, testY=testY, lwd = lwd,
               n = n, iters = iters, simPeriod=simPeriod, discretize = discretize,
               lims=c(tstart,tend), display, ...)
    }
    else{
      dscurveParam(xfun = xfunc, yfun = yfunc,
                   colors = colors, lwd = lwd,
                   n = n, iters = iters, crop, discretize = discretize,
                   tstart = tstart, tend = tend, display,
                   ...)
    }
  } else {
    func <- ensureFunction(substitute(fun), FALSE)
    if(simPeriod){
      dscurveSim(getX=identity , getY = func, colors = colors,  testX=testX, testY=testY,
               lwd = lwd, n = n, iters = iters,  simPeriod=simPeriod, discretize = discretize,
               lims = xlim, display=display, ...)
}
    else{
      dscurveGraph(fun = func, colors = colors,
                   lwd = lwd, n = n, iters = iters, discretize = discretize,
                   crop, xlim = xlim, display, ...)
    }

  }
}

dscurveParam<- function(xfun, yfun, colors, lwd, n, tstart=0, tend=1,
                        iters, crop = TRUE, discretize = FALSE, display, ...){
  if(is.null(n))
    renderInputs = NULL
  else
    renderInputs = seq(tstart, tend, length.out=n)
  dsproto(
    `_class` = "curve", `_inherit` = feature,
    xfun = xfun,
    yfun = yfun,
    col = colors,
    iters = iters,
    tstart = tstart,
    tend=tend,
    toPlot = NULL,
    lwd = lwd,
    renderInputs = renderInputs,
    crop = crop,
    discretize = discretize,
    display = display,
        ... = ...,
    on.bind = function(self, model) {
      if(is.null(model$range)) stop("dscurve: Add range first")
      if(is.null(self$renderInputs))
        tValues = seq(self$tstart,self$tend,length.out=model$range$renderCount)
      else
        tValues = self$renderInputs
      self$toPlot <- model$apply(self$xfun(tValues), self$yfun(tValues),
                                 iters = self$iters, crop = self$crop)
    },
    render = function(self, model) {
      if(display){
        if(self$discretize){
          for(i in 1:(self$iters+1))
            points(self$toPlot[[i]]$x, self$toPlot[[i]]$y,
                   col = self$col[[i]], ... = self$...)
        }
        else{
          for(i in 1:(self$iters+1))
            lines(self$toPlot[[i]]$x, self$toPlot[[i]]$y, lwd = self$lwd,
                  col = self$col[[i]], ... = self$...)
        }
      }
    }
  )
}


dscurveGraph <- function(fun, colors, lwd, n, iters,
                            crop = FALSE, discretize = FALSE,
                            xlim = NULL, display, ...){
  dsproto(
    `_class` = "curve", `_inherit` = feature,
    fun = fun,
    col = colors,
    lwd = lwd,
    iters = iters,
    n = n,
    xValues = NULL,
    yValues = NULL,
    toPlot = NULL,
    xlim = xlim,
    discretize = discretize,
    crop = crop,
    display = display,
    ... = ...,
    on.bind = function(self, model) {
      self$bound = TRUE
      if(is.null(self$n))
        numPoints <- model$range$renderCount
      else
        numPoints <- self$n
      if(is.paramrange(model$range)){
        from=min(model$range$alim)
        to=max(model$range$alim)
      }
      else{
        from=min(model$range$xlim)
        to=max(model$range$xlim)
      }
      if(!is.null(xlim)){
        from=max(from,min(xlim))
        to=min(to,max(xlim))
      }
      self$xValues <-seq(from,to, length.out = numPoints)
      self$yValues <- mapply(self$fun,self$xValues)
      self$toPlot <- model$apply(self$xValues, self$yValues, iters=self$iters, crop = self$crop)
    },
    render = function(self, model) {
      if(display){
        if(self$discretize){
          for(i in 1:(self$iters+1))
            points(self$toPlot[[i]]$x, self$toPlot[[i]]$y,
                  col = self$col[[i]], ... = self$...)
        }
        else{
          for(i in 1:(self$iters+1))
            lines(self$toPlot[[i]]$x, self$toPlot[[i]]$y, lwd = self$lwd,
                  col = self$col[[i]], ... = self$...)
        }
      }
    }
  )
}
dscurveSim= function(getX, getY, colors, testX, testY, lwd, n, iters,  simPeriod,
                   discretize = FALSE,
                   lims = NULL, display, ...){
  dsproto(
  `_class` = "curve", `_inherit` = feature,
  getX=getX,
  getY=getY,
  col = colors,
  testX=testX, testY=testY,
  lwd = lwd,
  iters = iters,
  simPeriod=simPeriod,
  n = n,
  sources = NULL,
  xValues = NULL,
  yValues = NULL,
  toPlot = NULL,
  discretize = discretize,
  lims=lims,
  display = display,
  ... = ...,
  #functions to interact with the model
  makeSourceSeq= function(self, model){ #if we have a prange pull from alim. if not pull from xlim. previous assert should make sure we have the right one.
    if(is.null(self$n))
      numPoints <- model$range$renderCount
    else
      numPoints <- self$n
    if(is.null(self$lims)){
      if(is.paramrange(model$range))
        self$lims=model$range$alim
      else
        self$lims=model$range$xlim
    }
    else
      self$lims=make.lims(self$lims) #when done should be move to before dsproto is called and only run if using xlim and not tstart and tend
    from=min(self$lims)
    to=max(self$lims)
    seq(from,to, length.out = numPoints)
  },
  on.bind = function(self, model) { #do common stuff, check if sim. if so, assert range is prange, do this \/, if not do other curve stuff.
    self$bound = TRUE
    self$sources <-self$makeSourceSeq(model)
    self$xValues <-self$getX(self$sources)
    self$yValues <-self$getY(self$sources)

    if(simPeriod){
      dsassert(is.paramrange(model$range),"Model must have a paramRange to use simPeriod=TRUE.")

      args=list(FUN=model$find.period,x=self$testX,y=self$testY, numTries=10, maxPeriod=512) #,the rest of args
      self$aname=model$range$aname
      self$bname=model$range$bname
      args[[self$aname]]=self$xValues
      args[[self$bname]]=self$yValues
      periods=do.call(what=mapply,args=args)

      transitions = rle(periods)
      p = cumsum(transitions$lengths)
      n = length(p)
      starts = c(1,(p+1)[-n])
      ends = p
      self$phaseFrame = data.frame(start  = self$sources[starts],
                                   period = transitions$values,
                                   stop   = self$sources[ends])

      segments = vector("list", length=length(ends))
      for(i in 1:length(ends)) {
        phase = starts[i]:ends[i]
        segments[[i]] = data.frame(x = self$xValues[phase], y = self$yValues[phase], period=periods[phase])
      }
      self$toPlot=segments #with new rendering toplot dosent need to know periods.

      darken <- function(color, factor=1.4){
        col <- col2rgb(color)
        col <- col/factor
        col <- rgb(t(col), maxColorValue=255)
        col
      }
      colMap=sort(unique(append(mapply(function(seg)seg$period[[1]],self$toPlot),c(1,0))))
      numCol=length(colMap)
      #slightly darker version of simmapperiod's colors
      if(is.null(self$col) || length(self$col)<numCol){
        if (numCol <= 6)
          self$col <- darken(c("yellow", "magenta", "orange", "green", "red", "blue"))
        else if (numCol <= 28)
          self$col <- darken(c("#00119c","#cdff50","#8d00a9","#00b054","#ff40dd","#01f9be","#ff1287","#2a73ff","#d99b00","#f5ff84","#3e004a","#91fffa","#ff455a","#00a5f3","#850f00","#9897ff","#0e2100","#e2b5ff","#005238","#ffa287","#12002c","#e2ffe0","#620045","#ffd3e1","#2b0a00","#0068b0","#5f1800","#00376f"))
        else
          self$col <- rainbow(numCol) #warning? More colors needed
      }
      self$model=model

      newCol=vector("character",length(transitions$values))
      for(i in 1:length(transitions$values)){
        newCol[i]=self$col[which(colMap==transitions$values[[i]])]
      }
      self$col=newCol
    }
  },
  render = function(self, model) {
    if(display){
      if(self$discretize){
        for(i in 1:length(self$toPlot))
          points(self$toPlot[[i]]$x, self$toPlot[[i]]$y,
                 col = self$col[[i]], ... = self$...)
      }
      else{
        for(i in 1:length(self$toPlot))
          lines(self$toPlot[[i]]$x, self$toPlot[[i]]$y, lwd = self$lwd,
                col = self$col[[i]], ... = self$...)
      }
    }
  },
  #functions to give data to the user
  phaseDist=function(self, prev, post){
    x1=self$getX(prev$stop)
    x2=self$getX(post$start)
    y1=self$getY(prev$stop)
    y2=self$getY(post$start)
    p1=c(x1,y1)
    p2=c(x2,y2)
    sqdist(p1,p2)
  },
  recurNarrow= function(self, prev,post,tolerance){
    if(self$phaseDist(prev,post) < tolerance){ #xydist
      return(rbind(prev,post))
    }
    midPoint=(prev$stop+post$start)/2
    p1=prev$period
    p2=post$period
    x=self$getX(midPoint)
    y=self$getY(midPoint)
    args=list(x=self$testX,y=self$testY, numTries=10, maxPeriod=512, epsilon=.0000001) #,the rest of args
    args[[self$aname]]=x
    args[[self$bname]]=y
    p=do.call(self$model$find.period,args)
    if(p!=p1){
      if(p!=p2){ #new phase in between
        mid=data.frame(start=midPoint ,period=p,stop=midPoint)
        prev=self$recurNarrow(prev,mid,tolerance)   #compute both sides
        post=self$recurNarrow(mid,post,tolerance)
        lenPrev=nrow(prev)
        midStart=prev[lenPrev,]$start   #merge the result from both sides
        post[1,]$start=midStart
        return(rbind(prev[1:(lenPrev-1),],post))
      }
      else{
        #midpoint goes into post
        post$start=midPoint
      }
    }
    else{
      #midpoint goes into prev
      prev$stop=midPoint
    }
    return(self$recurNarrow(prev,post,tolerance))

  },
  narrow= function(self, tolerance=sqrt(sqrt(.Machine$double.eps))){
    pha=self$recurNarrow(prev = self$phaseFrame[1,],post = self$phaseFrame[nrow(self$phaseFrame),],tolerance=tolerance)
    self$phaseFrame=pha
    pha
  },
  addDistanceToPhase=function(self,inPhase){
    findDist=function(index,phases){
      x1=self$getX(phases[index,]$stop)
      x2=self$getX(phases[index,]$start)
      y1=self$getY(phases[index,]$stop)
      y2=self$getY(phases[index,]$start)
      sqrt((x1-x2)^2 + (y1-y2)^2)
    }
    dist=mapply(findDist,1:nrow(inPhase),MoreArgs=list(inPhase))
    withDist=cbind(inPhase,dist)
    findRatio=function(index,phases){
      (phases[index,]$dist)/(phases[index+1,]$dist)
    }
    ratio=append(NA,mapply(findRatio,1:(nrow(inPhase)-1),MoreArgs=list(withDist)))
    cbind(withDist,ratio)
  },
  phases=function(self, distances=FALSE, sources=TRUE, params=FALSE){  #add or take out columns of phaseFrame according to parameters.
    ret=self$phaseFrame
    if(params){
      startA=paste("start",self$aname)
      stopA=paste("stop",self$aname)
      startB=paste("start",self$bname)
      stopB=paste("stop",self$bname)
      start=ret$start
      stop=ret$stop
      add=data.frame(self$getX(start),self$getY(start),self$getX(stop),self$getY(stop))
      names(add)=c(startA,startB,stopA,stopB)
      ret=cbind(ret,add)
      ret=ret[c("start",startA,startB,"period","stop",stopA,stopB)]
    }
    if(distances){
      ret=self$addDistanceToPhase(ret)
    }

    if(!sources){
      ret[,c("start","stop")]=NULL
    }
    ret
  }

  )
}

#' Reports whether x is a dscurves object.
#' @param x An object to test.
# @rdname dscurve
#' @keywords internal
#' @export
is.curve <- function(x) inherits(x,"curve")


ensureFunction <- function(expr, par){
  if(safe.apply(is.function, eval(expr))){
    eval(expr)
  } else {
    if(safe.apply(is.numeric,expr)){
      if(par)
        function(t) expr
      else
        function(x) expr
    } else {
      if(par)
        make_function(alist(t=), expr, parent.frame())
      else
        make_function(alist(x=), expr, parent.frame())
    }
  }
}
