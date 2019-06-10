#' Parametric curves or a graph of functions
#'
#' This function takes a description of a curve and creates an object displaying the curve, and optionally
#' it's behavior throughout iterations of the system. Functions can be provided as expressions of \code{x},
#' for graphing curves, or \code{t}, for parametric curves.
#' The curve is defined either by the graph of a single function or a pair of parametric
#' equations. By default, rendered with the \code{lines} function.
#'
#' @section The graph of a function:
#'
#' If the parameter \code{yfun} is not provided, then \code{dscurve} contains
#' the curve of points (x,fun(x)). The inputs to \code{fun} are \code{n} points between the maximum
#' and minimum. The maximum and minimum are taken from  the
#' \code{\link{dsrange}}'s x limits, but can be overwritten with the \code{xlim} parameter.
#' \code{fun} can either be any function of a single parameter, or an expression with
#' exactly \code{x} as the free variable.
#'
#' @section  Parametric equations:
#'
#' If the parameter \code{fun} and \code{yfun} are both provided,
#' \code{dscurve} contains the parametric curve described by the functions. The function is
#' calculated at \code{n}
#' points ranging from \code{tmin} to \code{tmax}.
#' \code{fun} and \code{yfun} can either be any function of a single parameter,
#' or an expression with exactly \code{t} as the free variable.
#'
#' @section SimPeriod:
#'
#' If the parameter \code{simPeriod} is set to \code{TRUE}, \code{dscurve} will color its curve
#' according to the periodicity. This requires the model's range to be a paramRange. Iters will be
#' ignored.
#'

#'
#' @section Images of curves:
#'
#' The \code{dscurve} object begins with an initial curve. Images of the curve may be displayed in three ways.
#' If the \code{image} parameter is a single color and \code{iters} is not set, then \code{dscurve}
#' will calculate and display the image of the curve under the model's function in that color.
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
#' @param simPeriod Logical, determines if the curve will be colored according to its periodicity.
#'  Requires model's range to be a paramRange if \code{TRUE}. Defaults to \code{FALSE}. See also \code{\link{sim.map.period}}
#' @param find.period.args Additional arguments to find.period. Only used if simPeriod is set to \code{TRUE}.
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
#' @param xlim Only used for the graph of a function. Determines the range of x values (or a values, if parameterized) for which the function
#' is plotted. Defaults to the x limits of the model's dsrange.
#' @param crop If \code{crop==TRUE}, the original curve and all iterations are cropped to the range.
#' @param discretize Set \code{discretize=TRUE} to display the calculated points, instead of
#' connecting them as a curve: the curve is displayed with \code{points}
#' instead of \code{lines}.
#' @param stretch The stretch parameter passed to breakDisconts when the curve is bound to a model.
#' @param label A string representing the label to be displayed when the curve is rendered.
#' @param labelLoc A real number between 0 and 1 denoting at what fraction of the way through the line the label should be displayed. Defaults to 0.5.
#' @param labelOffset This will offset the label. Enter as c(x, y). Defaults to an automatic scale dependent on the dsrange's y axis size.
#' @param labelCol A string color denoting what color the label's text will be. Defaults to black.
#' @param labelBg A string color denoting what color the label's background will be. Defaults to white.
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
#' #using simPeriod
#' f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
#' list(x*exp(r-x-a*y),
#'      y*exp(s-b*x-y))
#' }
#'
#' mod=dsmodel(f)
#'
#' mod + paramrange(3,3,discretize = .1, paramNames = c(s,r),renderCount = 20)
#' #period map to compare curves to
#' mod + sim.map.period(.5,.5,maxPeriod = 8, epsilon=.0001, iters = 100,
#'                      numTries = 1, powerOf2=TRUE)
#'
#' #not parametric
#' c=dscurve(x/2,simPeriod = TRUE)
#' mod+c
#' #get the ranges of periodicity
#' print(c$narrow(tolerance=.001)) #refine the ranges
#' print(c$phases(distances=TRUE)) #add Distances to the ranges
#'
#' #parametric
#' c=dscurve(1*t,3*t,simPeriod = TRUE)
#' mod+c
#' #get the ranges of periodicity
#' print(c$narrow())
#' print(c$phases(params=TRUE,source=FALSE)) #replace the tValues(sources) with the parameter values
#'
#' @export
dscurve <- function(fun, yfun = NULL,
                    col = NULL, image = NULL,
                    lwd = 3, n=NULL, iters = 0, simPeriod=FALSE, find.period.args=list(),
                    testX=.1, testY=.1, #better names? simX, simY?
                    crop = FALSE,  tstart=0, tend=1,
                    discretize=FALSE, xlim = NULL, display=TRUE, stretch = 0, label = "", labelLoc = 0.5, labelOffset = NULL,
                    labelCol = "Black", labelBg = "white",
                    ...) {
  if(!simPeriod) {
    if(is.null(col))
      col="black"
    colors <- colorVector(col, image, iters)
    iters <- length(colors)-1
  } else {
    colors=NULL
  }

  fun = substitute(fun)
  yfun = substitute(yfun)
  if(safe.apply(is.null,yfun)) { #curve is not parametric
    isParametric=FALSE
    if(is.null(xlim)){
      lims=NULL
    }
    else{
      lims=make.lims(xlim)
    }
  } else { #curve is parametric
    isParametric=TRUE
    lims=c(tstart,tend)
  }

  dsproto(
    `_class` = "curve", `_inherit` = feature,
    fun = fun,
    yfun = yfun,
    getX=NULL,
    getY=NULL,
    isParametric=isParametric,
    hasLabel = label != "",
    label = label,
    col = colors,
    labelBg = labelBg,
    labelCol = labelCol,
    colMap = NULL,
    givenColors = col,
    testX=testX,
    testY=testY,
    lwd = lwd,
    iters = iters,
    simPeriod=simPeriod,
    find.period.args=find.period.args,
    n = n,
    narrowed = FALSE,
    sources = NULL,
    xValues = NULL,
    yValues = NULL,
    toPlot = NULL,
    discretize = discretize,
    crop=crop,
    lims=lims,
    display = display,
    ... = ...,
    #functions to interact with the model
    on.bind = function(self, model) {
      #common between all curves
      self$bound = TRUE
      self$model=model
      self$makeSources(model)
      self$buildFunctions(model)
      self$xValues <-mapply(self$getX,self$sources)
      self$yValues <-mapply(self$getY,self$sources)
      if(simPeriod) {# only simPeriod curves
        self$buildSimPlots()
      } else { #only not sim Period curves
        self$toPlot <- mapply(breakDisconts, model$apply(self$xValues, self$yValues, iters=self$iters, crop = self$crop), MoreArgs = list(xlim = model$range$xlim, ylim = model$range$ylim, stretch = stretch), SIMPLIFY = FALSE)
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
        if(self$hasLabel) self$displayLabel(model$range)
      }
    },
    displayLabel = function(self, range) {
      if(self$hasLabel) {
        if(is.null(labelOffset)) {
          scale <- 0.08*(abs(max(range$ylim) - min(range$ylim)))
          self$labelOffset=c(0,scale)
        }
        xloc <- self$xValues[labelLoc * length(self$xValues)] + self$labelOffset[1]
        yloc <- self$yValues[labelLoc * length(self$yValues)] + self$labelOffset[2]
        shadowtext(xloc, yloc, labels = self$label, col = self$labelCol, bg = self$labelBg)
      }
    },
    recalculate = function(self, model) {
      if(self$simPeriod && self$narrowed)
      { #recalculate toPlot and col from narrowed phases
        self$makeColMap()
        self$toPlot = vector("list", length=nrow(self$phaseFrame))
        for(i in 1:nrow(self$phaseFrame)){
          row=self$phaseFrame[i,]
          start=row$start
          stop=row$stop
          mid=self$sources[self$sources >= start & self$sources <= stop]
          sourceSeg=c(start,mid,stop)
          xs=mapply(self$getX,sourceSeg)
          ys=mapply(self$getY,sourceSeg)
          self$toPlot[[i]]= breakDisconts(data.frame(x=xs,y=ys), model$range$xlim, model$range$ylim, stretch = stretch)
          self$col[[i]]=self$colMap[[as.character(row$period)]]
        }
      } else {
        self$on.bind(model)
      }
    },
    makeSources = function(self, model){
      if(is.null(self$n)) #get numPoints
        numPoints <- model$range$renderCount
      else
        numPoints <- self$n
      if(is.null(self$lims)){ #get limits if we dont have them
        if(is.paramrange(model$range)) #if we have a paramRange, we want the alim.
          self$lims=model$range$alim
        else                           #if not, we want the xlim
          self$lims=model$range$xlim
      }
      from=min(self$lims)  #make a sequence
      to=max(self$lims)
      self$sources <- seq(from,to, length.out = numPoints)
    },
    buildFunctions = function(self,model){ #determining how to pick x and y values
      if(self$isParametric){
        getX <- ensureFunction(self$fun, TRUE)
        getY <- ensureFunction(self$yfun, TRUE)
      }
      else{ #not parametric curve
        getX <- identity
        getY <- ensureFunction(self$fun, FALSE)
        if(is.paramrange(model$range) && !safe.apply(is.function, eval(self$fun))) {
          #param model and getX is an expression, figure out parameter name.
          subNames=all.names(self$fun)
          ain=model$range$aname %in% subNames
          xin="x" %in% subNames
          if(!xin || ain){
            names(formals(getY))=model$range$aname
            if(xin && ain) {
              warning(paste("curve function contains both 'x' and'", self$sourceName, "'. Assuming you want to vary on ",self$sourceName,"."))
            }
          }
        }
      }
      self$sourceName=names(formals(getY))[[1]]
      self$getX=getX
      self$getY=getY
    },
    buildSimPlots = function(self) {
      dsassert(self$simPeriod, "Simulation can only be used on dscurves constructed with simPeriod=TRUE")
      dsassert(is.paramrange(self$model$range),"Model must have a paramRange to use simPeriod=TRUE")
      #find the periods
      args=append(self$find.period.args,list(FUN=self$model$find.period,x=self$testX,y=self$testY))
      self$aname=self$model$range$aname
      self$bname=self$model$range$bname
      args[[self$aname]]=self$xValues
      args[[self$bname]]=self$yValues
      periods=do.call(what=mapply,args=args)
      #break into phases (transitions)
      transitions = rle(periods)
      p = cumsum(transitions$lengths)
      n = length(p)
      starts = c(1,(p+1)[-n])
      ends = p
      self$phaseFrame = data.frame(start  = self$sources[starts],
                                   period = transitions$values,
                                   stop   = self$sources[ends])
      #if(self$narrowFlag){       # if we want to have the option to autimatically narrow
      # do the stuff that is done in recalculate:
      #call narrow
      #update segments with new phaseFrame
      #}

      #chose colors and line segments to plot
      self$makeColMap()
      self$toPlot = vector("list", length=length(ends))
      self$col = vector(length=length(ends))
      for(i in 1:length(ends)) {
        phase = starts[i]:ends[i]
        self$toPlot[[i]] = data.frame(x = self$xValues[phase], y = self$yValues[phase])
        p = transitions$values[[i]]
        self$col[[i]] = self$colMap[[as.character(p)]]
      }
    },
    makeColMap = function(self) {
      periods=self$phaseFrame[,"period"]
      maxPeriod=max(periods[is.finite(periods)])
      #only runs if current map is to small/ missing maxPeriod
      if(maxPeriod+2>length(self$colMap)){ #or if(is.null(self$colMap[[as.character(maxPeriod)]])){
        powersOf2=self$find.period.args$powersOf2
        if(is.null(powersOf2))
          powersOf2=TRUE
        if(powersOf2)
          numCol=log(maxPeriod,2)+2
        else
          numCol=maxPeriod+2
        #slightly darker version of simmapperiod's colors
        if(is.null(self$givenColors) || length(self$givenColors)<numCol){
          if(!is.null(self$givenColors))
            warning("not enough colors given, using a preset") #warning? More colors needed
          if (numCol <= 6) {
            self$col <- darken(c("yellow", "magenta", "orange", "green", "red", "blue"))
          } else if (numCol <= 28) {
            self$col <- darken(c("#00119c","#cdff50","#8d00a9","#00b054","#ff40dd","#01f9be","#ff1287",
                                 "#2a73ff","#d99b00","#f5ff84","#3e004a","#91fffa","#ff455a","#00a5f3",
                                 "#850f00","#9897ff","#0e2100","#e2b5ff","#005238","#ffa287","#12002c",
                                 "#e2ffe0","#620045","#ffd3e1","#2b0a00","#0068b0","#5f1800","#00376f"))
          } else {
            self$col <- rainbow(numCol)
          }
        }
        #for each period, assign a color
        self$colMap=new.env()
        self$colMap[[as.character(0)]]=self$col[1]
        self$colMap[[as.character(Inf)]]=self$col[numCol]
        if(powersOf2) {
          i=1
          colIndex=2
          while(i<=maxPeriod){
            self$colMap[[as.character(i)]]=self$col[colIndex]
            colIndex=colIndex+1
            i=i*2
          }
        } else {
          for(i in 1:maxPeriod) {
            self$colMap[[as.character(i)]]=self$col[i+1]
          }
        }
      }
    },
    #functions to give data to the user
    phases=function(self, distances=FALSE, sources=TRUE, params=FALSE){  #add or take out columns of phaseFrame according to parameters.
      dsassert(self$bound, "To use this method the curve must be bound to a model")
      dsassert(self$simPeriod, "To use this method the curve be constructed with simPeriod=TRUE")
      ret=self$phaseFrame
      if(params){ #add the value of the parameters to the data frame
        startA=paste("start",self$aname)
        stopA=paste("stop",self$aname)
        startB=paste("start",self$bname)
        stopB=paste("stop",self$bname)
        start=ret$start
        stop=ret$stop
        add=data.frame(self$getX(start),self$getY(start),self$getX(stop),self$getY(stop))
        names(add)=c(startA,startB,stopA,stopB)
        ret=cbind(ret,add)[c("period","start",startA,startB,"stop",stopA,stopB)]
      }
      if(distances){ #add the distances of each phase to thte data frame
        ret=self$addDistanceToPhase(ret)
      }
      if(!sources){ #remove the source values from the dataFrame
        ret[,c("start","stop")]=NULL
      }
      ret
    },
    addDistanceToPhase=function(self,inPhase){
      findDist=function(index,phases){
        sqrt(self$distOfSources(phases[index,]$start, phases[index,]$stop))
      }
      dist=mapply(findDist, 1:nrow(inPhase), MoreArgs=list(inPhase))
      withDist=cbind(inPhase, dist)
      findRatio=function(index,phases){
        (phases[index,]$dist)/(phases[index+1,]$dist)
      }
      ratio=append(NA, mapply(findRatio, 1:(nrow(inPhase)-1), MoreArgs=list(withDist)))
      cbind(withDist, ratio)
    },
    distOfSources=function(self, pointA, pointB){
      x1=self$getX(pointA)
      y1=self$getY(pointA)
      x2=self$getX(pointB)
      y2=self$getY(pointB)
      sqdist(c(x1,y1),c(x2,y2))
    },
    narrow= function(self, tolerance=sqrt(sqrt(.Machine$double.eps)), redisplay=TRUE){
      dsassert(self$bound, "To use this function the curve must be bound to a model")
      dsassert(self$simPeriod, "To use this function the curve must have simPeriod set to true")
      self$narrowed = TRUE
      pf=self$phaseFrame
      end = nrow(pf)
      firstStart=pf[1,]$start
      lastStop=pf[end,]$stop
      #convert phases into gaps and recursively narrow
      gaps=Reduce(rbind,
              mapply(self$recurNarrow,
                        pf$stop[-end], pf$period[-end],
                        pf$start[-1], pf$period[-1],
                        MoreArgs = list(tolerance=tolerance), SIMPLIFY = FALSE))
      #convert back into phases
      self$phaseFrame = data.frame(
          start=c(firstStart,gaps$start),
          stop=c(gaps$stop, lastStop),
          period = c(gaps$startP[1], gaps$stopP))
      if(redisplay){ #update the l=plot
        self$recalculate(self$model)
        #self$model$redisplay() #if something should be on top of this, redisplay will keep it that way
        self$render(self$model) #I think the curve should alway be on top anyways though
      }
      self$phaseFrame
    },
    recurNarrow=function(self, start, startP, stop, stopP, tolerance){
      #narrows the gaps in between periods to within tolerance, returns list of vectors (in case new periods found)
      if(self$distOfSources(start,stop) < tolerance) #calculate xydist. if gap is small enough, we are done.
        return(data.frame(start=start, startP=startP, stop=stop, stopP=stopP))
      #calculate the periodicity of the midpoint
      midPoint=(start+stop)/2
      a=self$getX(midPoint)
      b=self$getY(midPoint)
      args=append(self$find.period.args,list(x=self$testX,y=self$testY))
      args[[self$aname]]=a
      args[[self$bname]]=b
      p=do.call(self$model$find.period,args)
      if(p==startP)   #gap gets smaller
        return(self$recurNarrow(midPoint,startP,stop,stopP,tolerance))
      else if(p==stopP)
        return(self$recurNarrow(start,startP,midPoint,stopP,tolerance))
      else{           #gap splits into two gaps
        g1=self$recurNarrow(start,startP,midPoint,p,tolerance)
        g2=self$recurNarrow(midPoint,p,stop,stopP,tolerance)
        return(rbind(g1,g2))
      }
    }
  )
}

#' Reports whether x is a dscurves object.
#' @param x An object to test.
# @rdname dscurve
#' @keywords internal
#' @export
is.curve <- function(x) inherits(x,"curve")

#takes an expression or function and a boolean for if the expression is parametric
#returns a function in the format that dscurve expects
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

#' Darkens a color by a factor.
#' @param color A color to darken.
#' @param factor The factor to darken color by. defaults to 1.4
# @rdname dscurve
#' @keywords internal
#' @export
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}
  
#' Takes limits for the x and y values of a line, returns a version of the line with any values beyond those limits replaced by NaN.
#' @param xlim A vector of length 2 where the first element is the minimum x value, and the second element is the maximum x value.
#' @param ylim A vector of length 2 where the first element is the minimum y value, and the second element is the maximum y value.
#' @param line A list of length 2 where the first element is a vector containing the x values of the line, and the second element is a vector containing the y values of the line.
#' @param stretch A real number. Entering -1 will cause the function to replace any points beyond the limits provided with NaN. Entering -2 will cause the function to search for consecutive points beyond the limits provided, and replace one of them with NaN. Entering a positive real number will cause the function to insert NaN between any two consecutive points with a euclidean distance between them that is greater than the entered value. Entering 0 will cause the function to simply return an unaltered line. Defaults to 0.
# @rdname dscurve
#' @keywords internal
#' @export
breakDisconts <- function(line, xlim, ylim, stretch = 0) {
  if(stretch == -1) {
    line$x[line$x < xlim[1] | line$x > xlim[2]] <- NaN
    line$y[line$y < ylim[1] | line$y > ylim[2]] <- NaN
  }
  else if(stretch == -2) {
    oobx <- line$x < xlim[1] | line$x > xlim[2]
    ooby <- line$y < ylim[1] | line$y > ylim[2]
    line$x[oobx & c(FALSE, oobx[-length(oobx)]) & c(oobx[-1], FALSE)] <- NaN
    line$y[ooby & c(FALSE, ooby[-length(ooby)]) & c(ooby[-1], FALSE)] <- NaN
  }
  else if(stretch > 0) {
    rep <- which(c(FALSE, diff(line$x)^2 + diff(line$y)^2) > stretch^2)
    if(length(rep) > 1) {
      line$y <- insert(line$y, rep, NaN)
      line$x <- insert(line$x, rep, NaN)
    }
  }
  line
}

