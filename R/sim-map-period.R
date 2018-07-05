#' Creates an image describing periodicity over a range of parameters
#'
#' Attempts to determine the periodicity of the model's function accross a range
#' of parameters. Discretizes the parameterspace into squares and repeated iteration
#' of the function at \code{testX,testY} with the parameters of that square is used to
#' determine the periodicity of that square. It is then colored accordingly.
#'
#' @include dsproto.R
#' @param testX the x value of the point at which periodicity is tested.
#' @param testY the y value of the point at which periodicity is tested.
#' @param alim The range of the first parameter to calculate periods over. Defaults to the limits of the range.
#' @param blim The range of the second parameter to calculate periods over. Defaults to the limits of the range.
#' @param xlim The range of x values to calculate periods over. Defaults to the limits of the range.
#' @param ylim The range of y values to calculate periods over. Defaults to the limits of the range.
#' @param paramNames Specifies the names of parameters to be varied. Defaults to the paramNames of the range.
#' @param discretize The discretization for the parameters. Defaults to the discretization of the range.
#' @param cols The colors of the periods. If insufficient not provided, reasonable defaults are used.
#' @param key it \code{TRUE}, displays a key showing what period each color signifies. Defaults to \code{TRUE}
#' @param iters The number of iterations of the function applied before looking for a period. Defaults to 1000.
#' @param maxPeriod The largest period looked for. Any periods larger are considered divergent. defaults to 128.
#' @param initIters The number of iterations of the function to go before looking for periods. Defaulsts to \code{0}
#' @param numTries The number of times a period is looked for. Defaults to 1.
#' @param epsilon The distance at which two points are considered to be the same attractor. Defaults to \code{sqrt(sqrt(.Machine$double.eps))}
#' @param crop Logical. If \code{TRUE}, points that go past xlim or ylim are considered divergent. If \code{FALSE},
#'  a point must reach Inf to be considered divergent. Defaults to \code{FALSE}.
#' @import graphics
#' @import grDevices
#' @seealso \code{\link{paramrange}}
#' @seealso \code{\link{simbasins}}
#' @export
#' @examples
#' f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
#'   list(x*exp(r-x-a*y),
#'        y*exp(s-b*x-y))
#' }
#' #create a model with the function
#' model = dsmodel(f)
#' #add a range of parameters, set discretize, specify that I want to vary s and r
#' model + paramrange(3,3,discretize = .02, paramNames = c(s,r))
#' #generate an image based on periodicity tested at the point (.5,.5). Takes a bit of time.
#' #maxperiodicity=8 makes every periodicity above 8 count as divergent or 0.
#' model + sim.map.period(.5,.5,maxPeriod = 8, epsilon=.0001, iters = 1000, numTries = 1)
#' #varying only one variable can be done by using a dummy variable.
#' #create a model with the function
#' model = dsmodel(f)
#' #add a range and image. set blim very small because it dosent matter. we want to vary s and dummy
#' model + sim.map.period(.5,.5,alim=3,blim=.05, discretize=.05, paramNames=c(s,dummy), maxPeriod = 8)
#'









sim.map.period = function(testX=NULL, testY=NULL, alim=NULL, blim=NULL, xlim=NULL, ylim=NULL, paramNames=NULL, discretize=0, cols=NULL,
                key=TRUE, iters=500, maxPeriod=128, initIters=0, numTries=2, powerOf2=TRUE,
                epsilon=sqrt(sqrt(.Machine$double.eps)), crop=FALSE){
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
  xlim=make.lims(xlim)
  ylim=make.lims(ylim)
  dsproto(
    `_class` = "image", `_inherit` = background, #what should this inherit?
    requiresRange=FALSE,
    x=testX,
    y=testY,
    alim=alim,
    blim=blim,
    xlim=xlim,
    ylim=ylim,
    discretize=discretize,
    aname=aname,
    bname=bname,
    key=key,
    iters=iters, maxPeriod=maxPeriod,
    initIters=initIters, numTries=numTries,
    powerOf2=powerOf2, epsilon=epsilon, crop=crop,
    grid=NULL,
    colMatrix=NULL,
    cols=cols,
    bound=FALSE,
    on.bind = function(self, model){
      if(is.null(model$range)){
        model+paramrange(alim=self$alim,blim=self$blim,discretize=self$discretize,xlim=self$xlim,ylim=self$ylim)
      }
      else{
        if(is.null(self$aname)) {
          dsassert(is.null(self$bname), "bname set in sim.map.period, but not aname.", critical=TRUE)
          dsassert(!is.null(model$range$aname) && !is.null(model$range$bname), "Parameter names not provided, and could not be inferred from function definition.")
          self$aname <- model$range$aname   #this is now done in find.perid but we need them elsewhere.
          self$bname <- model$range$bname
        } else {
          dsassert(!is.null(self$bname), "aname set in sim.map.period, but not bname.", critical=TRUE)
        }
        self$grid=model$range$paramcenters(self$discretize,alim=self$alim,blim=self$blim)
        self$bound=TRUE
        self$calculate.bifmap(model)
        if(all(!is.xlabel(model$facade))){
          model+xlabel(label=self$aname)+ylabel(label=self$bname)
        }
      }
    },
    calculate.bifmap = function(self,model){
      z=model$find.period(a=self$grid$X0, b=self$grid$Y0, x=self$x, y=self$y,
                          iters=self$iters, maxPeriod=self$maxPeriod,
                          initIters=self$initIters, numTries=self$numTries,
                          powerOf2=self$powerOf2, epsilon=self$epsilon,
                          crop=self$crop, xlim=self$xlim, ylim=self$ylim, aname=self$aname, bname=self$bname)

      map=sort(unique(append(z,c(1,0,Inf)))) #should be repaced with an enviornment like dscurve
      normalize=function(x){
        which(map==x)
      }
      z=mapply(normalize,z)
      numCol=length(map)
      if(is.null(self$cols) || length(self$cols)<numCol){
        if (numCol <= 6)
          self$cols <- c("yellow", "magenta", "orange", "green", "red", "blue")
        else if (numCol <= 28)
          self$cols <- c("#00119c","#cdff50","#8d00a9","#00b054","#ff40dd","#01f9be","#ff1287","#2a73ff","#d99b00","#f5ff84","#3e004a","#91fffa","#ff455a","#00a5f3","#850f00","#9897ff","#0e2100","#e2b5ff","#005238","#ffa287","#12002c","#e2ffe0","#620045","#ffd3e1","#2b0a00","#0068b0","#5f1800","#00376f")
        else
          self$cols <- rainbow(numCol) #warning? More colors needed
      }
      self$map=map
      self$numCol=numCol
      self$colMatrix=matrix(z,length(self$grid$x))
    },
    render = function(self, model){
      if((is.null(self$firstRender) || self$firstRender==TRUE) && self$key){
        self$firstRender=FALSE
        par(mar=c(5, 4, 4, 6) + 0.1)
        model$redisplay()
      }
      else{
        dsassert(self$bound,"sim.map.period: attempting to render bifmap before bound.", critical = TRUE)
        range=1:self$numCol
        image(self$grid$x,self$grid$y, self$colMatrix, zlim = c(1, self$numCol), col=self$cols[range], add=TRUE)
        if(self$key){
          names=self$map
          names[1]="Divergent"
          names[2]="Fixed"
          names[length(names)]="Chaotic"
          legend("topright", inset=c(-0.25,0), legend=names,
                 fill=self$cols, title="Periods", xpd=TRUE)
        }
      }
    }
  )
}

