sim.map.period = function(testX,testY, alim=NULL, blim=NULL, discretize=0, xlim=NULL, ylim=NULL,cols=NULL,
                paramNames=NULL, initIters=1000, maxPeriod=128, numTries=1,
                epsilon=sqrt(sqrt(.Machine$double.eps)), rangeMult=0){
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
    `_class` = "image", `_inherit` = background, #what should this inherit?
    requiresRange=FALSE,
    x=testX,
    y=testY,
    alim=alim,
    blim=blim,
    discretize=discretize,
    aname=aname,
    bname=bname,
    initIters=initIters, maxPeriod=maxPeriod, numTries=numTries,
    epsilon=epsilon, rangeMult=rangeMult,
    grid=NULL,
    colMatrix=NULL,
    cols=cols,
    bound=FALSE,
    on.bind = function(self, model){
      if(is.null(model$range)){
        model+paramrange(a=alim,b=blim,discretize=discretize,x=xlim,y=ylim)
      }
      else{
        if(is.null(self$aname)) {
          dsassert(is.null(self$bname), "bname set in sim.map.period, but not aname.", critical=TRUE)
          dsassert(!is.null(model$range$aname) && !is.null(model$range$bname), "Parameter names not provided, and could not be inferred from function definition.")
          self$aname <- model$range$aname
          self$bname <- model$range$bname
        } else {
          dsassert(!is.null(self$bname), "aname set in sim.map.period, but not bname.", critical=TRUE)
        }
        self$grid=model$range$paramcorners(discretize,alim=self$alim,blim=self$blim)
        self$bound=TRUE
        self$calculate.bifmap(model)
      }
    },
    calculate.bifmap = function(self,model){
      #has to be mapply because find.period cant take in lists.
      ##z=mapply(model$find.period,self$x,self$y,self$grid$X0,self$grid$Y0,
      #        initIters=initIters, maxPeriod=maxPeriod,
      #         numTries=numTries,epsilon=epsilon, rangeMult=rangeMult)

      args=list(FUN=model$find.period, x=self$x, y=self$y, initIters=initIters, maxPeriod=maxPeriod,
           numTries=numTries,epsilon=epsilon, rangeMult=rangeMult)
      args[[self$aname]]=self$grid$X0
      args[[self$bname]]=self$grid$Y0
      z=do.call(mapply,args)
      numCol=max(z)
      if(is.null(self$cols) || length(self$cols)<numCol){
        self$cols=rainbow(numCol+1)
        #warning? More colors needed
      }
      self$colMatrix=matrix(z,length(self$grid$x))
    },
    render = function(self, model){
      dsassert(self$bound,"sim.map.period: attempting to render bifmap before bound", critical = TRUE)
      image(self$grid$x,self$grid$y, self$colMatrix, zlim = c(0, length(self$cols)-1), col=self$cols)
    }
  )
}

