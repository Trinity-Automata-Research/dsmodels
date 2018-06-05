simmapperiod = function(testX,testY, alim=NULL, blim=NULL, discretize=0, xlim=NULL, ylim=NULL,cols=NULL,
                display= NULL, initIters=1000, maxPeriod=128, numTries=1,
                epsilon=sqrt(sqrt(.Machine$double.eps)), rangeMult=0){
  dsproto(
    `_class` = "image", `_inherit` = background, #what should this inherit?
    requiresRange=FALSE,
    x=testX,
    y=testY,
    alim=alim,
    blim=blim,
    discretize=discretize,
    display=display,
    initIters=initIters, maxPeriod=maxPeriod, numTries=numTries,
    epsilon=epsilon, rangeMult=rangeMult,
    grid=NULL,
    colMatrix=NULL,
    cols=cols,
    bound=FALSE,
    on.bind = function(self, model){
      if(is.null(model$range)){
        model+paramrange(a=alim,b=blim,paramdiscretize=discretize,x=xlim,y=ylim)
      }
      else{
        self$bound=TRUE
        self$calculate.grid(model)
        self$calculate.bifmap(model)
      }
    },
    calculate.grid = function(self, model){
      #check if model has a paramrange. if not make one
      self$grid=model$range$paramcorners(discretize,alim=alim,blim=blim)
    },
    calculate.bifmap = function(self,model){
      #has to be mapply because find.period cant take in lists.
      ##z=mapply(model$find.period,self$x,self$y,self$grid$X0,self$grid$Y0,
      #        initIters=initIters, maxPeriod=maxPeriod,
      #         numTries=numTries,epsilon=epsilon, rangeMult=rangeMult)

      args=list(FUN=model$find.period, x=self$x, y=self$y, initIters=initIters, maxPeriod=maxPeriod,
           numTries=numTries,epsilon=epsilon, rangeMult=rangeMult)
      args[[model$range$aname]]=self$grid$X0
      args[[model$range$bname]]=self$grid$Y0
      z=do.call(mapply,args)

      numCol=max(z)
      if(is.null(self$cols) || length(self$cols)<numCol){
        self$cols=rainbow(numCol+1)
        #warning? More colors needed
      }
      self$colMatrix=matrix(z,length(self$grid$x))
    },
    render = function(self, model){
      dsassert(self$bound,"attempting to render bifmap before bound", critical = TRUE)
      image(self$grid$x,self$grid$y, self$colMatrix, zlim = c(0, length(self$cols)-1), col=self$cols)
    }
  )
}

dsassert = function(t,str,critical=FALSE) {
  if(!t) {
    if(critical)
      stop(paste("Critical error:",str,". Please notify developers."))
    else
      stop(str)
  }
}


m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(a=2,b=2,paramdiscretize = .5)
m+simmapperiod(1,1,discretize = .5)
