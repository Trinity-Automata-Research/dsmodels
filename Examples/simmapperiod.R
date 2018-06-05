simmapperiod = function(testX,testY, alim=NULL, blim=NULL, discretize=0,cols=NULL,
                initIters=1000, maxPeriod=128, numTries=1,
                epsilon=sqrt(sqrt(.Machine$double.eps)), rangeMult=0){
  dsproto(
    `_class` = "image", `_inherit` = background,
    requiresRange=FALSE,
    x=testX,
    y=testY,
    alim=alim,
    blim=blim,
    discretize=discretize,
    initIters=initIters, maxPeriod=maxPeriod, numTries=numTries,
    epsilon=epsilon, rangeMult=rangeMult,
    grid=NULL,
    colMatrix=NULL,
    cols=cols,
    bound=FALSE,
    on.bind = function(self, model){
      if(is.null(model$range)){
        #this should be a paramrange
        model+paramrange(a=alim,b=blim,discretize)
      }
      else{
        self$bound=TRUE
        self$calculate.grid(model)
        self$calculate.bifmap(model)
      }
    },
    calculate.grid = function(self, model){
      #check if model has a paramrange. if not make one
      self$grid=model$range$paramcorners(discretize,xlim=alim,ylim=blim)
    },
    calculate.bifmap = function(self,model){
      #has to be mapply because find.period cant take in lists.
      z=mapply(model$find.period,self$x,self$y,self$grid$X0,self$grid$Y0,
               initIters=initIters, maxPeriod=maxPeriod,
               numTries=numTries,epsilon=epsilon, rangeMult=rangeMult)
      numCol=max(z)
      if(is.null(self$cols) || length(self$cols)<numCol){
        self$cols=rainbow(numCol+1)
        #warning? More colors needed
      }
      self$colMatrix=matrix(z,length(self$grid$x))
    },
    render = function(self){
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
m+simmapperiod(1,1,alim=2,blim=2,discretize = .5)
