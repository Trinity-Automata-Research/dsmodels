simmapperiod = function(testX,testY, alim=NULL, blim=NULL, discretize=0, xlim=NULL, ylim=NULL,cols=NULL,
                display= TRUE, initIters=1000, maxPeriod=128, numTries=1,
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
      dsassert(self$bound,"simmapperiod: attempting to render bifmap before bound", critical = TRUE)
      if(display){
        image(self$grid$x,self$grid$y, self$colMatrix, zlim = c(0, length(self$cols)-1), col=self$cols)
      }
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

#test cases

#add range first, no defaults in sim (should give no warnigs about assuming divergence, red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(a=2,b=2,x=2,y=2,paramDiscretize = 1)
m+simmapperiod(1,1,rangeMult=3)

#FIXME------- if model has a range, x/ylims dont get overidden. not that important.
#add range first, defaults in sim (should give warnigs about assuming divergence, only show blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(a=2,b=2,x=2,y=2,paramdiscretize = 1)
m+simmapperiod(1,1,alim=1,blim=1,xlim=Inf,ylim=Inf,discretize = .5)

#dont add range, no defaults in sim (should crash)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+simmapperiod(1,1)

#dont add range, param defaults in sim, but no x,y defaults (red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+simmapperiod(1,1,alim=2,blim=2,discretize = 1)

#dont add range, param defaults and x,y defaults in sim (should give no warnigs about assuming divergence, red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+simmapperiod(1,1,alim=2,blim=2, xlim=2,ylim=2,discretize = 1, rangeMult = 3)
