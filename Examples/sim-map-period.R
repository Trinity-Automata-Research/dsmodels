sim.map.period = function(testX,testY, alim=NULL, blim=NULL, discretize=0, xlim=NULL, ylim=NULL,cols=NULL,
                paramNames=NULL, key=TRUE, initIters=1000, maxPeriod=128, numTries=1,
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
    key=key,
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
        self$grid=model$range$paramcenters(discretize,alim=self$alim,blim=self$blim)
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

      map=sort(unique(c(z,1,0)))
      normalize=function(x){
        spot=which(map==x)
        if(length(spot)!=1)
          stop("aaa again")
        spot
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
      dsassert(self$bound,"sim.map.period: attempting to render bifmap before bound", critical = TRUE)
      range=1:self$numCol
      image(self$grid$x,self$grid$y, self$colMatrix, zlim = c(1, self$numCol), col=self$cols[range], add=TRUE)
      model+xlabel(label=self$aname)+ylabel(label=self$bname)
      if(key){
        names=self$map
        names[1]="Divergent"
        names[2]="Fixed"
        legend("topright", inset=c(-0.2,0), legend=names,
               fill=self$cols, title="Periods", xpd=TRUE)
      }
    }
  )
}

