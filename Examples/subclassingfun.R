#for now, force the order of variales to be x,y,param1,param2

absmodel = dsproto(
  `_class` = "model",
  `_inherit` = NULL,

  #temporary value for testing
  range=dsrange(4,4),
  paramrange=dsrange(2,2,discretize = .2),

  #probably dont need default values but then all thoes would need to come before ... i think
  apply=function(self, x, y, ..., iters=1, accumulate=TRUE,crop=TRUE, trace=FALSE){
    if(trace){
      print("absmodel apply")
      print(as.list(environment()))
    }
    if(is.null(x) || is.null(y))
      stop("dsmodel: Please make sure your x and y values are defined in latest object created.")
    if(accumulate) {
      iterAccum = vector("list",iters+1)
      startvals = list(x=x, y=y)
      iterAccum[[1]] <- startvals
      iterSeq = 1:iters
      if(iters==0)
        iterSeq <- NULL
      for(i in iterSeq){
        tmp=self$fun(x,y,...)
        if(any(is.nan(tmp[[1]])) || any(is.nan(tmp[[2]])))
        {
          warning("dsmodel: model undefined, NaN computed. (Division by zero). Removing points with NaN value and continuing procedure..")
          tmp = NaNRemove(tmp)
        }
        x = tmp[[1]]
        y = tmp[[2]]
        iterAccum[[i+1]] <- tmp
      }
      iterAccum
    } else {
      for(i in 1:iters){
        tmp=self$fun(x,y,...)
        x = tmp[[1]]
        y = tmp[[2]]
      }
      tmp
    }
  },
  has.diverged = function(self, x, y, rangeMult=0){
    if(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult))
      !finite.points(c(x,y))
    else
      !all(x < rangeMult*self$range$xlim[[2]] & y < rangeMult*self$range$ylim[[2]])
  },
  #having parameters breaks being able to pass in a list for x
  find.period= function(self, x, y, ..., initIters=1000, maxPeriod=128, numTries=1,
                        epsilon=sqrt(sqrt(.Machine$double.eps)), rangeMult=0){
    if(!(!is.null(y) && length(x)==1 && length(y)==1)){
      if(is.dspoint(x)){
        y=x$y
        x=x$x
      }
      else if(is.vector(x) && length(x)==2){
        y=x[[2]]
        x=x[[1]]
      }
      else {
        stop("dsmodel: expected input formats for find.period's starting point are two scalars, a vector of length two or a dspoint")
      }
    }
    if(!(rangeMult==0 || rangeMult==Inf ||is.null(rangeMult)) && is.null(self$range)){
      stop("is.stable with rangeMult!=0 requires range() to have been composed with the model.")
    }
    #moves all the points untill they are either all infinite, fixed, or outside of range*rangeMult
    for(i in 1:numTries) {
      startPoint <- self$apply(x,y,...,iters=initIters,accumulate=FALSE,crop=FALSE)
      if(self$has.diverged(startPoint[[1]],startPoint[[2]],rangeMult)){
        #print("no period found, diverged")
        return(FALSE)
      }
      candidates=self$apply(startPoint[[1]], startPoint[[2]], ..., iters=maxPeriod,accumulate=TRUE,crop=FALSE)
      period=FALSE
      i=1
      while(i<maxPeriod && !period){
        ithPoint=candidates[[i+1]]
        if(sqdist(startPoint, ithPoint) < epsilon)
          period=TRUE
        else
          i=i+1
      }
      if(period){
        return(i)
      }
      x=ithPoint[[1]]
      y=ithPoint[[2]]
    }
    warning(paste("Assuming divergance: no period found after",(initIters+maxPeriod)*numTries,"iterations. Consider increasing initIters."))
    return(FALSE)
  }
)

dsmodel = function(fun){
  dsproto(`_class` = "dsmodel",
          `_inherit` = absmodel,
          fun=fun
  )
}

hasDefault = function(x) {
  return(!is.name(x) || nzchar(x))
}

dsassert = function(t,str,critical=FALSE) {
  if(!t) {
    if(critical)
      stop(paste("Critical error:",str,". Please notify developers."))
    else
      stop(str)
  }
}

#use formals/pryr to change xname to x and yname to y in func
#also moev aname to _a and bname to _b
getParamsOfModelFunc = function(func) { #returns list of up to 4 parameter names-x,y,a,b
  fformals = formals(func)
  allparams = names(fformals)
  defaults = unlist(lapply(allparams, function(k) { hasDefault(fformals[[k]]) }))
  openParams = allparams[!defaults]
  if(length(openParams) == 4)
    params = openParams
  else if (length(allparams) >= 4)
    params = allparams
  else
    stop("AAAAA")
  if(any(c("x", "X") %in% params) && any(c("y","Y") %in% params)) {
    modelParams = setdiff(params, c("x", "X", "y", "Y"))
    dsassert(length(modelParams)>=2, "AAAAAHMORE")
    modelParams[1:2]
  } else {
    params[1:2]
  }
}

parammodel = function(fun, ..., paramNames = NULL) {
  givenNames = substitute(paramNames)
  if(is.null(givenNames)) {
    paramNames = getParamsOfModelFunc(fun)
    aname <- paramNames[1]
    bname <- paramNames[2]
  } else {
    aname <- as.character(givenNames[2])
    bname <- as.character(givenNames[3])
  }
  dotvals = list(...)
  dotnames = names(dotvals)
  if(aname %in% dotnames && bname %in% dotnames)
  {
    a = list(...)[[aname]]
    b = list(...)[[bname]]
  } else if (...length() == 2) {
    a = ..1
    b = ..2
  } else {
    a = NULL
    b = NULL
  }
  #print(c(aname,a,bname,b))
  dsproto(`_class` = "parammodel",
          `_inherit` = absmodel,
          aname = aname,
          bname = bname,
          a=a,
          b=b,
          fun=fun,
          apply=function(self,x,y,...,iters=1,accumulate=TRUE,crop=TRUE,trace=FALSE){
            if(trace){
              print("parammodel apply")
              print(as.list(match.args()))
              print(as.list(environment()))
            }
            if(...length()==2){
              self$parent$apply(x=x,y=y,...,iters=iters, accumulate=accumulate, crop=crop, trace=trace)
            }
            else{
              if(is.null(self$a) || is.null(self$b)) {
                print("Cannot evalaute!")
              }
              else {
                args = list(x=x,y=y, trace=trace, iters=iters, accumulate=accumulate, crop=crop)
                args[[self$aname]] = self$a
                args[[self$bname]] = self$b
                do.call(self$parent$apply, args)
              }
            }
          }
  )
}

bifmap = function(testX,testY, xlim=NULL,ylim=NULL,cols=NULL,discretize=0,
                  initIters=1000, maxPeriod=128, numTries=1,
                  epsilon=sqrt(sqrt(.Machine$double.eps)), rangeMult=0){
  dsproto(
    x=testX,
    y=testY,
    xlim=xlim,
    ylim=ylim,
    initIters=initIters, maxPeriod=maxPeriod, numTries=numTries,
    epsilon=epsilon, rangeMult=rangeMult,
    discretize=discretize,
    grid=NULL,
    colMatrix=NULL,
    cols=cols,
    bound=FALSE,
    on.bind = function(self, model){
      self$bound=TRUE
      self$calculate.grid(model)
      self$calculate.bifmap(model)
      self$render()

    },
    calculate.grid = function(self, model){
      self$grid=model$paramrange$corners(discretize,xlim=xlim,ylim=ylim)
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








modelx = parammodel(m=4,n=3,fun= function(x,y,a=0.7,b=0.9,m=3,n=2){
  #print(environment())
  #print(y)
  print(paste(c("m",m,"n",n,"x",x,"y",y),collapse = " "))
  if(m>1&&n>1){
    list(x+1,y)
  }
  else{
    list(x,y+1)
  }
} )



#
# print("Stop a")
# print(unlist(dsmodel$apply(1)))
# dsmodel$val = 4
# print("STop b")
# print(unlist(dsmodel$apply(1)))
#
#print(unlist(modelx$apply(.5,.5)))
#print(unlist(modelx$apply(c(1,2,4),c(1,4,5),m=1,n=1, iters=5,crop=FALSE,accumulate=FALSE)))


logisticmodel=parammodel(
  function(x,y,a,b){
    list(x=a*x*(1-x),y=y)
  }
)

#manualy give a paramrange
logisticmodel$paramrange=dsrange(c(2,4),c(0,.1),discretize = .1)

bif=bifmap(.5,.5)

#manualy render
#bif$calculate.grid(logisticmodel)
#bif$calculate.bifmap(logisticmodel)
#bif$bound=TRUE
#bif$render()

#manualy bind
bif$on.bind(logisticmodel)

sometimesstable= parammodel( function(x,y,a,b){list(a*x,b*y)})
#right now numtires breaks it
bif=bifmap(1,1,numTries = 5)
bif$on.bind(sometimesstable)

#bif$calculate.grid(sometimesstable)
#bif$calculate.bifmap(sometimesstable)
#bif$bound=TRUE
#bif$render()

