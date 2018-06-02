absmodel = dsproto(`_class` = "model",
                   `_inherit` = NULL,
                   apply=function(self,x,y,...,trace=FALSE){
                     if(trace){
                       print("absmodel apply")
                       print(as.list(environment()))
                     }
                     self$func(...,x,y)
                   },
                   applyN=function(self,x,y,n,...){
                     tmp=list(x,y)
                     for(i in 1:n) {
                       tmp <- self$apply(tmp[[1]],tmp[[2]],...)
                     }
                     tmp
                   }

)

dsmodel = dsproto(`_class` = "dsmodel",
                  `_inherit` = absmodel,
                  func=function(x,y){list(x+y,y)},
                  val=2,
                  apply=function(self,x){
                    #print("Dsmodel apply")
                    #print(as.list(environment()))
                    self$parent$apply(x=x,y=self$val)
                  }

)

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

getParamsOfModelFunc = function(func) {
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

parammodel = function(func, ..., paramNames = NULL) {
  givenNames = substitute(paramNames)
  if(is.null(givenNames)) {
    paramNames = getParamsOfModelFunc(func)
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
  print(c(aname,a,bname,b))
  dsproto(`_class` = "parammodel",
          `_inherit` = absmodel,
          aname = aname,
          bname = bname,
          a=a,
          b=b,
          func=func,
          apply=function(self,x,y,...,trace=FALSE){
            if(trace){
              print("parammodel apply")
              print(as.list(match.args()))
              print(as.list(environment()))
            }
            if(...length()==2){
              self$parent$apply(x=x,y=y,...,trace=trace)
            }
            else{
              if(is.null(self$a) || is.null(self$b)) {
                print("Cannot evalaute!")
              }
              else {
                print("Doing the thing")
                args = list(x=x,y=y, trace=trace)
                args[[self$aname]] = self$a
                args[[self$bname]] = self$b
                print(names(args))
                do.call(self$parent$apply, args)
              }
            }
          }
  )
}

modelx = parammodel(m=4,n=3,func= function(a=0.7,b=0.9,m=3,n=2,x,y){
  print(environment())
  print(y)
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
print(unlist(modelx$apply(.5,.5)))
print(unlist(modelx$applyN(c(1,2,4),c(1,4,5),5,m=1,n=1)))

#fnamed=function(a=1,b=2,c=3,d=4){}
#funnamed=function(a,b,c,d){}

#interesting functions: substitue, args


foo <- function(x=NULL) {
  k <- substitute(x)
  if(is.null(k))
  {
    5
  }
  else {
    print(typeof(k[2]))
    list(a <- as.character(k[2]), b <- as.character(k[3]))
    k
  }
}
