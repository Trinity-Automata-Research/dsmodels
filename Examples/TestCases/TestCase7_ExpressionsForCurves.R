m <- dsmodel(function(x,y)list(x+1,y+1)) + dsrange(3,3,0.01)
a <- dscurve(2)
m+a
b <- dscurve(x^2)
m+b
#cl1_crv <- dscurve(1-t, t, col="red", n = 300, discretize = TRUE)
#m+cl1_crv

# safe.apply <- function(fun,inp){
#   tryCatch({fun(inp)},
#            error=function(e) { FALSE })
# }
# is.function.safe <- function(inp) safe.apply(is.function, inp)
# is.null.safe <- function(inp) safe.apply(is.null, inp)
# is.numeric.safe <- function(inp) safe.apply(is.numeric, inp)
#
# dispatchFun <- function(fun, par){
#   if(!is.function.safe(fun)){
#     if(is.numeric.safe(fun)){
#       if(par)
#         fun <- function(t) fun
#       else
#         fun <- function(x) fun
#     } else {
#       if(par)
#         fun <- make_function(alist(t=), substitute(fun), parent.frame())
#       else
#         fun <- make_function(alist(x=), substitute(fun), parent.frame())
#     }
#   }
#   fun
# }
