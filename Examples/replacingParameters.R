library(pryr)
#modify_lang(x,f) changes every thing in x (the function body) to f(that thing)
#we would also need to use names(formals(g))[[index]]=newName


f=function(a,b){
  a=a+1
  a*2+b
}
f(1,2)

#only changes a in the function body, not in the parameters
#instead of quote(a) and quote(b), we can use as.symbol(aname/bname)
atob=function(x){
  if (is.name(x) && identical(x, quote(a))) return(quote(b))
  x
}
print(f)
g=modify_lang(f,atob)
print(g)
g(-300,1)

#changes a in the parameters to b
names(formals(g))[[1]]="b"

#only the first parameter matters
print(g(1,2))
print(g(1,6))
print(g)


#change instances of a to whatever is in bname
bname="bannana"
h=function(a,bannana){
  print(a)
  print(bannana)
}
print(h)
atobname=function(x){
  if (is.name(x) && identical(x, quote(a))) return(as.symbol(bname))
  x
}
g=modify_lang(h,atobname)
print(g)


#function to make an atob function with specified a's and b's
make.atob=function(a,b){
  function(x){
    if (is.name(x) && identical(x, as.symbol(a))) return(as.symbol(b))
    x
  }
}

#function to swap a with b in
swap=function(a,b,f){
  atob=make.atob(a,b)
  g=modify_lang(f,atob)
  names=names(formals(g))
  names(formals(g))[which(names==a)]=b
  g
}
