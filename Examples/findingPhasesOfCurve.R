#make points
#similar to dscruve
f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}
model=dsmodel(f)
range = paramrange(alim=3,blim=3,paramNames = c(s,r))
model + range
#curve=dscurve(function(x)x/2, display = TRUE)
curve=dscurve(x/2, n=100, xlim=c(0,2.68),display = FALSE)
model+curve
#model + sim.map.period(.5,.5, discretize=.2, maxPeriod = 4, epsilon=.001, iters = 100, numTries = 1, powerOf2=TRUE)

#numPoints=10000
#amin = min(range$alim)
#amax = max(range$alim)
#as=seq(amin,amax,length.out=numPoints)
#bs=mapply(curve$fun,as)
testX=1
testY=1
args=list(FUN=model$find.period,x=testX,y=testY, numTries=10) #,the rest of args
args[[range$aname]]=curve$xValues
args[[range$bname]]=curve$yValues
periods=do.call(what=mapply,args=args)

#pointsWithPeriods=mapply(c,as,periods)

#bifSpots=list()
#spot=1
#for(i in 2:length(periods)-1){
#  if(periods[[i]]!=periods[[i+1]]){
#    print(c(curve$xValues[[i]],periods[[i]],"to",curve$xValues[[i+1]],periods[[i+1]]))
#  }
  #if(periods[[i]]!=periods[[i+1]]){
  #  bifSpots[[spot]]c(as[[i]],periods[[i]])
  #  spot=spot+1
  #}
#}

#we should stop this when we hit chaos
#turn list of data frames into list of segments
points = list(x=curve$xValues, y=curve$yValues, periods=periods)
#list.group(points, periods)
points=data.frame(points)
segments=split.data.frame(points,as.factor(periods))

segments=lapply(FUN=function (s) {list(x=segments[[s]]$x, y=segments[[s]]$y, color=as.numeric(s))}, X=names(segments))
#print(segments)

#make list of phases
mkphase = function(seg){
  start=min(seg$x)
  stop=max(seg$x)
  p = seg$color
  list(start=start,period=p,stop=stop)
}
#skip 0 by starting at 2? probably not what we actually want. we should filter segments for divergent and chaotic
phases=mapply(mkphase,segments)






processPhase=function(phaseNum){ #done for 1:(length(phases)-1)
  phase=phases[,phaseNum]
  phase=phases[,phaseNum]
  #bin search for inflection. for now just taking the end of the phase
  inflectionX=phase$stop
  inflectionY=curve$fun(inflectionX)
  segments[[phaseNum+1]]$x=append(segments[phaseNum+1]$x,inflectionX)
  segments[[phaseNum+1]]$y=append(segments[phaseNum+1]$y,inflectionY)
  list(period=phase$period,x=inflectionX,y=inflectionY)
}
#inflections=mapply(processPhase,1:ncol(phases))

#narrowing:
#newPhases=narrow(phases[,1],phase[,2],...)
#for( i in 2:ncol(phases)){
# last=ncol(newPhases)
# new= narrow(newPhases[,last], phases[,i],...)
# newPhases=cbind(newPhases[,1:(last-1)],new) #newPhases-last of newPhases +new
#}

 #also takes curvefun, model but for now thoes are int hte global scope
#if not within tolerance, add midpoint to appropriate segment?, call again with mid
#else make new phases with right stop,start
narrow= function(prev,post,tolerance=sqrt(sqrt(.Machine$double.eps))){
  x1=prev$stop
  x2=post$start
  if(x2-x1<tolerance){
    return(cbind(prev,post))
  }
  p1=prev$period
  p2=post$period
  x=(x1+x2)/2
  y=curve$fun(x)
  args=list(x=testX,y=testY, numTries=10) #,the rest of args
  args[[range$aname]]=x
  args[[range$bname]]=y
  p=do.call(model$find.period,args)
  if(p>p1){
    if(p<p2){ #new phase in between
      mid=list(start=x,period=p,stop=x)
      prev=narrow(prev,mid,tolerance)
      post=narrow(mid,post,tolerance)
      lenPrev=ncol(prev)
      midStart=prev[,lenPrev]$start
      post[,1]$start=midStart
      return(cbind(prev[,1:(lenPrev-1)],post))
    }
    else{
      #midpoint goes into post
      post$start=x
    }
  }
  else{
    #midpoint goes into prev
    prev$stop=x
  }
  return(narrow(prev,post,tolerance))

}

#binary search for inflection
#takes in 2 periods and 2 x(a) values
#base case x1-x2<epsilon- return x1
#compute p=periodicity of x=(x1+x2)/2.
#if p>p1 return(binsearch(x,p,x2,p2))
#else p<p2, so return(binsearch(x1,p1,x,p))
binSearch= function(x1,p1,x2,p2, epsilon=sqrt(sqrt(.Machine$double.eps))){
  if(x2-x1<epsilon){
    return(x1)
  }
  x=(x1+x2)/2
  y=curve$fun(x)
  args=list(x=testX,y=testY, numTries=10) #,the rest of args
  args[[range$aname]]=x
  args[[range$bname]]=y
  p=do.call(model$find.period,args)
  if(p>p1){
    if(p<p2)
      return(c(binSearch(x1,p1,x,p,epsilon),binSearch(x,p,x2,p2,epsilon)))
    return(binSearch(x1,p1,x,p,epsilon))
  }
  return(binSearch(x,p,x2,p2,epsilon))
}




#from https://gist.github.com/Jfortin1/72ef064469d1703c6b30
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

numCol=length(segments)
self=list(color=NULL) #temporary. will be removed when this is converted into a dsproto
#slightly darker version of simmapperiod's colors
if(is.null(self$cols) || length(self$cols)<numCol){
  if (numCol <= 6)
    self$cols <- darken(c("yellow", "magenta", "orange", "green", "red", "blue"))
  else if (numCol <= 28)
    self$cols <- darken(c("#00119c","#cdff50","#8d00a9","#00b054","#ff40dd","#01f9be","#ff1287","#2a73ff","#d99b00","#f5ff84","#3e004a","#91fffa","#ff455a","#00a5f3","#850f00","#9897ff","#0e2100","#e2b5ff","#005238","#ffa287","#12002c","#e2ffe0","#620045","#ffd3e1","#2b0a00","#0068b0","#5f1800","#00376f"))
  else
    self$cols <- rainbow(numCol) #warning? More colors needed
}

colMap=sort(unique(append(mapply(function(seg)seg$color,segments),c(1,0))))


for(i in 1:(length(segments))){
  print(segments[[i]]$y)
  lines(segments[[i]]$x, segments[[i]]$y, lwd = curve$lwd,
        col = self$cols[[which(colMap==segments[[i]]$color)]], ... = curve$...)
}
