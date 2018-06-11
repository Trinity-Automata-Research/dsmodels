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
curve=dscurve(x/2, n=100, xlim=c(0,4),display = FALSE)
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
mkphase = function(x){
  pre=max(segments[[x]]$x)
  post=min(segments[[x+1]]$x)
  p = segments[[x+1]]$color
  list(pre=pre,period=p,post=post)
}
#skip 0 to 1 by starting at 2
phases=mapply(mkphase,2:(length(segments)-1))

processPhase=function(phaseNum){
  phase=phases[,phaseNum]
  #bin search for inflection. for now just taking pre
  inflectionX=phase$pre
  inflectionY=curve$fun(inflectionX)
  segments[[phaseNum+1]]$x=append(segments[phaseNum+1]$x,inflectionX)
  segments[[phaseNum+1]]$y=append(segments[phaseNum+1]$y,inflectionY)
  list(period=phase$period,x=inflectionX,y=inflectionY)
}
inflections=mapply(processPhase,1:ncol(phases))

numCol=length(segmens)
self=list(color=NULL) #temporary. will be removed when this is converted into a dsproto
if(is.null(self$cols) || length(self$cols)<numCol){
  if (numCol <= 6)
    self$cols <- c("yellow", "magenta", "orange", "green", "red", "blue")
  else if (numCol <= 28)
    self$cols <- c("#00119c","#cdff50","#8d00a9","#00b054","#ff40dd","#01f9be","#ff1287","#2a73ff","#d99b00","#f5ff84","#3e004a","#91fffa","#ff455a","#00a5f3","#850f00","#9897ff","#0e2100","#e2b5ff","#005238","#ffa287","#12002c","#e2ffe0","#620045","#ffd3e1","#2b0a00","#0068b0","#5f1800","#00376f")
  else
    self$cols <- rainbow(numCol) #warning? More colors needed
}

for(i in 1:(length(segments))){
  print(segments[[i]]$y)
  lines(segments[[i]]$x, segments[[i]]$y, lwd = curve$lwd,
        col = self$cols[[i]], ... = curve$...)
}
