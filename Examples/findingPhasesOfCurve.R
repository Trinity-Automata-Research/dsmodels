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
curve=dscurve(x/2, n=100, xlim=c(2,2.71))
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
phases=mapply(mkphase,1:(length(segments)-1))
