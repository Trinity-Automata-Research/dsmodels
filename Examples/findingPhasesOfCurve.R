#make points
#similar to dscruve
if(!exists("model")){
f=function(x,y,a=.5,b=.5,s=1,r=1,dummy=0){
  list(x*exp(r-x-a*y),
       y*exp(s-b*x-y))
}
model=dsmodel(f)
range = paramrange(alim=3,blim=3,paramNames = c(s,r))
model + range
}
#curve=dscurve(function(x)x/2, display = TRUE)


#start here if show and tell has already been run

curve=dscurve(x/1.5, n=100, xlim=c(0,3),display = FALSE)
model+curve
#model + sim.map.period(.5,.5, discretize=.2, maxPeriod = 4, epsilon=.001, iters = 100, numTries = 1, powerOf2=TRUE)

#numPoints=10000
#amin = min(range$alim)
#amax = max(range$alim)
#as=seq(amin,amax,length.out=numPoints)
#bs=mapply(curve$fun,as)
testX=.1
testY=.1

args=list(FUN=model$find.period,x=testX,y=testY, numTries=10, maxPeriod=512) #,the rest of args
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

transitions = rle(periods)
p = cumsum(transitions$lengths)
n = length(p)
starts = c(1,(p+1)[-n])
ends = p
phases = data.frame(astart = curve$xValues[starts],
                    bstart = curve$yValues[starts],
                    period = transitions$values,
                    astop  = curve$xValues[ends],
                    bstop  = curve$yValues[ends])



segments = vector("list", length=length(ends))
for(i in 1:length(ends)) {
  phase = starts[i]:ends[i]
  segments[[i]] = data.frame(x = curve$xValues[phase], y = curve$yValues[phase], period=periods[phase])
}


#processPhase=function(phaseNum){ #done for 1:(length(phases)-1)
#  phase=phases[,phaseNum]
#  #bin search for inflection. for now just taking the end of the phase
#  inflectionX=phase$stop
#  inflectionY=curve$fun(inflectionX)
#  segments[[phaseNum+1]]$x=append(segments[phaseNum+1]$x,inflectionX)
#  segments[[phaseNum+1]]$y=append(segments[phaseNum+1]$y,inflectionY)
#  list(period=phase$period,x=inflectionX,y=inflectionY)
#}
#inflections=mapply(processPhase,1:ncol(phases))

#narrowing:
#newPhases=narrow(phases[,1],phase[,2],...)
#for( i in 2:ncol(phases)){
# last=ncol(newPhases)
# new= narrow(newPhases[,last], phases[,i],...)
# newPhases=cbind(newPhases[,1:(last-1)],new) #newPhases-last of newPhases +new
#}

#sqdist from end of prev to start of post
#eventually should be abstracted for parametric functions too
phaseDist=function(prev,post){
  x1=prev$astop
  y1=prev$bstop
  x2=post$astart
  y2=post$bstart
  sqdist(c(x1,y1),c(x2,y2))
}

 #also takes curvefun, model but for now thoes are in the global scope
#if not within tolerance, add midpoint to appropriate segment?, call again with mid
#else make new phases with right stop,start
narrow= function(prev,post,tolerance=sqrt(sqrt(.Machine$double.eps))){
  #print(c(prev,post))
  if(phaseDist(prev,post) < tolerance){ #xydist
    return(rbind(prev,post))
  }
  x1=prev$astop
  x2=post$astart
  p1=prev$period
  p2=post$period
  x=(x1+x2)/2
  y=curve$fun(x)
  args=list(x=testX,y=testY, numTries=10, maxPeriod=512, epsilon=.0000001) #,the rest of args
  args[[range$aname]]=x
  args[[range$bname]]=y
  p=do.call(model$find.period,args)
  if(p!=p1){
    if(p!=p2){ #new phase in between
      mid=list(astart=x,bstart=y ,period=p,astop=x, bstop=y)
      prev=narrow(prev,mid,tolerance)   #compute both sides
      post=narrow(mid,post,tolerance)
      lenPrev=nrow(prev)
      midaStart=prev[lenPrev,]$astart   #merge the result from both sides
      midbStart=prev[lenPrev,]$bstart
      post[1,]$astart=midaStart
      post[1,]$bstart=midbStart
      return(rbind(prev[1:(lenPrev-1),],post))
    }
    else{
      #midpoint goes into post
      post$astart=x
      post$bstart=y
    }
  }
  else{
    #midpoint goes into prev
    prev$astop=x
    prev$bstop=y
  }
  return(narrow(prev,post,tolerance))

}

#binary search for inflection
#takes in 2 periods and 2 x(a) values
#base case x1-x2<epsilon- return x1
#compute p=periodicity of x=(x1+x2)/2.
#if p>p1 return(binsearch(x,p,x2,p2))
#else p<p2, so return(binsearch(x1,p1,x,p))
##binSearch= function(x1,p1,x2,p2, epsilon=sqrt(sqrt(.Machine$double.eps))){
#  if(x2-x1<epsilon){
#    return(x1)
#  }
#  x=(x1+x2)/2
#  y=curve$fun(x)
#  args=list(x=testX,y=testY, numTries=10) #,the rest of args
#  args[[range$aname]]=x
#  args[[range$bname]]=y
#  p=do.call(model$find.period,args)
#  if(p>p1){
#    if(p<p2)
#      return(c(binSearch(x1,p1,x,p,epsilon),binSearch(x,p,x2,p2,epsilon)))
#    return(binSearch(x1,p1,x,p,epsilon))
#  }
#  return(binSearch(x,p,x2,p2,epsilon))
#}
#



#from https://gist.github.com/Jfortin1/72ef064469d1703c6b30
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}
colMap=sort(unique(append(mapply(function(seg)seg$color,segments),c(1,0))))
numCol=length(colMap)
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


for(i in 1:(length(segments))){
  lines(segments[[i]]$x, segments[[i]]$y, lwd = curve$lwd,
        col = self$cols[[which(colMap==segments[[i]]$color)]], ... = curve$...)
}

addDistanceToPhase=function(inPhase){
  findDist=function(index,phases){
    sqrt((phases[index,]$astop-phases[index,]$astart)^2 + (phases[index,]$bstop-phases[index,]$bstart)^2)
  }
  dist=mapply(findDist,1:nrow(inPhase),MoreArgs=list(inPhase))
  withDist=cbind(inPhase,dist)
  findRatio=function(index,phases){
    (phases[index,]$dist)/(phases[index+1,]$dist)
  }
  ratio=append(NA,mapply(findRatio,1:(nrow(inPhase)-1),MoreArgs=list(withDist)))
  cbind(withDist,ratio)
}

#to find bifucation points
addDistanceToPhase(narrow(phases[1,],phases[nrow(phases),],tolerance=.000001))

#when r=s/1.5, periodicity at s=2.826480 is 128

curve=dscurve(x/1.5)
prev=list(astart=0,bstart=0,period=1,astop=0,bstart=0)
post=list(astart=2.826480,bstart=2.826480/1.5, period=256,astop=2.826480,bstop=2.826480/1.5)
addDistanceToPhase(narrow(prev,post,tolerance=.000001))

make.phases=function(startA,endA,tolerance=sqrt(sqrt(.Machine$double.eps))){
  startB=curve$fun(startA)
  endB=curve$fun(endA)
  args=list(x=testX,y=testY, numTries=10) #,the rest of args
  args[[range$aname]]=startA
  args[[range$bname]]=startB
  startP=do.call(model$find.period,args)
  args=list(x=testX,y=testY, numTries=10) #,the rest of args
  args[[range$aname]]=endA
  args[[range$bname]]=endB
  endP=do.call(model$find.period,args)

  prev=list(astart=startA,bstart=startB,period=startP,astop=startA,bstop=startB)
  post=list(astart=endA,bstart=endB,period=endP,astop=endA,bstop=endB)
  addDistanceToPhase(narrow(prev,post,tolerance))
}

#chaos is hanled much nicer now, but there is still
#a chance that you pick a phase whose endPoints share a periodicity even
#though there is a different periodicity in between.
#that means it is better to pick an endpoint very slightly into the chaos.
# for x/1.5, 2.82648 is just outside of non- chaos.
# i think 2.82647 is inside non- chaos

#out? 2.826447 2.8248105
#in?



