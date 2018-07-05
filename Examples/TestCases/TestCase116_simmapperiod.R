#testing sim.map.period. some of the parameter and function names are subjectto change

#expected outputs: ~~~note red and blue are probably different. as of now it is yellow and magenta
#red(r) and blue(b)-
# rrrr
# bbrr
# bbrr
#only blue-
# bbbb
# bbbb
# bbbb
#warnings should be "Assuming divergance: no period found after 1128 iterations. Consider increasing initIter"

#test cases

#add range first, no defaults in sim (should give no warnigs about assuming divergence, red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
#m+paramrange(alim=2,blim=2,xlim=2,ylim=2,discretize = 1)
m+paramrange(alim=2,blim=2,xlim=3,ylim=3,discretize = .5)
m+sim.map.period(1,1,crop=TRUE)

#FIXME part of issue #139------- if model has a range, x/ylims dont get overidden. not that important.
#add range first, defaults in sim (should give warnigs about assuming divergence, only show blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(alim=2,blim=2,xlim=2,ylim=2,discretize = 1)
m+sim.map.period(alim=1,blim=1,xlim=Inf,ylim=Inf,discretize = .5)

#dont add range, no defaults in sim (should crash)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
#m+sim.map.period(1,1)

#dont add range, param defaults in sim, but no x,y defaults (red and blue)
#with init iters =0, this calls everything past 1 chaotic instead of divergent.
#with init iters =1000, this calls it divergent
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+sim.map.period(alim=2,blim=2,discretize = 1) #initIters=1000)

#dont add range, param defaults and x,y defaults in sim (should give no warnigs about assuming divergence, red and blue)
#also no key for this one
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+sim.map.period(alim=2,blim=2, xlim=2,ylim=2,discretize = 1, crop = TRUE, key=FALSE)

#test case for #158 sim.map.period's xlims should override range's
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(alim=2,blim=2,xlim=0,ylim=0, discretize = .5)
#this should all be divergent
m+sim.map.period(testX=1,testY=1,crop=TRUE, iters=1, maxPeriod = 8, numTries = 10)
#this should be fixed when a,b<1 and divergent otherwise
m+sim.map.period(xlim=2,ylim=2,testX=1,testY=1,crop=TRUE, iters=1, maxPeriod = 8, numTries = 10)

m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(alim=2,blim=2,xlim=NULL,ylim=NULL, discretize = .5)
#this should crash, needs xlim/ylim
#m+sim.map.period(testX=1,testY=1,crop=TRUE)
#this should not crash
m+sim.map.period(testX=1,testY=1,xlim=1,ylim=1,crop=TRUE)
