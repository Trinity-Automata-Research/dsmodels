#testing sim.map.period. some of the parameter and function names are subjectto change

#expected outputs:
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
m+paramrange(a=2,b=2,x=2,y=2,discretize = 1)
m+sim.map.period(1,1,rangeMult=3)

#FIXME------- if model has a range, x/ylims dont get overidden. not that important.
#add range first, defaults in sim (should give warnigs about assuming divergence, only show blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+paramrange(a=2,b=2,x=2,y=2,discretize = 1)
m+sim.map.period(1,1,alim=1,blim=1,xlim=Inf,ylim=Inf,discretize = .5)

#dont add range, no defaults in sim (should crash)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
#m+sim.map.period(1,1)

#dont add range, param defaults in sim, but no x,y defaults (red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+sim.map.period(1,1,alim=2,blim=2,discretize = 1)

#dont add range, param defaults and x,y defaults in sim (should give no warnigs about assuming divergence, red and blue)
m=dsmodel( function(x,y,a,b){list(a*x,b*y)})
m+sim.map.period(1,1,alim=2,blim=2, xlim=2,ylim=2,discretize = 1, rangeMult = 3)