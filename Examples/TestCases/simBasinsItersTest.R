library(dsmodels)
model <- dsmodel(function(X0,Y0) {
  list(X0*exp(2.6-X0-6.45/(1+4.5*X0)),
       Y0*exp(2.6-Y0-0.15*X0-6.25/(1+4.5*Y0)))
})

model+dsrange(0:3,0:3,discretize =.08)
model+simattractors(col=c("red","blue","green","purple"))
model+simbasins(iters = 2, missingCol = "white")

