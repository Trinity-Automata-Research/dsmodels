model <- dsmodel(function(x, y) list(x, y))

curve1 <- dscurve(function(x) x)
curve2 <- dscurve(function(x) 5 - x)

model + dsrange(5, 5)
model + curve1 + curve2
model + dsintersection(curve1, curve2, col = "blue", pch = 21, size = 2, bg = "black", labels = "Intersection")
