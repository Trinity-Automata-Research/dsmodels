model <- dsmodel(function(x, y) list(x, y))

model + dsrange(0:3, 0:3)

# Produces a curve with a label automatically created from the arguments passed in.
model + dscurve(x, label = "y = x", labelLoc = 0.5, labelCol = "black", labelBg = "white")
