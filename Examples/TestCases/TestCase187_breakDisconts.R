model <- dsmodel(function(x, y) list(x, y))

model + dsrange(-3:3, -3:3)

# Omitting the stretch parameter will yield a graph with an inaccurate vertical line connecting the two parts of this function. By passing in
# the stretch parameter, we look for two or more points outside the range of the model, and insert NaNs to force R to render the discontinuities.
model + dscurve(function(x) 1/x-0.000013296, n = 1000, stretch = -2)
