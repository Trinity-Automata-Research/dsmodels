mod <- dsmodel(function(x,y)list(x,y)) + dsrange(5,5)
mod1 <- dsmodel(function(x,y)list(x+1,y+1)) + dsrange(5,5, 0.5, axes= FALSE)
mod1 + dsarrows()

mod2 <- dsmodel(function(x,y)list(x+1,y+1)) + dsrange(5,5, 0.5, frame.plot = FALSE)
mod2 + dsarrows()

mod3 <- dsmodel(function(x,y)list(x+1,y+1)) + dsrange(5,5, 0.5, axes = FALSE, frame.plot = FALSE)
mod3 + dsarrows()
