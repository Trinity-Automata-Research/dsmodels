f <- function(x,y) list(x+1, 1)

x <- c(1,2,3,4)
y <- c(10,20,30,40)
for(i in 1:10){
 tmp <- f(x,y)
 x <- tmp[[1]]
 y <- tmp[[2]]
}

xp <- c(1,2,3,4)
yp <- c(10,20,30,40)
for(i in 1:5){
  tmp <- mapply(f, x=xp, y=yp)
  #names(tmp) <- c("x","y")
  xp <- tmp[1]
  yp <- tmp[2]
}
