library(dsmodels)

r1 <- 2.6
r2 <- 2.6
m1 <- 6.45
m2 <- 6.25
s1 <- 4.5
s2 <- 4.5
b <- 0.15

#The two-dimenstional model expects a two-dimensional function that outputs a list of the x and y value(s).
def <- function(X0,Y0) {
  xp <- X0*exp(r1-X0-m1/(1+s1*X0))
  yp <- Y0*exp(r2-Y0-b*X0-m2/(1+s2*Y0))
  list(xp,yp)
}

#A model contains a function and a title.
model <- dsmodel(fun = def, title="Four interior fixed points")

#The field is the graph area. We could change the name if you like, for instance to dsplot
field <- dsrange(0:3,0:3,discretize = .2)

#By default the arrows will be scaled to the discretization parameter, and blue.
#They can be set manually with parameters.
model+field
model + dsarrows(head.length=0.15)

d1=(1-r1*s1)^2-4*s1*(m1-r1)
d2=(1-r2*s2)^2-4*s2*(m2-r2)
A1<- (r1*s1-1-sqrt(d1))/(2*s1)
A2<- (r2*s2-1-sqrt(d2))/(2*s2)
K1<- (r1*s1-1+sqrt(d1))/(2*s1)
K2<- (r2*s2-1+sqrt(d2))/(2*s2)

y11Func <- function(x){(-b*x*s2-1+r2*s2-sqrt((b*x*s2+1-r2*s2)^2-4*s2*(m2-r2+b*x)))/(2*s2)}
y12Func <- function(x){(-b*x*s2-1+r2*s2+sqrt((b*x*s2+1-r2*s2)^2-4*s2*(m2-r2+b*x)))/(2*s2)}

A11y <- y11Func(A1)
A12y <- y12Func(A1)
K11y <- y11Func(K1)
K12y <- y12Func(K1)

#dspoints carry a label and a color for the dot. The dot can be configured in size.
#We haven't yet done so, but we could also have the model check that the dspoints are (roughly) fixed.
#We could also try to categorize them, if that would be useful.
A1F <- dspoint(A1,A11y, col="yellow", label = expression(paste(group("(",list(A[1], y[A[11]]),")"))))
A2F <- dspoint(A1,A12y, col="orange", label = expression(paste(group("(",list(A[1], y[A[12]]),")"))))
K1F <- dspoint(K1,K11y, col="magenta", label = expression(paste(group("(",list(K[1], y[K[11]]),")"))))
K2F <- dspoint(K1,K12y, col="green", label = expression(paste(group("(",list(K[1], y[K[12]]),")"))))

#To add fixpoints to the model, you use the + operation. You can also combine them into a single group with +.
model+A1F+A2F+K1F+K2F
#You don't need to name the dspoints to add them, although having variables makes it easier to define regions.
model +
  dspoint(A1,0, label = expression(paste(A[1])), offset=c(0,-0.08)) +
  dspoint(K1,0, label = expression(paste(K[1])), offset=c(0,-0.08)) +
  dspoint(0,A2, label = expression(paste(A[2])), offset=c(-0.08,0)) +
  dspoint(0,K2, label = expression(paste(K[2])), offset=c(-0.08,0))

#regions are defined by the sequence of points and a color. They are rendered in the order they are added to the model.
model + dsregion(dspoint(0,0),pnt(0,A2), A1F, pnt(A1,0), col = "yellow")
model + dsregion(A1F, A2F, pnt(A1,3), pnt(3,3), pnt(3, K11y), K1F, col= "green")
model + dsregion(pnt(0,A2), pnt(0,3), pnt(A1,3), A1F, col="orange")
#If you prefer to use the polygon function, dspolygon will accept all the same parameters and be rendered appropriately.
model + dspolygon(x=c(A1,A1,K1,3,3,K1,A1),y=c(0,A11y,K11y,K11y,0,0,0),col="magenta",border=NA)
