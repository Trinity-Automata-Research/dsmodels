# This file is copied from the TeachingDemos package with the consent of Greg Snow, the package's creator. The file is here relicensed in
# accordance with the Artistic 2.0 license under the GPL 2 license.

shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
	theta= seq(pi/4, 2*pi, length.out=8), r=0.1, ... ) {

	xy <- xy.coords(x,y)
	xo <- r*strwidth('A')
	yo <- r*strheight('A')


	if(!(is.null(bg) || identical(bg,"") || identical(bg, "NA"))) {
	  for (i in theta) {
	    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
	  }
	}
	text(xy$x, xy$y, labels, col=col, ... )
}



#plot(1:10, 1:10, bg='aliceblue')
#rect(3,3,5,8, col='navy')
#text(5,6, 'Test 1', col='lightsteelblue')
#shadowtext(5,4, 'Test 2', col='lightsteelblue')
