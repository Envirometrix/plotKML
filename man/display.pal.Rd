\name{display.pal}
\alias{display.pal}
\title{Display a color palette}
\description{Plots a color palette in a new window.}
\usage{
display.pal(pal, sel=1:length(pal), names=FALSE)
}
\arguments{
  \item{pal}{list; each palette a vector of HEX-formated colors}
  \item{sel}{integer; selection of palettes to plot}
  \item{names}{logical; specifies whether to print also the class names}
}
\details{The internal palettes available in plotKML typically consists of 20 elements. If class names are requrested (\code{names=TRUE}) than only one palette will be plotted.}
\author{ Tomislav Hengl and Pierre Roudier }
\seealso{ \code{\link{SAGA_pal}}, \code{\link{R_pal}}, \code{\link{worldgrids_pal}}
}
\examples{
# SAGA GIS palette (http://saga-gis.org/en/about/software.html)
data(SAGA_pal)
names(SAGA_pal)
\dontrun{# display palettes:
display.pal(pal=SAGA_pal, sel=c(1,2,7,8,10,11,17,18,19,21,22))
dev.off()
data(worldgrids_pal)
worldgrids_pal[["globcov"]]
display.pal(pal=worldgrids_pal, sel=c(5), names = TRUE)
dev.off()
# make icons (http://www.statmethods.net/advgraphs/parameters.html):
for(i in 0:25){
  png(filename=paste("icon", i, ".png", sep=""), width=45, height=45, 
  bg="transparent", pointsize=16)
  par(mar=c(0,0,0,0))
  plot(x=1, y=1, axes=FALSE, xlab='', ylab='', pch=i, cex=4, lwd=2)
  dev.off()
}
}
}
\keyword{spatial}