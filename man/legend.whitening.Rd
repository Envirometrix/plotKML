\name{kml_legend.whitening}
\alias{kml_legend.whitening}
\title{Whitening legend (PNG)}
\description{Produces a PNG file that can be used in KML plots (visualization of uncertainty).}
\usage{
kml_legend.whitening(legend.res = 0.01, width = 120, height = 300, pointsize = 14, 
                 x.lim, e.lim, leg.asp = 0.3 *  width/height, 
                 legend.file = "whitening_legend.png",
                 matte = FALSE, png.type = "cairo-png")
}
\arguments{
  \item{legend.res}{numeric; resolution on a 0-1 scale}
  \item{width}{integer; image width}
  \item{height}{integer; image height}
  \item{pointsize}{integer; point size in units for text}
  \item{x.lim}{numeric; upper and lower limits for target variable}
  \item{e.lim}{numeric; upper and lower limits for the normalized error}
  \item{leg.asp}{numeric; legend aspect}
  \item{legend.file}{character; output PNG file name}
  \item{matte}{logical; specify whether to fix transparency using ImageMagick}
  \item{png.type}{character; PNG type}
}
\details{The output PNG file shows a 2D legend with values on the vertical axis and uncertainty on the horizontal axis. Whitening is only valid with Hue-Saturation-Intensity system where Hue's are used to represent values of the target variable, so that the amount of white color can be linearly used to represent uncertainty (i.e. whitening can not be used with different color palettes; or at least we do not recommend this).}
\note{Google Earth does not properly handle a 24-bit PNG file which has a single transparent color. In order to force transparency in the output PNG, the function with try using ImageMagick convert function. ImageMagick needs to be installed separately and located using \code{plotKML.env()}.}
\references{
\itemize{
\item Hengl, T., Heuvelink, G.M.B., Stein, A., (2004) \href{http://dx.doi.org/10.1016/j.geoderma.2003.08.018}{A generic framework for spatial prediction of soil variables based on regression-kriging}. Geoderma 122 (1-2): 75-93.
\item Hengl, T., (2003) \href{http://www.geocomputation.org/2003/Papers/Hengl_Paper.pdf}{Visualisation of uncertainty using the HSI colour model: computations with colours}. 7th International Conference on GeoComputation (CD-ROM), p. 8.
}
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{whitening}}
}
\examples{
\dontrun{# create the 2D legend for whitening (PNG file):
kml_legend.whitening(x.lim=c(5,20), e.lim=c(.6,1))
}
}
