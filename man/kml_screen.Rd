\name{kml_screen}
\alias{kml_screen}
\title{Add a screen overlay}
\description{Adds an image file (map legend or logo) as screen overlay. The same file connection is further accessible by other \code{kml_*()} functions such as \code{kml_layer()} and \code{kml_close()}. This allows creation of customized multi-layered KML files.}
\usage{
kml_screen(image.file, sname = "", 
     position = c("UL","ML","LL","BC","LR","MR","UR","TC")[1],
     overlayXY, screenXY, xyunits = c("fraction", "pixels", "insetPixels")[1], 
     rotation = 0, size = c(0,0) )
}
\arguments{
  \item{image.file}{image file to be used for screen overlay}
  \item{sname}{screen overlay name}
  \item{position}{one of the nine standard positions}
  \item{overlayXY}{manually specified tie point on the overlay image e.g. \code{'x="0" y="1"'}}
  \item{screenXY}{manually specified matching tie point on the scren e.g. \code{'x="0" y="1"'}}
  \item{xyunits}{values of the XY units ((\code{"pixels"}, \code{"fraction"}, or \code{"insetPixels"})}
  \item{rotation}{(optional) rotation in degrees clock-wise}
  \item{size}{size correction in \emph{x} and \emph{y} direction}
}
\details{If nothing else is specified the function looks for some of the nine typical positions: \code{"UL"} (upper left), \code{"ML"} (middle left), \code{"LL"} (lower left), \code{"BC"} (bottom centre), \code{"LR"} (lower right), \code{"MR"} (middle right), \code{"UR"} (upper right), and \code{"TC"} (top centre). The \emph{x} and \emph{y} values can be specified in three different ways: as pixels (\code{"pixels"}), as fractions of the image (\code{"fraction"}), or as inset pixels (\code{"insetPixels"}) --- an offset in pixels from the upper right corner of the image.}
\note{The function, by default, calculates with fractions. If you change the \code{xyunits} type, all other elements need to be expressed in the same units.}
\references{
\itemize{
\item KML Reference (\url{http://code.google.com/apis/kml/documentation/})
}
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{kml-methods}}
}
\examples{
library(rgdal)
library(sp)
data(eberg_zones)
\dontrun{# add logo in the top-center:
kml_open("eberg_screen.kml")
kml_layer(eberg_zones)
logo = "http://meta.isric.org/images/ISRIC_right.png"
kml_screen(image.file = logo, position = "TC", sname = "ISRIC logo")
kml_close("eberg_screen.kml")
kml_compress("eberg_screen.kml")
}
}
