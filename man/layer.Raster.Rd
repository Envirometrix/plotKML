\name{kml_layer.Raster}
\alias{kml_layer.Raster}
\alias{RasterLayer}
\title{Writes raster objects to KML}
\description{Writes rasters to PNG images and makes a KML code (ground overlays). Works with \code{"RasterLayer"} and \code{"RasterStack"} class objects. Target attributes can be specified using aesthetics arguments (e.g. \code{"colour"}).}
\usage{
kml_layer.Raster(obj, subfolder.name = paste(class(obj)), plot.legend = TRUE, 
     metadata = NULL, raster_name, 
     png.width = ncol(obj), png.height = nrow(obj), 
     min.png.width = 800, TimeSpan.begin, TimeSpan.end,
     layer.name, png.type, \dots)
}
\arguments{
  \item{obj}{object of class \code{"RasterLayer"}, \code{"SpatialPixelsDataFrame"} or \code{"SpatialGridDataFrame"}}
  \item{subfolder.name}{character; optional subfolder name} 
  \item{plot.legend}{logical; specify whether a map legend should be generated automatically}
  \item{metadata}{(optional) specify the metadata object}
  \item{raster_name}{(optional) specify the output file name (PNG)}
  \item{png.width}{(optional) width of the PNG file}
  \item{png.height}{(optional) height of the PNG file}
  \item{min.png.width}{(optional) minimum width of the PNG file}
  \item{TimeSpan.begin}{object of class \code{"POSIXct"}; (optional) begin of the sampling period}
  \item{TimeSpan.end}{object of class \code{"POSIXct"}; (optional) end of the sampling period}
  \item{layer.name}{character; optional layer name}
  \item{png.type}{character; PNG type} 
  \item{\dots}{additional \code{\link{aesthetics}} arguments}
}  
\details{Google Earth does not properly handle a 24-bit PNG file which has a single transparent color (read more at \href{http://groups.google.com/group/earth-help/}{Google Earth Help}). To force transparency, plotKML will try to set it using the \code{-matte -transparent "\#FFFFFF"} option in the \href{http://www.imagemagick.org/Usage/masking/}{ImageMagick convert program} (ImageMagick needs to be installed separately and located using \code{plotKML.env()}). On some Unix run machines the \code{png.type} argument has to be set manually to avoid producing empty PNGs.
}
\author{Tomislav Hengl, Pierre Roudier and Dylan Beaudette}
\seealso{ \code{\link{kml-methods}}, \code{\link{kml_open}}, \code{\link{kml_layer.RasterBrick}}, \code{\link{plotKML-method}} }
\examples{
data(eberg_grid)
library(sp)
coordinates(eberg_grid) <- ~x+y
gridded(eberg_grid) <- TRUE
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
data(SAGA_pal)
library(raster)
r <- raster(eberg_grid["TWISRT6"])
\dontrun{# KML plot with a single raster:
kml(r, colour_scale = SAGA_pal[[1]], colour = TWISRT6) 
}
}
\keyword{spatial}
