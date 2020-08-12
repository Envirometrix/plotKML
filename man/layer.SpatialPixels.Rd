\name{kml_layer.SpatialPixels}
\alias{kml_layer.SpatialPixels}
\alias{SpatialPixels}
\title{Writes SpatialPixels or SpatialGrid objects to KML}
\description{Writes sp classes \code{"SpatialGrid"} or \code{"SpatialPixels"} to PNG images and makes a KML document (ground overlays). Target attributes can be specified using aesthetics arguments (e.g. \code{"colour"}).}
\usage{
kml_layer.SpatialPixels(obj, subfolder.name = paste(class(obj)), raster_name, 
     plot.legend = TRUE, metadata = NULL, 
     png.width = gridparameters(obj)[1,"cells.dim"], 
     png.height = gridparameters(obj)[2,"cells.dim"], 
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
\details{Google Earth does not properly handle a 24-bit PNG file which has a single transparent color (read more at \href{http://groups.google.com/group/earth-help/}{Google Earth Help}). To force transparency, plotKML will try to set it using the \code{-matte -transparent "\#FFFFFF"} option in the \href{http://www.imagemagick.org/Usage/masking/}{ImageMagick convert program} (ImageMagick needs to be installed separately and located using \code{plotKML.env()}). The PNG export uses the 'cairographics', which will never use a palette and normally creates a larger 32-bit ARGB file, but then always allows transparancy. On some Unix run machines the \code{png.type} argument has to be set manually to avoid producing empty PNGs.
}
\author{Tomislav Hengl, Pierre Roudier and Dylan Beaudette}
\seealso{ \code{\link{kml-methods}}, \code{\link{kml_open}}, \code{\link{kml_layer.Raster}}, \code{\link{plotKML-method}} }
\examples{
data(eberg_grid)
library(sp)
library(rgdal)
library(raster)
coordinates(eberg_grid) <- ~x+y
gridded(eberg_grid) <- TRUE
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
data(SAGA_pal)
\dontrun{## KML plot with a single raster:
kml(eberg_grid, colour_scale = SAGA_pal[[1]], colour = TWISRT6)
## make a larger image:
kml(eberg_grid, colour_scale = SAGA_pal[[1]], colour = TWISRT6, 
  png.width = 600, png.height = 600)
}
}
\keyword{spatial}                          
