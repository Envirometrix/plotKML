\name{kml-methods}
\docType{methods}
\alias{kml}
\alias{kml-methods}
\alias{plotKML.fileIO}
\alias{kml.Spatial}
\alias{kml,Raster-method}
\alias{kml,STIDF-method}
\alias{kml,SoilProfileCollection-method}
\alias{kml,Spatial-method}
\alias{kml,SpatialPhotoOverlay-method}
\title{Write to a KML file}
\description{
Writes any \code{Spatial*} object (from the sp package) or \code{Raster*} object (from the raster package) to a KML file via the \code{plotKML.fileIO} environment. Various \emph{aesthetics} parameters can be set via \code{colour}, \code{alpha}, \code{size}, \code{shape} arguments. Their availability depends on the class of the object to plot.
}
\usage{
\S4method{kml}{Raster}(obj, folder.name, file.name, kmz, \dots)
\S4method{kml}{Spatial}(obj, folder.name, file.name, kmz, \dots)
\S4method{kml}{STIDF}(obj, folder.name, file.name, kmz, \dots)
\S4method{kml}{SoilProfileCollection}(obj, folder.name, file.name, kmz, \dots)
\S4method{kml}{SpatialPhotoOverlay}(obj, folder.name, file.name, kmz, \dots)
}
\arguments{
  \item{obj}{object inheriting from the \code{Spatial*} or the \code{Raster*} classes}
  \item{folder.name}{character; folder name in the KML file}
  \item{file.name}{character; output KML file name}
  \item{kmz}{logical; specief whether to compress the KML file}
  \item{\dots}{additional aesthetics arguments (see details below)}
}
\value{A KML file. By default parses the object name and adds a ".kml" extension.}
\details{To \code{kml} you can also pass \code{folder.name}, \code{file.name} (output file name *.kml), \code{overwrite} (logical; overwrites the existing file) and \code{kmz} (logical; specifies whether to compress the kml file) arguments. Gridded objects (objects of class \code{"SpatialGridDataFrame"} or \code{"RasterLayer"} require at least one aesthetics parameter to run, usually the \code{colour}.) 
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\seealso{
\code{\link{kml_open}}, \code{\link{kml_aes}}, \code{\link{kml_close}}, \code{\link{kml_compress}}
}
\examples{
# Plotting a SpatialPointsDataFrame object
library(rgdal)
data(eberg)
eberg <- eberg[runif(nrow(eberg))<.1,]
library(sp)
library(rgdal)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
\dontrun{# Simple plot
kml(eberg, file = "eberg-0.kml")
# Plot using aesthetics
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
kml(eberg, colour = SNDMHT_A, size = CLYMHT_A, 
   alpha = 0.75, file = "eberg-1.kml", shape=shape)
}
}
\keyword{spatial}
\keyword{methods}
