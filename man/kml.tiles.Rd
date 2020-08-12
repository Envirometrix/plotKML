\name{kml.tiles}
\alias{kml.tiles}
\title{Write vector object as tiled KML}
\description{Writes vector object as tiled KML. Suitable for plotting large vectors i.e. large spatial data sets.}
\usage{
kml.tiles(obj, folder.name, file.name, 
   block.x, kml.logo, cpus, home.url=".", desc=NULL, 
   open.kml=TRUE, return.list=FALSE, \dots)
}
\arguments{
  \item{obj}{\code{"SpatialPoints*"} or \code{"SpatialLines*"} or \code{"SpatialPolygons*"}; vector layer}
  \item{folder.name}{character; KML folder name}
  \item{file.name}{character; output KML file name}
  \item{block.x}{numeric; size of block in decimal degrees (geographical coordinates)}
  \item{kml.logo}{character; optional project logo file (PNG)}
  \item{cpus}{integer; specifies number of CPUs to be used by the snowfall package to speed things up}
  \item{home.url}{character; optional web-directory where the PNGs will be stored}
  \item{desc}{character; optional layer description}
  \item{open.kml}{logical; specifies whether to open the KML file after writing}
  \item{return.list}{logical; specifies whether to return list of tiled objects}
  \item{\dots}{(optional) \link{aesthetics} arguments (see \code{\link{aesthetics}})}  
}
\value{Returns a list of KML files.}
\note{This operation can be time-consuming for processing very large vectors. To speed up writing of KMLs, use the snowfall package.}
\author{Tomislav Hengl }
\seealso{\code{\link{plotKML}}, \code{\link{plotKML.GDALobj}}}
\examples{
\dontrun{
library(sp)
library(snowfall)
library(GSIF)
library(rgdal)

data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
## plot using tiles:
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
tiles.p <- kml.tiles(eberg["SNDMHT_A"], block.x=0.05, 
   size=0.8, z.lim=c(20,50), colour=SNDMHT_A, shape=shape, 
   labels=SNDMHT_A, return.list=TRUE)
## Returns a list of tiles
data(eberg_contours)
tiles.l <- kml.tiles(eberg_contours, block.x=0.05, 
   colour=Z, z.lim=range(eberg_contours$Z), 
   colour_scale=SAGA_pal[[1]], return.list=TRUE)
}
}
\keyword{spatial}