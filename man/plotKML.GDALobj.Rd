\name{plotKML.GDALobj}
\alias{plotKML.GDALobj}
\title{Write tiled objects to KML}
\description{Write tiled objects to KML. Suitable for plotting large rasters i.e. large spatial data sets.}
\usage{
plotKML.GDALobj(obj, file.name, block.x, tiles=NULL, 
   tiles.sel=NULL, altitude=0, altitudeMode="relativeToGround", colour_scale, 
   z.lim=NULL, breaks.lst=NULL, kml.logo, overwrite=TRUE, cpus, 
   home.url=".", desc=NULL, open.kml=TRUE, CRS=attr(obj, "projection"), 
   plot.legend=TRUE)
}
\arguments{
  \item{obj}{\code{"GDALobj"} object i.e. a pointer to a spatial layer}
  \item{file.name}{character; output KML file name}
  \item{block.x}{numeric; size of block in meters or corresponding mapping units}
  \item{tiles}{data.frame; tiling definition generated using \code{landmap::tile}}
  \item{tiles.sel}{integer; selection of tiles to be plotted}
  \item{altitude}{numeric; altitude of the ground overlay}
  \item{altitudeMode}{character; either \code{"absolute"}, \code{"relativeToGround"} or \code{"clampToGround"}}
  \item{colour_scale}{character; color palette}
  \item{z.lim}{numeric; upper lower boundaries}  
  \item{breaks.lst}{numeric; optional break lines (must be of size \code{length(colour_scale)+1)}}
  \item{kml.logo}{character; optional project logo file (PNG)}
  \item{overwrite}{logical; specifies whether to overwrite PNGs if available}
  \item{cpus}{integer; specifies number of CPUs to be used by the snowfall package to speed things up}
  \item{home.url}{character; optional web-directory where the PNGs will be stored}
  \item{desc}{character; optional layer description}
  \item{open.kml}{logical; specifies whether to open the KML file after writing}
  \item{CRS}{character; projection string (if missing)}
  \item{plot.legend}{logical; indicate whether to plot summary legend}
}
\value{Returns a list of KML files.}
\note{This operation can be time-consuming for processing very large rasters e.g. more than 10,000 by 10,000 pixels. To speed up writing of KMLs, use the snowfall package.}
\author{Tomislav Hengl }
\seealso{\code{\link{plotKML}}, \code{\link{kml.tiles}}}
\examples{
\dontrun{
library(sp)
library(snowfall)
library(landmap)
library(rgdal)
fn = system.file("pictures/SP27GTIF.TIF", 
 package = "rgdal")
obj <- GDALinfo(fn)
tiles <- getSpatialTiles(obj, block.x=5000, 
  return.SpatialPolygons = FALSE)
## plot using tiles:
plotKML.GDALobj(obj, tiles=tiles, z.lim=c(0,185))
## Even better ideas is to first reproject 
## the large grid using 'gdalUtils::gdalwarp', then tile...
}
}
\keyword{spatial}