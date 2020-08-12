\name{reproject}
\docType{methods}
\alias{reproject}
\alias{reproject.RasterBrick}
\alias{reproject.RasterLayer}
\alias{reproject.RasterStack}
\alias{reproject.SpatialGrid}
\alias{reproject.SpatialPoints}
\alias{reproject,SpatialLines-method}
\alias{reproject,SpatialPoints-method}
\alias{reproject,SpatialPixelsDataFrame-method}
\alias{reproject,SpatialPixelsDataFrame-method}
\alias{reproject,SpatialPolygons-method}
\alias{reproject,SpatialGridDataFrame-method}
\alias{reproject,RasterStack-method}
\alias{reproject,RasterLayer-method}
\alias{reproject,RasterBrick-method}
\title{Methods to reproject maps to a referent coordinate system (WGS84)}
\description{This wrapper function reprojects any vector or raster spatial data to some referent coordinate system (by default: geographic coordinates on the \href{http://spatialreference.org/ref/epsg/4326/}{World Geodetic System of 1984 / WGS84} datum).}
\usage{
\S4method{reproject}{SpatialPoints}(obj, CRS, \dots)
\S4method{reproject}{SpatialPolygons}(obj, CRS, \dots)
\S4method{reproject}{SpatialLines}(obj, CRS, \dots)
\S4method{reproject}{RasterLayer}(obj, CRS, program = "raster", tmp.file = TRUE, 
      NAflag, show.output.on.console = FALSE, method, \dots)
\S4method{reproject}{SpatialGridDataFrame}(obj, CRS, tmp.file = TRUE, program = "raster", 
      NAflag, show.output.on.console = FALSE, \dots)
\S4method{reproject}{SpatialPixelsDataFrame}(obj, CRS, tmp.file = TRUE, program = "raster", 
      NAflag, show.output.on.console = FALSE, \dots)
\S4method{reproject}{RasterBrick}(obj, CRS)
\S4method{reproject}{RasterStack}(obj, CRS)
}
\arguments{
  \item{obj}{Spatial* or Raster* object}
  \item{CRS}{object of class \code{"CRS"}; proj4 string}
  \item{program}{reprojection engine; either raster package or GDAL}
  \item{tmp.file}{logical; specifies whether to create a temporary file or not}
  \item{NAflag}{character; missing value flag}
  \item{show.output.on.console}{logical; specifies whether to print the progress}
  \item{method}{character; resampling method e.g.\code{"bilinear"}}
  \item{\dots}{arguments evaluated in the context of function \code{projectRaster} from the raster package}
}
\details{
In the case of raster and/or gridded maps, by selecting \code{program = "GDAL"} \code{gdalwarp} functionality will be initiated (otherwise it tries to reproject via the package raster). This requires that \href{http://gdal.org}{GDAL} are installed and located from R via \code{paths()}.
}
\references{
\itemize{
\item Raster package (\url{https://CRAN.R-project.org/package=raster})
\item GDAL (\url{http://GDAL.org})
}
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\section{Warning}{\code{obj} needs to have a proper proj4 string (CRS), otherwise \code{reproject} will not run.}
\seealso{\link{paths}, \link[raster]{projectRaster}, \link[sp]{spTransform}, \link[sp]{CRS-class}}
\examples{
## example with vector data:
data(eberg)
library(sp)
library(rgdal)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
eberg.geo <- reproject(eberg)
\dontrun{## example with raster data:
data(eberg_grid25)
gridded(eberg_grid25) <- ~x+y
proj4string(eberg_grid25) <- CRS("+init=epsg:31467")
## reproject to geographical coords (can take few minutes!):
eberg_grid_ll <- reproject(eberg_grid25[1])
## much faster when using GDAL:
eberg_grid_ll2 <- reproject(eberg_grid25[1], program = "GDAL")
## optional: compare processing times:
system.time(eberg_grid_ll <- reproject(eberg_grid25[1]))
system.time(eberg_grid_ll2 <- reproject(eberg_grid25[1], program="GDAL"))
}
}
\keyword{spatial}
