\name{grid2poly}
\alias{grid2poly}
\title{Converts a gridded map to a polygon map}
\description{Converts a \code{"SpatialGridDataFrame"} object to a polygon map with each available grid node represented with a polygon. To allow further export to KML, \code{grid2poly} will, by default, convert any projected coordinates to the lat-lon system (geographic coordinates in the WGS84 system).}
\usage{
grid2poly(obj, var.name = names(obj)[1], reproject = TRUE, 
    method = c("sp", "raster", "RSAGA")[1], tmp.file = TRUE, 
    saga_lib = "shapes_grid", saga_module = 3, silent = FALSE, \dots)
}
\arguments{
  \item{obj}{\code{"SpatialGridDataFrame"} object}
  \item{var.name}{target variable column name}
  \item{reproject}{logical; reproject coordinates to lat lon system?}
  \item{method}{decide to convert grids to polygons either using \code{"sp"}, \code{"raster"} or \code{"RSAGA"} packages}
  \item{tmp.file}{logical; specify whether to create a temporary file, or to actually write to the workding directory (in the case of SAGA GIS is used to convert grids)}
  \item{saga_lib}{string; SAGA GIS library name}
  \item{saga_module}{SAGA GIS module number; see \code{?rsaga_get_modules} for more details}
  \item{silent}{logical; specifies whether to print the SAGA GIS output}
  \item{\dots}{additional arguments that can be parsed to the \code{rasterToPolygons} command}
}
\details{\code{grid2poly} is not recommended for large grids (>>10e4 pixels). Consider splitting large input grids into tiles before running \code{grid2poly}. For converting large grids to polygons consider using SAGA GIS (\code{method = "RSAGA"}) instead of using the default sp method.}
\author{Tomislav Hengl }
\seealso{\code{\link{vect2rast}}, \code{raster::rasterToPolygons}}
\examples{
data(eberg_grid)
library(sp)
coordinates(eberg_grid) <- ~x+y
gridded(eberg_grid) <- TRUE
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
data(SAGA_pal)
\dontrun{# compare various methods:
system.time(dem_poly <- grid2poly(eberg_grid, "DEMSRT6", method = "raster"))
system.time(dem_poly <- grid2poly(eberg_grid, "DEMSRT6", method = "sp"))
system.time(dem_poly <- grid2poly(eberg_grid, "DEMSRT6", method = "RSAGA"))
## plotting large polygons in R -> not a good idea
# spplot(dem_poly, col.regions = SAGA_pal[[1]])
## visualize the data in Google Earth:
kml(dem_poly, colour_scale = SAGA_pal[[1]], colour = DEMSRT6, kmz = TRUE)
}
}
\keyword{spatial}
