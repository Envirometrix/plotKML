\name{kml_layer-methods}
\docType{methods}
\alias{kml_layer}
\alias{kml_alpha}
\alias{kml_altitude}
\alias{kml_altitude_mode}
\alias{kml_colour}
\alias{kml_shape}
\alias{kml_size}
\alias{kml_layer,RasterBrick-method}
\alias{kml_layer,RasterLayer-method}
\alias{kml_layer,RasterStack-method}
\alias{kml_layer,STIDF-method}
\alias{kml_layer,STFDF-method}
\alias{kml_layer,STSDF-method}
\alias{kml_layer,STTDF-method}
\alias{kml_layer,SoilProfileCollection-method}
\alias{kml_layer,SpatialGrid-method}
\alias{kml_layer,SpatialLines-method}
\alias{kml_layer,SpatialPhotoOverlay-method}
\alias{kml_layer,SpatialPixels-method}
\alias{kml_layer,SpatialPoints-method}
\alias{kml_layer,SpatialPolygons-method}
\alias{kml_layer,sfc_POINT-method}
\alias{kml_layer,sfc_MULTIPOINT-method}
\alias{kml_layer,sfc_LINESTRING-method}
\alias{kml_layer,sfc_MULTILINESTRING-method}
\alias{kml_layer,sfc_POLYGON-method}
\alias{kml_layer,sfc_MULTIPOLYGON-method}
\alias{kml_layer,sf-method}
\title{Write objects to a KML connection}
\description{
Writes any \code{Spatial*} object (from the sp package), \code{sfc_*} or \code{sf} object (from the sf package), spatio-temporal object (from the \code{\link[spacetime]{ST-class}} package) or \code{Raster*} object (from the raster package) to a KML file (connection) as a separate layer. Various \emph{aesthetics}, i.e. ways to represent target variables, can be set via colour, transparency, size, width, shape arguments. Their availability depends on the class of the object to plot.
}
\usage{
kml_layer(obj, \dots)
}
\arguments{
  \item{obj}{object inheriting from the \code{Spatial*} or the \code{Raster*} classes}
  \item{\dots}{additional aesthetics arguments; see details for each \code{kml_layer} function and the \code{kml_aes} function}
}
\value{An XML object that can be further parsed to a KML file (via an open connection).}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\seealso{
\code{\link{kml_layer.SpatialPoints}}, \code{\link{kml_layer.Raster}}, \code{\link{kml_layer.SpatialLines}}, \code{\link{kml_layer.SpatialPolygons}}, \code{\link{kml_layer.STIDF}}, \code{\link{kml_layer.STTDF}}, \code{\link{kml_layer.SoilProfileCollection}}, \code{\link{kml-methods}}, \code{\link{kml_open}}, \code{\link{kml_close}}
}
\examples{
library(rgdal)
data(eberg_grid)
library(sp)
library(raster)
gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
data(SAGA_pal)
data(R_pal)
\dontrun{# Plot two layers one after the other:
kml_open("eberg_grids.kml")
kml_layer(eberg_grid, colour=DEMSRT6, colour_scale=R_pal[["terrain_colors"]])
kml_layer(eberg_grid, colour=TWISRT6, colour_scale=SAGA_pal[[1]])
kml_close("eberg_grids.kml")
# print the result:
library(XML)
xmlRoot(xmlTreeParse("eberg_grids.kml"))[["Document"]]
}

# examples with sf
eberg_grid_sf <- sf::st_as_sf(eberg_grid)

# sfc objects
\dontrun{
kml_open("eberg_grids.kml")
kml_layer(st_geometry(eberg_grid_sf))
kml_close("eberg_grids.kml")
}

# sf objects
\dontrun{
kml_open("eberg_grid_sf.kml")
kml_layer(eberg_grid_sf, colour = DEMSRT6, colour_scale = R_pal[["terrain_colors"]])
kml_layer(eberg_grid_sf, colour = TWISRT6, colour_scale = SAGA_pal[[1]])
kml_close("eberg_grid_sf.kml")
}
}
\keyword{spatial}
\keyword{methods}
