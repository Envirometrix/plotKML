\name{getCRS-methods}
\docType{methods}
\alias{getCRS}
\alias{getCRS.Raster}
\alias{getCRS.Spatial}
\alias{getCRS,Raster-method}
\alias{getCRS,Spatial-method}
\title{Methods to get the proj4 string}
\description{Gets the proj4 string from a object of type "Spatial" or "Raster".}
\usage{
\S4method{getCRS}{Spatial}(obj)
\S4method{getCRS}{Raster}(obj) 
}
\arguments{
  \item{obj}{object of type \code{"Spatial"} or \code{"Raster"}}
}
\details{For more details about the PROJ.4 parameters refer to the \url{https://proj4.org}.}
\author{ Tomislav Hengl and Pierre Roudier}
\seealso{ \code{sp::CRS}, \code{raster::raster}, \code{\link{check_projection}}
}
\examples{
data(eberg_grid)
library(sp)
coordinates(eberg_grid) <- ~x+y
gridded(eberg_grid) <- TRUE
library(rgdal)
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
library(raster)
r <- raster(eberg_grid[1])
getCRS(r)
r.ll <- reproject(r)
getCRS(r.ll)
}
\keyword{spatial}
\keyword{methods}
