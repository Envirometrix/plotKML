\name{vect2rast.SpatialPoints}
\alias{vect2rast.SpatialPoints}
\title{Converts points to rasters}
\description{Converts object of class \code{"SpatialPoints*"} to a raster map, and (optional) writes it to an external file (GDAL-supported formats; it used the SAGA GIS format by default).}
\usage{
vect2rast.SpatialPoints(obj, fname = names(obj)[1], cell.size, bbox, 
      file.name, silent = FALSE, method = c("raster", "SAGA")[1], FIELD = 0, 
      MULTIPLE = 1, LINE_TYPE = 0, GRID_TYPE = 2, \dots )
}
\arguments{
  \item{obj}{\code{"SpatialPoints*"} object}
  \item{fname}{target variable name in the \code{"data"} slot}
  \item{cell.size}{(optional) grid cell size in the output raster map}
  \item{bbox}{(optional) output bounding box (class \code{"bbox"}) for cropping the data}
  \item{file.name}{(optional) file name to export the resulting raster map}
  \item{silent}{logical; specifies whether to print any output of processing}
  \item{method}{character; specifies the gridding method}
  \item{FIELD}{character; SAGA GIS argument attribute table field number}
  \item{MULTIPLE}{character; SAGA GIS argument method for multiple values --- [0] first, [1] last, [2] minimum, [3] maximum, [4] mean}
  \item{LINE_TYPE}{character; SAGA GIS argument method for rasterization --- [0] thin, [1] thick}  
  \item{GRID_TYPE}{character; SAGA GIS argument for coding type --- [0] integer (1 byte), [1] integer (2 byte), [2] integer (4 byte), [3] floating point (4 byte), [4] floating point (8 byte)}  
  \item{\dots}{additional arguments that can be passed to the \code{raster::rasterize} command}
}
\value{Returns an object of type \code{"SpatialGridDataFrame"}.}
\author{Tomislav Hengl }
\seealso{\code{\link{vect2rast}}}
\examples{
\dontrun{
library(sp)
data(meuse)
coordinates(meuse) <- ~x+y
# point map:
x <- vect2rast(meuse, fname = "om")
data(SAGA_pal)
sp.p <- list("sp.points", meuse, pch="+", cex=1.5, col="black")
spplot(x, col.regions=SAGA_pal[[1]], sp.layout=sp.p)
}
}
\keyword{spatial}
