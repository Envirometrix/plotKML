\name{kml_layer.SpatialLines}
\alias{kml_layer.SpatialLines}
\alias{SpatialLines}
\title{Writes spatial lines to KML}
\description{Writes object of class \code{"SpatialLines*"} to KML with a possibility to parse attribute variables using several aesthetics arguments. }
\usage{
kml_layer.SpatialLines(obj, subfolder.name = paste(class(obj)), 
  extrude = FALSE, z.scale = 1, metadata = NULL, 
  html.table = NULL, TimeSpan.begin = "", TimeSpan.end = "", \dots)
}
\arguments{
  \item{obj}{object of class \code{"SpatialLines*"}}
  \item{subfolder.name}{character; optional subfolder name}
  \item{extrude}{logical; specifies whether to connect the LinearRing to the ground}
  \item{z.scale}{vertical exaggeration}  
  \item{metadata}{(optional) specify the metadata object}
  \item{html.table}{optional description block (html) for each GPS point (vertices)}
  \item{TimeSpan.begin}{(optional) beginning of the referent time period}
  \item{TimeSpan.end}{(optional) end of the referent time period}
  \item{\dots}{additional style arguments (see \code{\link{aesthetics}})}
}
\details{Only colour and width (aesthetics) are recommended when visualizing \code{SpatialLines*} objects.\cr
\code{TimeSpan.begin} and \code{TimeSpan.end} are optional TimeStamp vectors in the format:\cr 
\code{yyyy-mm-ddThh:mm:sszzzzzz} \cr
Use the same time values for both \code{TimeSpan.begin} and \code{TimeSpan.end} if the measurements refer to a single moment in time. \code{TimeSpan.begin} and \code{TimeSpan.end} can be either a single value or a vector of values.
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\seealso{ \code{\link{kml-methods}}, \code{\link{kml_open}}, \code{\link{kml_layer.SpatialPolygons}}, \code{\link{plotKML-method}} }
\examples{
library(rgdal)
library(sp)
data(eberg_contours)
data(SAGA_pal)
names(eberg_contours)
# KML plot with elevations used as 'colour' argument:
kml.file = paste0(tempdir(), "/eberg_contours.kml")
kml(eberg_contours, file.name = kml.file, 
  colour_scale = SAGA_pal[[1]], colour = Z)
}
\keyword{spatial}

