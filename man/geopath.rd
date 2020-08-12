\name{geopath}
\alias{geopath}
\title{Geopath --- shortest trajectory line between two geographic locations}
\description{Derives a SpatialLines class object showing the shortest path between the two geographic locations and based on the Haversine Formula for Great Circle distance.}
\usage{
geopath(lon1, lon2, lat1, lat2, ID, n.points, print.geo = FALSE)
}
\arguments{
  \item{lon1}{longitude coordinate of the first point}
  \item{lon2}{longitude coordinate of the second point}
  \item{lat1}{latitude coordinate of the first point}
  \item{lat2}{latitude coordinate of the second point}
  \item{ID}{(optional) point ID character}
  \item{n.points}{number of intermediate points}
  \item{print.geo}{prints the distance and bearing}
}
\details{Number of points between the start and end point is derived using a simple formula:\cr
 
\code{round(sqrt(distc)/sqrt(2), 0)}\cr 

where \code{distc} is the Great Circle Distance.}
\value{Bearing is expressed in degrees from north. Distance is expressed in kilometers (Great Circle Distance).}
\references{
\itemize{
\item fossil package (\url{https://CRAN.R-project.org/package=fossil})
\item Haversine formula from Math Forums (\url{http://mathforum.org/dr.math/})
}
}
\author{ Tomislav Hengl}
\seealso{ \code{\link{kml_layer.SpatialLines}}, \code{\link{kml_layer.STTDF}}, \code{fossil::earth.bear}}
\examples{
library(fossil)
ams.ny <- geopath(lon1=4.892222, lon2=-74.005973, 
          lat1=52.373056, lat2=40.714353, print.geo=TRUE)
kml.file = paste0(tempdir(), "/ams.ny.kml")
# write to a file:
kml(ams.ny, file.name=kml.file)
}
\keyword{spatial}
