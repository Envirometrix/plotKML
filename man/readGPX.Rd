\name{readGPX}
\alias{readGPX}
\title{Import GPX (GPS track) files}
\description{Reads various elements from a \code{*.gpx} file --- metadata, waypoints, tracks and routes --- and converts them to dataframes.}
\usage{readGPX(gpx.file, metadata = TRUE, bounds = TRUE, 
              waypoints = TRUE, tracks = TRUE, routes = TRUE)
}
\arguments{
  \item{gpx.file}{location of the gpx.file}
  \item{metadata}{logical; species whether the metadata should be imported}
  \item{bounds}{logical; species whether the bounding box coordinates should be imported}
  \item{waypoints}{logical; species whether all waypoints should be imported}
  \item{tracks}{logical; species whether all tracks should be imported}
  \item{routes}{logical; species whether all routes should be imported}
}
\details{\bold{Waypoint} is a point of interest, or named feature on a map. \bold{Track} is an ordered list of points describing a path. \bold{Route} is an ordered list of waypoints representing a series of turn points leading to a destination.
}
\references{
\itemize{
\item GPX data format (\url{http://www.topografix.com/gpx.asp})
\item XML tutorial (\url{https://github.com/omegahat/XML})
}
}
\author{Tomislav Hengl }
\seealso{\code{rgdal::readOGR}, \code{\link{kml_layer.STTDF}}}
\examples{
\dontrun{# read GPX file from web:
fells_loop <- readGPX("http://www.topografix.com/fells_loop.gpx")
str(fells_loop)
}
}
\keyword{spatial}