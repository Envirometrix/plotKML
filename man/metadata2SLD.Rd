\name{metadata2SLD-methods}
\docType{methods}
\alias{metadata2SLD-methods}
\alias{metadata2SLD}
\alias{metadata2SLD.Spatial}
\alias{metadata2SLD,SpatialMetadata-method}
\title{Methods to create a Styled Layer Description (\bold{SLD}) file}
\description{Creates a \href{http://docs.geoserver.org/stable/en/user/styling/}{Styled Layer Description} (\bold{SLD}) file, that can be attached to a spatial layer contributed to GeoServer. It writes the \code{"sp.pallete"} object (legend entries, titles and colors) to an external file.}
\usage{
\S4method{metadata2SLD}{SpatialMetadata}(obj, \dots)
}
\arguments{
  \item{obj}{object of class \code{"SpatialMetadata"}}
  \item{\dots}{other arguments}
}
\details{The structure of the SLD file is determined by the object class (Point, Polygon, SpatialPixels). }
\author{ Tomislav Hengl }
\seealso{ \code{\link{metadata2SLD.SpatialPixels}}, \code{\link{spMetadata}}
}
\keyword{spatial}
\keyword{methods}