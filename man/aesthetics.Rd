\name{aesthetics}
\alias{aesthetics}
\alias{kml_aes}
\alias{kml_width}
\title{Plotting aesthetics parameters}
\description{Parses various object parameters / columns to KML aesthetics: size of the icons, fill color, labels, altitude, width, \dots}
\usage{
kml_aes(obj, \dots)
}
\arguments{
  \item{obj}{space-time object for plotting}
  \item{\dots}{other arguments}
}
\author{Pierre Roudier}
\details{Valid aesthetics: \code{colour = "black"}, \code{fill = "white"}, \code{shape}, \code{whitening}, \code{alpha}, \code{width = 1}, \code{labels}, \code{altitude = 0}, \code{size}, \code{balloon = FALSE}. Specific features (target variables and the connected hot-spots) can be emphasized by using two or three graphical parameters for the same variable. See plotKML package homepage / vignette for more examples.}
\seealso{ \code{\link{kml-methods}} }
\keyword{spatial}

