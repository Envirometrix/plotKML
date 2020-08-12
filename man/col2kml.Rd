\name{col2kml}
\alias{col2kml}
\alias{hex2kml}
\alias{kml2hex}
\alias{munsell2kml}
\title{Convert a color strings to the KML format}
\description{Converts some common color formats (internal R colors, hexadecimal format, Munsell color codes) color to KML format.}
\usage{col2kml(colour)}
\arguments{
  \item{colour}{R color string}
}
\value{KML-formatted color as \code{\#aabbggrr} where \code{aa}=alpha (00 to ff), \code{bb}=blue (00 to ff), \code{gg}=green (00 to ff), \code{rr}=red (00 to ff).}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\seealso{
\code{aqp::munsell2rgb}
}
\examples{
col2kml("white")
col2kml(colors()[2])
hex2kml(rgb(1,1,1))
x <- munsell2kml("10YR", "2", "4")
kml2hex(x)
}
\keyword{color}

