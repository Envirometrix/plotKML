\name{kml_compress}
\alias{kml_compress}
\title{Compress a KML file with auxiliary files}
\description{Compresses the KML file toghether with the auxiliary files (images, models, textures) using the default ZIP program.}
\usage{
kml_compress(file.name, zip = "", files = "", rm = FALSE, \dots)
}
\arguments{
  \item{file.name}{KML file name}
  \item{zip}{(optional) location of an external ZIP program}
  \item{files}{a character vector specifying the list of auxiliary files}
  \item{rm}{logical; specify whether to remove temporary files}
  \item{\dots}{other \code{kml} arguments}
}
\references{
\itemize{
\item KMZ description (\url{http://code.google.com/apis/kml/documentation/})
}
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\details{The KMZ file can carry the model files (\code{.dae}), textures and ground overlay images. For practical purposes, we recommend that you, instead of compressing the images together with the KML file, consider serving the ground overlay images via a server i.e. as network links. \cr
If no internal ZIP program exists, the function looks for the system ZIP program:\cr 

\code{Sys.getenv("R_ZIPCMD", "zip")}\cr

External ZIP program can also be specified via the \code{zip} argument.}
\seealso{ \code{\link{kml-methods}}, \code{\link{kml_open}} }
\examples{
data(eberg)
eberg <- eberg[runif(nrow(eberg))<.1,] 
library(sp)
library(rgdal)
library(raster)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
kml.file = paste0(tempdir(), "/eberg.kml")
kml_open(kml.file)
kml_layer(eberg, colour = CLYMHT_A)
kml_close(kml.file)
# compress:
kml_compress(kml.file)
}
\keyword{utilities}

