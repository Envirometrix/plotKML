\name{readKML.GBIFdensity}
\alias{readKML.GBIFdensity}
\title{Imports GBIF cell density records}
\description{Read GBIF cell (1--degree) density record counts and converts them to a \code{"raster"} object.}
\usage{readKML.GBIFdensity(kml.file, gbif.url = FALSE, silent = FALSE)
}
\arguments{
  \item{kml.file}{GBIF cell density file (local file or URL)}
  \item{gbif.url}{logical; species whether the cellid and taxon content information should be also imported (usually not used)}
  \item{silent}{logical; species whether the progress bar should be printed}
}
\details{This document contains data shared through the GBIF Network --- see \url{http://www.gbif.org/occurrence} for more information. GBIF records are constantly updated and every map derived refers to a certain date indicated in the \code{@zname} \emph{Last update} slot.\cr 
All usage of these data must be in accordance with the GBIF Data Use Agreement: \url{https://www.gbif.org/terms}.}
\references{
\itemize{
\item GBIF cell density description (\url{http://www.gbif.org/occurrence})
}
}
\author{Tomislav Hengl }
\seealso{\code{\link{readGPX}}}
\examples{
\dontrun{# reading taxon density maps:
kml.file <- "taxon-celldensity-2294100.kml"
# download.file(paste("http://data.gbif.org/occurrences/taxon/celldensity/", kml.file, sep=""),
# destfile=paste(getwd(), kml.file, sep="")) 
# this will not run (you must first accept the data usage agreeent); 
# instead, obtain the kml file via a web browser, and save it to the working directory:
r <- readKML.GBIFdensity(kml.file)
class(r)
summary(r)
image(r)
# add world borders:
library(maps)
country.m = map('world', plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(country.m$names, ":"), function(x) x[1])
library(maptools)
country <- as(map2SpatialPolygons(country.m, IDs=IDs), "SpatialLines")
lines(country)
# to import a list of files, use e.g.:
kml.list <- list(kml.file)
r.lst <- lapply(kml.list, readKML.GBIFdensity, silent = TRUE)
# mask out missing layers (empty KML files):
mask <- !sapply(r.lst, is.null)
r.lst <- brick(r.lst[mask])
}
}
\keyword{spatial}
