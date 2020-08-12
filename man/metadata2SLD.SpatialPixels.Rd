\name{metadata2SLD.SpatialPixels}
\alias{metadata2SLD.SpatialPixels}
\title{Writes a Styled Layer Description (\bold{SLD}) file}
\description{Writes a \href{http://docs.geoserver.org/stable/en/user/styling/}{Styled Layer Description} (\bold{SLD}) file, that can be attached to a spatial layer contributed to GeoServer. }
\usage{
metadata2SLD.SpatialPixels(obj, 
    Format_Information_Content = xmlValue(obj@xml[["//formcont"]]),
    obj.name = normalizeFilename(deparse(substitute(obj))), 
    sld.file = set.file.extension(obj.name, ".sld"),
    Citation_title = xmlValue(obj@xml[["//title"]]), 
    ColorMap_type = "intervals", opacity = 1, 
    brw.trg = 'Greys', target.var, \dots)
}
\arguments{
  \item{obj}{object of class \code{"SpatialMetadata"}}
  \item{Format_Information_Content}{character; class of the object to be written to SLD file}
  \item{obj.name}{character; name of the layer}
  \item{sld.file}{character; name of the output file}
  \item{Citation_title}{character; title of the layer}
  \item{ColorMap_type}{character; type of the colorMap see \url{http://docs.geoserver.org}}
  \item{opacity}{logical; specifies the opacity}
  \item{brw.trg}{character; color scheme according to www.colorbrewer2.org; default to 'Greys'}
  \item{target.var}{character; target variable used to calculate the class-intervals}
  \item{\dots}{additional arguments}
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{spMetadata}}
}
\examples{
\dontrun{# generate missing metadata
data(eberg_grid)
library(sp)
coordinates(eberg_grid) <- ~x+y
gridded(eberg_grid) <- TRUE
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
# with localy prepared metadata file:
eberg_TWI <- as(eberg_grid["TWISRT6"], "SpatialPixelsDataFrame")
eberg.md <- spMetadata(eberg_TWI, Target_variable="TWISRT6")
# export to SLD format:
metadata2SLD(eberg.md, "eberg_TWI.sld")
}
}
\keyword{spatial}
