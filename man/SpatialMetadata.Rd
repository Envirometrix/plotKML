\name{SpatialMetadata-class}
\docType{class}
\alias{SpatialMetadata-class}
\alias{summary,SpatialMetadata-method}
\alias{GetPalette,SpatialMetadata-method}
\alias{GetPalette}
\alias{GetNames,SpatialMetadata-method}
\alias{GetNames}
\title{A class for spatial metadata}
\description{A class containing spatial metadata in the \href{http://www.fgdc.gov/metadata/csdgm/}{Federal Geographic Data Committee (FGDC) Content Standard for Digital Geospatial Metadata}.}
\section{Slots}{
  \describe{
    \item{\code{xml}:}{object of class "XMLInternalDocument"; a metadata slot}
    \item{\code{field.names}:}{object of class "character"; corresponding metadata column names}
    \item{\code{palette}:}{object of class "sp.palette"; contains legend names and colors}
    \item{\code{sp}:}{object of class "Spatial"; bounding box and projection system of the input object}
  }
}
\section{Methods}{
  \describe{
	\item{summary}{\code{signature(obj = "SpatialMetadata")}: summarize object}
	\item{GetPalette}{\code{signature(obj = "SpatialMetadata")}: get only the color slot}
	\item{GetNames}{\code{signature(obj = "SpatialMetadata")}: get metadata field names}
  }
}
\author{ Tomislav Hengl and Michael Blaschek }
\seealso{ \code{\link{spMetadata}}, \code{\link{metadata2SLD-methods}} }
\examples{
\dontrun{
data(eberg)
library(sp)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
names(eberg)
# add metadata:
eberg.md <- spMetadata(eberg, xml.file=system.file("eberg.xml", package="plotKML"), 
    Target_variable="SNDMHT_A")
p <- GetPalette(eberg.md)
str(p)
x <- summary(eberg.md)
str(x)
}
}
\keyword{classes}
