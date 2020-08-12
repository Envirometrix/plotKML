\name{makeCOLLADA}
\alias{makeCOLLADA}
\alias{makeCOLLADA.rectangle}
\title{Generate a COLLADA file representing the 3D model of a rectangle}
\description{Produces a COLLADA file representing the 3D model of a rectangle with the image specifies via \code{href} wrapped over the surface (as texture fill). This allows free rotation of any rectangular image in the 3D space.
}
\usage{
makeCOLLADA.rectangle(coords, filename, href, DateTime, 
          up_axis = "Z_UP", authoring_tool = "plotKML", 
          technique_profile = "GOOGLEEARTH", 
          double_sided = TRUE)
}
\arguments{
  \item{coords}{a matrix defining the rectangle: 4 points with X, Z and Y coordinates (P1 --- upper right, P2 --- upper left, P3 --- lower right, P4 --- lower left)}
  \item{filename}{output filename with \code{*.dae} extension}
  \item{href}{location of the image used for wrapping (texture fill)}
  \item{DateTime}{creation / update time (system time)}
  \item{up_axis}{specify which axis is errected}
  \item{authoring_tool}{specify authoring tool}
  \item{technique_profile}{specify technique profile}  
  \item{double_sided}{logical; specify whether to drape image on both sides}
}
\details{COLLADA is managed by the nonprofit technology consortium, the Khronos Group. You can also simply drag and drop a COLLADA (\code{.dae}) file on top of the virtual Earth. 
}
\references{
\itemize{
\item COLLADA Schema (\url{https://www.khronos.org/collada/})
}
}
\author{Tomislav Hengl }
\seealso{ \code{\link{kml_layer.SpatialPhotoOverlay}}}
\examples{
\dontrun{# image previously uploaded to Wikimedia commons:
imagename = "Soil_monolith.jpg"
x1 <- getWikiMedia.ImageInfo(imagename)
sm <- spPhoto(filename = x1$url$url, exif.info = x1$metadata)
kml(sm, method="monolith")
xmlTreeParse("Soil_monolith_jpg.dae")
}
}
\keyword{spatial}
