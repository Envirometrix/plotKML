\name{kml_layer.SpatialPhotoOverlay}
\alias{kml_layer.SpatialPhotoOverlay}
\title{Exports objects of type SpatialPhotoOverlay to KML}
\description{Writes object of type SpatialPhotoOverlay to KML together with a COLLADA 3D model file (optional).}
\usage{
kml_layer.SpatialPhotoOverlay(obj, method = c("PhotoOverlay", "monolith")[1],
       PhotoOverlay.shape = obj@PhotoOverlay$shape, href = obj@filename, 
       coords, dae.name = "", heading = obj@PhotoOverlay$heading, 
       tilt = obj@PhotoOverlay$tilt, roll = obj@PhotoOverlay$roll, 
       near = obj@PhotoOverlay$near, range =  obj@PhotoOverlay$range, 
       leftFov = obj@PhotoOverlay$leftFov, rightFov = obj@PhotoOverlay$rightFov, 
       bottomFov = obj@PhotoOverlay$bottomFov, topFov = obj@PhotoOverlay$topFov, 
       altitudeMode = "clampToGround", block.size = 100, max.depth = 300, 
       scale.x = 1, scale.y = 1, scale.z = 1, refreshMode = "once", 
       html.table = NULL, \dots )
}
\arguments{
  \item{obj}{object of class \code{"SpatialPhotoOverlay"} (a photograph with spatial coordinates, metadata and orientation)}
  \item{method}{visualization type: either \code{"PhotoOverlay"} or \code{"monolith"}}
  \item{PhotoOverlay.shape}{PhotoOverlay shape value (KML)}
  \item{href}{location of the image file}
  \item{coords}{(optional) 3D coordinates of the trapesoid corners}
  \item{dae.name}{(optional) COLLADA 3D model file name (without the extension)}
  \item{heading}{a PhotoOverlay argument; direction (azimuth) of the camera, in degrees}
  \item{tilt}{a PhotoOverlay argument; rotation, in degrees, of the camera around the X axis}
  \item{roll}{a PhotoOverlay argument; rotation, in degrees, of the camera around the Z axis}  
  \item{near}{a PhotoOverlay argument; measurement in meters along the viewing direction from the camera viewpoint to the PhotoOverlay shape}  
  \item{range}{a PhotoOverlay argument; distance in meters from the point specified by <longitude>, <latitude>, and <altitude> to the LookAt position}  
  \item{leftFov}{a PhotoOverlay argument; angle, in degrees, between the camera's viewing direction and the left side of the view volume}
  \item{rightFov}{a PhotoOverlay argument; angle, in degrees, between the camera's viewing direction and the right side of the view volume}
  \item{bottomFov}{a PhotoOverlay argument; angle, in degrees, between the camera's viewing direction and the bottom side of the view volume}
  \item{topFov}{a PhotoOverlay argument; angle, in degrees, between the camera's viewing direction and the top side of the view volume}
  \item{altitudeMode}{altitude mode}
  \item{block.size}{width of the block (100 m by default)}
  \item{max.depth}{300 m by default}
  \item{scale.x}{exaggeration in X dimension (COLLADA rectangle)}
  \item{scale.y}{exaggeration in Y dimension (COLLADA rectangle)}
  \item{scale.z}{exaggeration in Z dimension (COLLADA rectangle)}
  \item{refreshMode}{refresh mode for the COLLADA object}
  \item{html.table}{(optional) specify the description block (html) for each point}
  \item{\dots}{other additional arguments}
}
\details{The default widht and height (100 m and 300 m) were selected based on empirical testing (level of detail in the background imagery in Google Earth). User specified coordinates can be passed via the \code{cords} argument. For more info see \code{\link{makeCOLLADA.rectangle}}.
}
\references{
\itemize{
\item KML Reference (\url{http://code.google.com/apis/kml/documentation/kmlreference.html})
\item COLLADA Reference (\url{https://www.khronos.org/collada/})
}
}
\author{Tomislav Hengl }
\seealso{ \code{\link{spPhoto}}, \code{\link{getWikiMedia.ImageInfo}} }
\examples{
\dontrun{# display spatially referenced photograph in Google Earth:
imagename = "Soil_monolith.jpg"
x1 <- getWikiMedia.ImageInfo(imagename)
sm <- spPhoto(filename = x1$url$url, exif.info = x1$metadata)
kml_open("sm.kml")
kml_layer(sm, method="monolith")
kml_close("sm.kml")
kml_compress("sm.kml", files="Soil_monolith_jpg.dae")
}
}
\keyword{spatial}