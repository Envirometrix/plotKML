\name{spPhoto}
\alias{spPhoto}
\alias{SpatialPhotoOverlay}
\title{Generate an object of class \code{"SpatialPhotoOverlay"}}
\description{
\code{spPhoto} function can be used to wrap pixel map (\code{pixmapRGB}), \href{http://en.wikipedia.org/wiki/Exchangeable_image_file_format}{EXIF} (Exchangeable Image File format) data, spatial location information (standing point), and \code{PhotoOverlay} (geometry) parameters to create an object of class \code{"SpatialPhotoOverlay"}. This object can then be parsed to KML and visualized using Google Earth.
}
\usage{spPhoto(filename, obj, pixmap, exif.info = NULL, ImageWidth = 0, 
   ImageHeight = 0, bands = rep(rep(1, ImageHeight*ImageWidth), 3), 
   bbox = c(0, 0, 3/36000*ImageWidth, 3/36000*ImageHeight),
   DateTime = "", ExposureTime = "", FocalLength = "50 mm", 
   Flash = "No Flash", rotation = 0, leftFov = -30, rightFov = 30, 
   bottomFov = -30, topFov = 30, near = 50, 
   shape = c("rectangle", "cylinder", "sphere")[1], range = 1000, tilt = 90, 
   heading = 0, roll = 0, test.filename = TRUE)
}
\arguments{
  \item{filename}{file name with extension (ideally an URL)}
  \item{obj}{object of class "SpatialPoints" (requires a single point object)}
  \item{pixmap}{object of class "pixmapRGB" (see package pixmap)}
  \item{exif.info}{named list containing all available \href{http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/EXIF.html}{EXIF} metadata}
  \item{ImageWidth}{(optional) image width in pixels}
  \item{ImageHeight}{(optional) image height in pixels}
  \item{bands}{(optional) RGB bands as vectors (see \code{pixmap::pixmapRGB})}
  \item{bbox}{(optional) bounding box coordinates (by default 1 pixel is about 1 m in arc degrees)}  
  \item{DateTime}{(optional) usually available from the camera EXIF data}
  \item{ExposureTime}{(optional) usually available from the camera EXIF data}
  \item{FocalLength}{(optional) usually available from the camera EXIF data}
  \item{Flash}{(optional) usually available from the camera EXIF data}
  \item{rotation}{(optional) rotation angle in 0--90 degrees}
  \item{leftFov}{(optional) angle, in degrees, between the camera's viewing direction and the left side of the view volume (-180 -- 0)}
  \item{rightFov}{(optional) angle, in degrees, between the camera's viewing direction and the right side of the view volume (0 -- 180)}
  \item{bottomFov}{(optional) angle, in degrees, between the camera's viewing direction and the bottom side of the view volume (-90 -- 0)}
  \item{topFov}{(optional) angle, in degrees, between the camera's viewing direction and the top side of the view volume (0 -- 90)}
  \item{near}{(optional) measurement in meters along the viewing direction from the camera viewpoint to the PhotoOverlay shape}
  \item{shape}{(optional) shape type --- rectangle (standard photograph), cylinder (for panoramas), or sphere (for spherical panoramas)}
  \item{range}{(optional) distance from the camera to the placemark}
  \item{tilt}{(optional) rotation, in degrees, of the camera around the X axis}
  \item{heading}{(optional) direction (azimuth) of the camera, in degrees (0 -- 360)}
  \item{roll}{(optional) rotation about the y axis, in degrees (0 -- 180)}
  \item{test.filename}{logical; species whether a test should be first performed that the file name really exists (recommended)}
}
\value{ Returns an object of class \code{"SpatialPhotoOverlay"}:
\item{filename}{URL location of the original image}
\item{pixmap}{optional; local copy of the image (\code{"pixmapRGB"} class)}
\item{exif.info}{list of EXIF metadata}
\item{PhotoOverlay}{list of the camera geometry parameters (KML specifications)}
\item{sp}{location of the camera (\code{"SpatialPoints"} class)}
}
\details{The most effective way to import a field photograph to \code{SpatialPhotoOverlay} for parsing to KML is to: (\strong{a}) use the \href{http://www.sno.phy.queensu.ca/~phil/exiftool/}{EXIF tool} (courtesy of Phil Harvey) to add any important tags in the image file, (\strong{b}) once you've added all important tags, you can upload your image either to a local installation of Mediawiki or to a public portal such as the \href{http://commons.wikimedia.org/wiki/Special:UploadWizard}{Wikimedia Commons}, (\strong{c}) enter the missing information if necessary and add an image description. Once the image is on the server, you only need to record its unique name and then read all metadata from the Wikimedia server following the examples below.\cr 
You can also consider importing images to R by using the \code{pixmap} package, and reading the technical information via e.g. the \href{https://cran.r-project.org/package=exif}{\code{exif}} package. If the image is taken using a GPS enabled camera, by getting the EXIF metadata you can generate the complete \code{SpatialPhotoOverlay} object with minimum user interaction. Otherwise, you need to at least specify: creation date, file name, and location of the focal point of the camera (e.g. by creating \code{"SpatialPoints"} object).
}
\note{The \code{spPhoto} function will try to automatically fix the aspect ratio of the \code{ViewVolume} settings (\code{leftFov}, \code{rightFov}, \code{bottomFov}, \code{topFov}), and based on the original aspect ratio as specified in the EXIF data. This might not work for all images, in which case you will have to manually adjust those parameters.\cr
Dimension of \code{3/36000*ImageWidth} in decimal degrees is about 10 m in nature (3-arc seconds is about 100 m, depending on the latitude). 
}
\references{
\itemize{
\item EXIF tool (\url{http://www.sno.phy.queensu.ca/~phil/exiftool/})
\item Wikimedia API (\url{http://www.mediawiki.org/wiki/API})
}
}
\author{Tomislav Hengl}
\seealso{ \code{\link{getWikiMedia.ImageInfo}}, \code{pixmap::pixmapRGB}, \code{\link{spMetadata}}}
\examples{
\dontrun{# two examples with images on Wikimedia Commons
# (1) soil monolith (manually entered coordinates):
imagename = "Soil_monolith.jpg"
# import EXIF data using the Wikimedia API:
x1 <- getWikiMedia.ImageInfo(imagename)
# create a SpatialPhotoOverlay:
sm <- spPhoto(filename = x1$url$url, exif.info = x1$metadata)
# plot it in Google Earth
kml(sm, method="monolith", kmz=TRUE)
# (2) PhotoOverlay (geotagged photo):
imagename = "Africa_Museum_Nijmegen.jpg"
x2 <- getWikiMedia.ImageInfo(imagename)
af <- spPhoto(filename = x2$url$url, exif.info = x2$metadata)
kml(af)
}
}
\keyword{spatial}