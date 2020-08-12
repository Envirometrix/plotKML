\name{getWikiMedia.ImageInfo}
\alias{getWikiMedia.ImageInfo}
\title{Gets EXIF information}
\description{
\code{getWikiMedia.ImageInfo} function fetches the EXIF (Exchangeable image file format) data via the \href{http://www.mediawiki.org/wiki/API}{Wikimedia API} for any donated image. The resulting EXIF data (named list) can then be further used to construct an object of class \code{"SpatialPhotoOverlay"}, which can be parsed to KML.
}
\usage{getWikiMedia.ImageInfo(imagename, 
     APIsource = "https://commons.wikimedia.org/w/api.php", 
     module = "imageinfo", 
     details = c("url", "metadata", "size", "extlinks"), testURL = TRUE)
}
\arguments{
  \item{imagename}{Wikimedia commons unique image title}
  \item{APIsource}{location of the API service}
  \item{module}{default module}
  \item{details}{detailed parameters of interest}
  \item{testURL}{logical; species if the program should first test whether the image exist at all (recommended)}
}
\details{Although this is often not visible in picture editing programs, almost any image uploaded to Wikimedia contains usefull EXIF metadata. However, it is highly recommended that you insert the some important tags in the image header yourself, by using e.g. the \href{http://www.sno.phy.queensu.ca/~phil/exiftool/}{EXIF tool} (courtesy of Phil Harvey), before uploading the files to Wikimedia. The \code{getWikiMedia.ImageInfo} function assumes that all required metadata has already been entered by the user before the upload, hence no further changes in the metadata will be possible. Examples of how to embed EXIF tags into an image file are available \href{http://commons.wikimedia.org/wiki/Commons:EXIF}{here}.\cr 
To geocode an uploaded image consider adding:\cr 

\code{{{location|lat deg|lat min|lat sec|NS|long deg|long min|long sec|EW}}} \cr

tag to the file description, in which case \code{getWikiMedia.ImageInfo} will automatically look for the attached coordinates via the external links. For practical purposes and because the image properties information determined by the Wikimedia system can are more reliable, the function will rewrite some important EXIF metadata (image width and height) using the actual values determined by Wikimedia server.\cr 
For a list of modules and parameters that can be used via \code{getWikiMedia.ImageInfo}, please refer to \href{http://www.mediawiki.org/wiki/API}{Wikimedia API manual}.
}
\references{
\itemize{
\item Wikimedia API (\url{http://www.mediawiki.org/wiki/API})
\item EXIF tool (\url{http://www.sno.phy.queensu.ca/~phil/exiftool/})
\item EXIF Tags (\url{http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/EXIF.html})
}
}
\author{Tomislav Hengl }
\seealso{\code{\link{spPhoto}}, \code{Rexif::getExifPy}}
\examples{
\dontrun{# Photo taken using a GPS-enabled camera:
imagename = "Africa_Museum_Nijmegen.jpg"
x <- getWikiMedia.ImageInfo(imagename)
# Get the GPS info:
x$metadata[grep(names(x$metadata), pattern="GPS")]
# prints the complete list of metadata tags;
}
}