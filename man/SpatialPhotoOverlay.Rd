\name{SpatialPhotoOverlay-class}
\docType{class}
\alias{SpatialPhotoOverlay-class}
\alias{spPhoto,SpatialPhotoOverlay}
\title{A class for Spatial PhotoOverlay}
\description{A class for spatial photographs (spatially and geometrically defined) that can be plotted in Google Earth.}
\section{Slots}{   
  \describe{
    \item{\code{filename}}{object of class "character"; URI of the filename location (typically a URL)}
    \item{\code{pixmap}}{object of class "pixmapRGB"; RGB bands of a bitmapped images}    
    \item{\code{exif.info}}{object of class "list"; \href{http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/EXIF.html}{EXIF} photo metadata}
    \item{\code{PhotoOverlay}}{object of class "list"; list of the camera geometry parameters (KML specifications)}
    \item{\code{sp}}{object of class "SpatialPoints"; location of the camera}
  }
}
\section{Extends}{
Class \code{"pixmapRGB"}.
}
\section{Methods}{
  \describe{
	\item{summary}{\code{signature(obj = "SpatialMetadata")}: summarize object}
  }
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{plotKML-method}}, \code{\link{spPhoto}} }
\keyword{classes}