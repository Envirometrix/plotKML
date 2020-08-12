\name{sp.palette-class}
\docType{class}
\alias{sp.palette-class}
\title{A class for color palette}
\description{A class for color palette that can be further used to create an object of class \code{"SpatialMetadata"}.}
\section{Slots}{
  \describe{    
    \item{\code{bounds}:}{object of class "numeric" or "character"; class boundaries}
    \item{\code{color}:}{object of class "character"; contains HEX colors}
    \item{\code{icons}:}{object of class "character"; (optional) contains symbols or URI to icons}
    \item{\code{names}:}{object of class "character"; class names (optional)}
    \item{\code{type}:}{object of class "character"; variable type}
  }
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{spMetadata}}, \code{\link{metadata2SLD-methods}} }
\note{Size of class boundaries (upper and lower) is 1 element larger then the size of colors and element names.}
\keyword{classes}