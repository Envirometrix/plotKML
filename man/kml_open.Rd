\name{kml_open}
\alias{kml_open}
\alias{kml_close}
\alias{kml_View}
\title{Open / close a KML file connection}
\description{Opens a KML file in write mode and initiates the KML header. The same file connection is further accessible by other \code{kml_*()} functions such as \code{kml_layer()} and \code{kml_close()}. \code{kml_View} tries to open the produced file using the default application.}
\usage{
kml_open(file.name, folder.name = file.name, kml_open = TRUE,
          kml_visibility = TRUE, overwrite = TRUE, use.Google_gx = FALSE, 
          kml_xsd = get("kml_xsd", envir = plotKML.opts),
          xmlns = get("kml_url", envir = plotKML.opts), 
          xmlns_gx = get("kml_gx", envir = plotKML.opts))
}
\arguments{
  \item{file.name}{KML file name}
  \item{folder.name}{character string; KML folder name}
  \item{kml_open}{logical; specify whether to open the folder by default}
  \item{kml_visibility}{logical; specify whether to make the whole folder visible}
  \item{overwrite}{logical; if TRUE, "name" will be overwritten if it exists}
  \item{use.Google_gx}{logical; specify whether to use the Google's extended schema}
  \item{kml_xsd}{URL of the KML scheme to be used}
  \item{xmlns}{URL of the OGC KML standard}
  \item{xmlns_gx}{URL of the extended standard}
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\details{These lower level functions can be used to create customized multi-layered KML files. See plotKML package homepage / manual for more examples.}
\seealso{ \code{\link{plotKML-method}}, \code{\link{kml_layer}}, \code{\link{kml-methods}} }
\keyword{spatial}