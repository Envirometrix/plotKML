\name{check_projection}
\alias{check_projection}
\alias{parse_proj4}
\alias{extractProjValue}
\title{Extracts the proj4 parameters and checks if the projection matches the referent CRS}
\description{Function \code{parse_proj4} gets the proj4 string from a space-time object and \code{check_projection} checks if the input projection is compatible with the referent projection system. The referent system is by default the \code{longlat} projection with WGS84 datum (KML-compatible coordinates).}
\usage{check_projection(obj, control = TRUE, 
                 ref_CRS = get("ref_CRS", envir = plotKML.opts))
}
\arguments{
  \item{obj}{object of class \code{Spatial*} or \code{Raster*}}
  \item{control}{logical; if TRUE, a logical value is returned, if FALSE, an error is thrown if the test failed}
  \item{ref_CRS}{the referent coordinate system.}
}
\details{
A cartographic projection is KML compatible if: (a) geographical coordinates are used, and (b) if they relate to the WGS84 ellispoid (\code{"+proj=longlat +datum=WGS84"}). You can also set your own local referent projection system by specifiying \code{plotKML.env(ref_CRS = ...)}.
}
\references{
\itemize{
\item WGS84 (\url{http://spatialreference.org/ref/epsg/4326/})
}
}
\author{Pierre Roudier, Tomislav Hengl, and Dylan Beaudette}
\section{Warning}{\code{obj} needs to have a proper proj4 string (CRS), otherwise \code{check_projection} will not run. If the geodetic datum is defined via the \code{+towgs}, consider converting the coordinates manually i.e. by using the \code{spTransform} or \code{\link{reproject}} method.}
\seealso{\code{\link{reproject}}, \code{rgdal::CRS-class}}
\examples{
data(eberg)
library(sp)
library(rgdal)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
check_projection(eberg)
# not yet ready for export to KML;
parse_proj4(proj4string(eberg))
eberg.geo <- reproject(eberg)
check_projection(eberg.geo)
# ... now ready for export
}
\keyword{spatial}
