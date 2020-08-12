\name{plotKML.env}
\alias{plotKML.env}
\alias{paths}
\alias{plotKML.opts}
\title{plotKML specific environmental variables / paths}
\description{Sets the environmental, package specific parameters and settings (URLs, names, default color palettes and similar) that can be later on passed to other functions.}
\usage{plotKML.env(colour_scale_numeric = "", colour_scale_factor = "",
        colour_scale_svar = "", ref_CRS, NAflag, icon, LabelScale, size_range, 
        license_url, metadata_sel, kmz, kml_xsd, kml_url, kml_gx, gpx_xsd,
        fgdc_xsd, inspire_xsd, convert, gdalwarp, gdal_translate, python,
        home_url, show.env = TRUE, silent = TRUE)
}
\arguments{
  \item{colour_scale_numeric}{default colour scheme for numeric variables}
  \item{colour_scale_factor}{default colour scheme for factor variables}
  \item{colour_scale_svar}{default colour scheme for model error (e.g. mapping error)}
  \item{ref_CRS}{the referent CRS \href{http://spatialreference.org/ref/epsg/4326/}{proj4string} (\code{"+proj=longlat +datum=WGS84"})}
  \item{NAflag}{the default missing value flag (usually \code{"-99999"})}
  \item{icon}{the default icon URL}
  \item{LabelScale}{the default scale factor for labels}
  \item{size_range}{the default size range}
  \item{license_url}{the default license URL}
  \item{metadata_sel}{a list of the default metadata fields for summary}
  \item{kmz}{logical; the default compression setting}
  \item{kml_xsd}{the default KML scheme URL}
  \item{kml_url}{the default KML format URL}
  \item{kml_gx}{the default extended KML scheme URL}
  \item{gpx_xsd}{the default GPX scheme URL}
  \item{fgdc_xsd}{the default metadata scheme URL}
  \item{inspire_xsd}{the default metadata scheme URL}
  \item{convert}{a path to ImageMagick convert program}
  \item{gdalwarp}{a path to gdalwarp program}
  \item{gdal_translate}{a path to gdalwarp program}
  \item{python}{a path to Python program}
  \item{home_url}{the default location of all icons and auxiliary files}
  \item{show.env}{logical; specify whether to print all environmental parameters}
  \item{silent}{logical; specify whether to search for paths for external software}
}
\details{ The function will try to locate external software tools under either Windows or Unix platform and then save the results to the \code{plotKML.opts} environment. \link{plotKML-package} does not look automatically for software paths (unless you specify this manually in your \code{"Rprofile.site"}). \cr 
The external software tools are not required by default and most of operations in \link{plotKML-package} can be run without using them. GDAL, SAGA GIS and Python are highly recommended, however, for processing large data sets. The function \code{paths} looks for GDAL, ImageMagick, Python, SAGA GIS, in the Windows Registry Hive, the Program Files directory or the usr/bin installation (Unix).
}
\references{
\itemize{
\item ImageMagick (\url{http://imagemagick.org})
\item GDAL (\url{http://gdal.org})
\item SAGA GIS (\url{http://www.saga-gis.org})
\item Python (\url{http://python.org})
}
}
\author{Tomislav Hengl, Dylan Beaudette}
\note{To further customize the plotKML options, consider putting:\cr 
\code{library(plotKML); plotKML.env(..., show.env = FALSE)}\cr 
in your \code{"/etc/Rprofile.site"}.
}
\section{Warning}{Under Linux OS you need to install GDAL binaries by using e.g.:\cr 
\code{sudo apt-get install gdal-bin}
}
\examples{
\dontrun{## look for paths:
library(gdalUtils)
pts <- paths()
pts
plotKML.env(silent = FALSE)
gdalwarp <- get("gdalwarp", envir = plotKML.opts)
## if missing you need to install it!
system(paste(gdalwarp, "--help-general")) 
system(paste(gdalwarp, "--formats"), intern = TRUE)
}
plotKML.env(show.env = FALSE)
get("home_url", envir = plotKML.opts)
}
\keyword{spatial}
