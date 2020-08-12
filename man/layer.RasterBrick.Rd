\name{kml_layer.RasterBrick}
\alias{kml_layer.RasterBrick}
\alias{RasterBrick}
\title{Export a time series of images to KML}
\description{Writes a series of images to PNGs and uses them to create ground overlays. Works only with \code{"RasterBrick"} class objects with time dimension specified via the \code{"@zvalue"}.}
\usage{
kml_layer.RasterBrick(obj, plot.legend = TRUE, dtime = "", tz = "GMT", 
          z.lim = c(min(minValue(obj), na.rm=TRUE), max(maxValue(obj), na.rm=TRUE)), 
          colour_scale = get("colour_scale_numeric", envir = plotKML.opts),
          home_url = get("home_url", envir = plotKML.opts),
          metadata = NULL, html.table = NULL,
          altitudeMode = "clampToGround", balloon = FALSE,
          png.width, png.height, min.png.width = 800, png.type, \dots)
}
\arguments{
  \item{obj}{object of class \code{"RasterBrick"} (e.g. a time series of images)}
  \item{plot.legend}{logical; specify whether a map legend should be generated automatically}
  \item{dtime}{temporal support (point or block) expressed in seconds}
  \item{tz}{referent time zone}
  \item{z.lim}{upper and lower limits (unique for all maps in the time series); the function by default uses the absolute minimum and maximum in values}
  \item{colour_scale}{color palette; by default uses the color scale for numeric variables}
  \item{home_url}{(optional) URL directory / location of the images}
  \item{metadata}{(optional) the metadata object}
  \item{html.table}{(optional) the description block (html)}
  \item{altitudeMode}{character; the default altitudeMode}
  \item{balloon}{logical; specifies whether to display balloon for each element}
  \item{png.width}{(optional) width of the PNG files}
  \item{png.height}{(optional) height of the PNG files}
  \item{min.png.width}{(optional) minimum width of the PNG file}
  \item{png.type}{character; PNG type}
  \item{\dots}{additional arguments (see \code{aesthetics})}
}
\details{This method is recommended for visualization of numeric bands representing the same variable i.e. time series of images. To export a stack of images of different type see \code{\link{kml_layer.Raster}}. If the \code{"@zvalue"} slot is empty, dates will be added by subtracting days from the current day with 1--day increments.
}
\author{Tomislav Hengl }
\seealso{ \code{\link{kml-methods}}, \code{\link{kml_open}}, \code{\link{kml_layer.Raster}}, \code{\link{plotKML-method}} }
\keyword{spatial}


