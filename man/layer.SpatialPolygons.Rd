\name{kml_layer.SpatialPolygons}
\alias{kml_layer.SpatialPolygons}
\alias{SpatialPolygons}
\title{Writes spatial polygons to KML}
\description{Writes object of class \code{"SpatialPolygons*"} to KML with a possibility to parse attribute variables using several aesthetics arguments. }
\usage{
kml_layer.SpatialPolygons(obj, subfolder.name = paste(class(obj)),
        extrude = TRUE, tessellate = FALSE, 
        outline = TRUE, plot.labpt = FALSE, z.scale = 1, 
        LabelScale = get("LabelScale", envir = plotKML.opts), 
        metadata =  NULL, html.table = NULL, TimeSpan.begin = "", 
        TimeSpan.end = "", colorMode = "normal", \dots)
}
\arguments{
  \item{obj}{object of class \code{"SpatialPolygons*"}}
  \item{subfolder.name}{character; optional subfolder name}
  \item{extrude}{logical; specifies whether to connect the point to the ground with a line}
  \item{tessellate}{logical; specifies whether to connect the LinearRing to the ground}
  \item{outline}{logical; specifies whether to outline the polygon}
  \item{plot.labpt}{logical; specifies whether to add the label point (polygon centre)}
  \item{z.scale}{numeric; exaggeration in vertical dimension}
  \item{LabelScale}{numeric; scale factor for size of labels}
  \item{metadata}{(optional) specify the metadata object}  
  \item{html.table}{optional description block (html) for each GPS point (vertices)}
  \item{TimeSpan.begin}{(optional) beginning of the referent time period}
  \item{TimeSpan.end}{(optional) end of the referent time period}
  \item{colorMode}{(optional) KML color mode (normal or random)}
  \item{\dots}{additional style arguments (see \code{aesthetics})}
}
\details{Label points are be default not plotted. We recommend adding the legend to attribute maps instead. Transparency can be set by using the alpha argument.\cr 
\code{TimeSpan.begin} and \code{TimeSpan.end} are optional TimeStamp vectors:\cr
 
\code{yyyy-mm-ddThh:mm:sszzzzzz}\cr 

Use the same time values for both \code{TimeSpan.begin} and \code{TimeSpan.end} if the measurements refer to a single moment in time. \code{TimeSpan.begin} and \code{TimeSpan.end} can be either a single value or a vector of values.}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\seealso{ \code{\link{kml_layer.SpatialLines}}, \code{\link{kml_layer.STIDF}}, , \code{\link{plotKML-method}} }
\examples{
library(rgdal)
library(sp)
data(eberg_zones)
names(eberg_zones)
## visualize zones using random colors:
kml.file = paste0(tempdir(), "/eberg_zones.kml")
kml(eberg_zones, file.name=kml.file, colorMode = "random")
## with labels:
kml.file2 = paste0(tempdir(), "/eberg_zones2.kml")
kml(eberg_zones, file.name=kml.file2, colour = ZONES, 
    plot.labpt = TRUE, labels = ZONES, kmz = TRUE, balloon=TRUE)
}
\keyword{spatial}
