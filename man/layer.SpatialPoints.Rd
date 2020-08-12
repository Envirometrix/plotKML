\name{kml_layer.SpatialPoints}
\alias{kml_layer.SpatialPoints}
\alias{SpatialPoints}
\title{Writes spatial points to KML}
\description{Writes object of class \code{"SpatialPoints*"} to KML with a possibility to parse attribute variables using several aesthetics arguments. }
\usage{
kml_layer.SpatialPoints(obj, subfolder.name = paste(class(obj)), 
        extrude = TRUE, z.scale = 1, 
        LabelScale = get("LabelScale", envir = plotKML.opts), 
        metadata = NULL, html.table = NULL, TimeSpan.begin = "",
        TimeSpan.end = "", points_names, \dots)
}
\arguments{
  \item{obj}{object of class \code{"SpatialPoints*"}}
  \item{subfolder.name}{character; optional subfolder name}
  \item{extrude}{logical; specifies whether to connect the point to the ground with a line}
  \item{z.scale}{numeric; exaggeration in vertical dimension}
  \item{LabelScale}{numeric; scale factor for size of labels}
  \item{metadata}{(optional) specify the metadata object}
  \item{html.table}{(optional) specify the description block (html) for each point}
  \item{TimeSpan.begin}{(optional) beginning of the referent time period}
  \item{TimeSpan.end}{(optional) end of the referent time period}
  \item{points_names}{character; forces the point labels (size of the character vector must equal the number of the points)}
  \item{\dots}{additional style arguments (see \code{\link{aesthetics}})}
}
\details{\code{TimeSpan.begin} and \code{TimeSpan.end} are optional TimeStamp vectors:\cr 

\code{yyyy-mm-ddThh:mm:sszzzzzz}\cr

For observations at point support (a single moment in time), use the same time values for both \code{TimeSpan.begin} and \code{TimeSpan.end}. \code{TimeSpan.begin} and \code{TimeSpan.end} can be either a single value or a vector of values. \cr
Optional aesthetics arguments are \code{shapes} (icons), \code{colour}, \code{sizes}, \code{altitude} (if not a 3D object; variable to be used to specify altitude above ground), \code{altitudeMode} (altitude mode type (\code{clampToGround}, \code{relativeToGround} or \code{absolute}). Although this function can be used to plot over five variables, more than three aesthetics arguments is not recommended (e.g. limit to size and colour).
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette }
\seealso{ \code{\link{kml_layer.STTDF}}, \code{\link{plotKML-method}} }
\examples{
data(eberg)
data(SAGA_pal)
library(sp)
library(rgdal)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
names(eberg)
# subset to 10 percent:
eberg <- eberg[runif(nrow(eberg))<.1,]
\dontrun{# plot the measured CLAY content:
kml(eberg, labels = CLYMHT_A)
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
# color only:
kml(eberg, shape = shape, colour = SLTMHT_A, labels = "", colour_scale = SAGA_pal[[1]])
# two variables at the same time:
kml(eberg, shape = shape, size = CLYMHT_A, colour = SLTMHT_A, labels = "")
# two aesthetics elements are effective in emphasizing hot-spots:
kml(eberg, shape = shape, altitude = CLYMHT_A*10, extrude = TRUE, 
  colour = CLYMHT_A, labels = CLYMHT_A, kmz = TRUE)
}

## example of how plotKML is programmed:
data(HRtemp08)
HRtemp08[1,]
library(XML)
p1 = newXMLNode("Placemark")
begin <- format(HRtemp08[1,"DATE"]-.5, "\%Y-\%m-\%dT\%H:\%M:\%SZ")
end <- format(HRtemp08[1,"DATE"]+.5, "\%Y-\%m-\%dT\%H:\%M:\%SZ")
txt <- sprintf('<name>\%s</name><TimeStamp><begin>\%s</begin><end>\%s</end></TimeStamp>
    <Point><coordinates>\%.4f,\%.4f,\%.0f</coordinates></Point>', HRtemp08[1,"NAME"], 
    begin, end, HRtemp08[1,"Lon"], HRtemp08[1,"Lat"], 0)
parseXMLAndAdd(txt, parent=p1)
p1
}
\keyword{spatial}

