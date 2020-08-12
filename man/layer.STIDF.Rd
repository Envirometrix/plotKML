\name{kml_layer.STIDF}
\encoding{latin1}
\alias{kml_layer.STIDF}
\alias{kml_layer.STFDF}
\alias{STIDF}
\title{Write irregular spatio-temporal observations (points, lines and polygons) to KML}
\description{Writes an object of class \code{"STIDF"} (unstructured/irregular spatio-temporal data) to a KML file with a possibility to parse attribute variables using several aesthetics arguments.
}
\usage{
kml_layer.STIDF(obj, dtime, \dots)
}
\arguments{
  \item{obj}{space-time object of class \code{"STIDF"} (spatio-temporal irregular data frame) or class \code{"STFDF"} (spatio-temporal full data frame)}
  \item{dtime}{temporal support (point or block) expressed in seconds}
  \item{\dots}{additional arguments that can be passed to the \code{kml_layer.Spatial} method}
}
\details{An object of class \code{"STIDF"} contains a slot of type \code{"Spatial*"}, which is parsed via the \code{kml_layer} method depending on the type of spatial object (points, lines, polygons). The dateTime is defined as:\cr 

\code{yyyy-mm-ddThh:mm:sszzzzzz}\cr

where \code{T} is the separator between the date and the time, and the time zone is either \code{Z} (for UTC) or \code{zzzzzz}, which represents \enc{±}{+-}hh:mm in relation to UTC. For more info on how Time Stamps work see \url{https://developers.google.com/kml/documentation/kml_tut}. If the time is measured at block support, then:\cr 

\code{<TimeStamp><begin> </begin><end> </end></TimeStamp>}\cr

tags will be inserted. Temporal support for any spacetime class, if not specified by the user, is determined as a difference between the \code{"time"} (indicating begin time) and \code{"endTime"} slots.
}
\references{
\itemize{
\item Pebesma, E. (2012) \href{http://www.jstatsoft.org/v51/i07}{Classes and Methods for Spatio-Temporal Data in R}. Journal of Statistical Software. 51(7): 1-30.
\item spacetime package (\url{https://CRAN.R-project.org/package=spacetime})
}
}
\author{Tomislav Hengl and Benedikt Graeler}
\seealso{ \code{\link{kml_layer.STTDF}}, \code{\link{plotKML-method}} }
\examples{
\dontrun{
data(HRtemp08)

# format the time column:
HRtemp08$ctime <- as.POSIXct(HRtemp08$DATE, format="\%Y-\%m-\%dT\%H:\%M:\%SZ")

# create a STIDF object:
library(spacetime)
sp <- SpatialPoints(HRtemp08[,c("Lon","Lat")])
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
HRtemp08.st <- STIDF(sp, time = HRtemp08$ctime, data = HRtemp08[,c("NAME","TEMP")])

# write to a KML file:
HRtemp08_jan <- HRtemp08.st[1:500]
shape <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
kml(HRtemp08_jan, dtime = 24*3600, colour = TEMP, shape = shape, labels = "", kmz=TRUE)

## North Carolina SIDS data set:
library(maptools)
fname <- system.file("shapes/sids.shp", package="maptools")[1]
nc <- readShapePoly(fname, proj4string=CRS("+proj=longlat +datum=NAD27"))
time <- as.POSIXct(strptime(c(rep("1974-01-01", length(nc)), 
   rep("1979-01-01", length(nc))), format="\%Y-\%m-\%d"), tz = "GMT")
data <- data.frame(BIR = c(nc$BIR74, nc$BIR79), NWBIR = c(nc$NWBIR74, nc$NWBIR79), 
   SID = c(nc$SID74, nc$SID79))
# copy polygons:
nc.poly <- rep(slot(nc, "polygons"), 2)
# fix the polygon IDs:
for(i in 1:length(row.names(data))) { nc.poly[[i]]@ID = row.names(data)[i] }
sp <- SpatialPolygons(nc.poly, proj4string=CRS("+proj=longlat +datum=NAD27"))
# create a STIDF object:
nct <- STIDF(sp, time = time, data = data)
# write to a KML file:
kml(nct, colour = SID)
}
}
\keyword{spatial}
