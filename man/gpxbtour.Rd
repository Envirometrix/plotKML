\name{gpxbtour}
\docType{data}
\encoding{latin1}
\alias{gpxbtour}
\alias{trajectory}
\title{GPS log of a bike tour}
\description{GPS log of a bike tour from Wageningen (the Netherlands) to \enc{Münster}{Muenster} (Germany). The table contains 3228 records of GPS locations, speed and elevation.}
\format{
  The data frame contains the following columns:
  \describe{
	\item{\code{lon}}{longitude (x-coordinate)}
	\item{\code{lat}}{latitude (y-coordinate)}
	\item{\code{ele}}{GPS-estimated elevation in m}
	\item{\code{speed}}{GPS-estimated speed in km per hour}
	\item{\code{time}}{XML Schema time}
} 
} 
\usage{data(gpxbtour)}
\author{ Tomislav Hengl }
\note{ The log was produced using the GlobalSat GH-615 GPS watch. The original data log (trackpoints) was first saved to GPX exchange format (\url{http://www.topografix.com/gpx.asp}) and then imported to R using the XML package and formatted to a data frame.
}
\examples{
\dontrun{## load the data:
data(gpxbtour)
library(sp)
## format the time column:
gpxbtour$ctime <- as.POSIXct(gpxbtour$time, format="\%Y-\%m-\%dT\%H:\%M:\%SZ")
coordinates(gpxbtour) <- ~lon+lat
proj4string(gpxbtour) <- CRS("+proj=longlat +datum=WGS84")
## convert to a STTDF class:
library(spacetime)
library(adehabitatLT)
gpx.ltraj <- as.ltraj(coordinates(gpxbtour), gpxbtour$ctime, id = "th")
gpx.st <- as(gpx.ltraj, "STTDF")
## Google maps plot:
library(RgoogleMaps)
llc <- c(mean(gpx.st@sp@bbox[2,]), mean(gpx.st@sp@bbox[1,]))
MyMap <- GetMap.bbox(center=llc, zoom=8, destfile="map.png")
PlotOnStaticMap(MyMap, lat=gpx.st@sp@coords[,2], lon=gpx.st@sp@coords[,1], 
  FUN=lines, col="black", lwd=4)
}
}
\keyword{datasets}
