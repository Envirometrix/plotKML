\name{HRprec08}
\docType{data}
\alias{HRprec08}
\title{Daily precipitation for Croatia for year 2008}
\description{The daily measurements of precipitation (rain gauges) for year 2008 kindly contributed by the \href{http://meteo.hr}{Croatian National Meteorological Service}. \code{HRprec08} contains 175,059 measurements of precipitation sums (489 stations by 365 days). 
}
\usage{data(HRprec08)}
\format{
The \code{HRprec08} data frames contain the following columns:
  \describe{
	\item{\code{NAME}}{name of the meteorological station}
	\item{\code{Lon}}{a numeric vector; x-coordiante / longitude in the WGS84 system}
	\item{\code{Lat}}{a numeric vector; y-coordinate / latitude in the WGS84 system}
	\item{\code{DATE}}{'Date' class vector}
	\item{\code{PREC}}{daily cummulative precipitation in mm (precipitation from the day before)}
  }
}
\author{ Tomislav Hengl and Melita Percec Tadic }
\references{
\itemize{
\item Testik, F.Y. and Gebremichael, M. Eds (2011) \href{https://dx.doi.org/10.1029/GM191}{Rainfall: State of the Science}. Geophysical monograph series, Vol. 191, 287 p.
\item Zaninovic K., Gajic-Capka, M., Percec Tadic, M. et al., (2010) \href{http://klima.hr/razno.php?id=publikacije&param=atlas}{Klimatski atlas Hrvatske / Climate atlas of Croatia 1961-1990., 1971-2000}. Zagreb, Croatian National Meteorological Service, 200 p.
\item AGGM book datasets (\url{http://spatial-analyst.net/book/HRclim2008})
}
}
\note{ The precipitation estimates in mm (\code{HRprec08}) are collected in a bottle within the rain gauge and readings are usually manual by an observer at 7 a.m. The precipitation collected in the morning refer to the precipitation for previous 24 hours. To project coordinates we suggest using the \href{http://spatialreference.org/ref/epsg/32633/}{UTM zone 33N} system as this coordinate system was used to prepare the \href{http://spatial-analyst.net/book/HRclim2008}{gridded predictors}.
}
\seealso{ \code{\link{HRtemp08}} }
\examples{
data(HRprec08)
library(sp)
\dontrun{# subset:
prec.2008.05.01 <- HRprec08[HRprec08$DATE=="2008-05-01",]
coordinates(prec.2008.05.01) <- ~Lon+Lat
proj4string(prec.2008.05.01) <- CRS("+proj=lonlat +datum=WGS84")
# write to KML:
shape = "http://plotkml.r-forge.r-project.org/circle.png"
data(SAGA_pal)
kml(prec.2008.05.01, size = PREC, shape = shape, colour = PREC, 
    colour_scale = SAGA_pal[[9]], labels = PREC)
}
}
\keyword{datasets}