\name{baranja}
\docType{data}
\encoding{latin1}
\alias{barxyz}
\alias{bargrid}
\alias{barstr}
\title{Baranja hill case study}
\description{Baranja hill is a 4 by 4 km large study area in the Baranja region, eastern Croatia (corresponds to a size of an aerial photograph). This data set has been extensively used to describe various DEM modelling and analysis steps (see \href{http://geomorphometry.org/book}{Hengl and Reuter, 2008}; \href{http://dx.doi.org/10.5194/hess-14-1153-2010}{Hengl et al., 2010}). Object \code{barxyz} contains 6370 precise observations of elevations (from field survey and digitized from the stereo images); \code{bargrid} contains \emph{observed} probabilities of streams (digitized from the 1:5000 topo map); \code{barstr} contains 100 simulated stream networks (\code{"SpatialLines"}) using \code{barxyz} point data as input (see examples below).}
\usage{data(bargrid)}
\format{
The \code{bargrid} data frame (regular grid at 30 m intervals) contains the following columns:
  \describe{
	\item{\code{p.obs}}{observed probability of stream (0-1)}
	\item{\code{x}}{a numeric vector; x-coordinate (m) in the MGI / Balkans zone 6 }
	\item{\code{y}}{a numeric vector; y-coordinate (m) in the MGI / Balkans zone 6 }
}
}
\author{ Tomislav Hengl }
\references{
\itemize{
\item Hengl, T., Reuter, H.I. (eds), (2008) \href{http://geomorphometry.org/book}{Geomorphometry: Concepts, Software, Applications}. Developments in Soil Science, vol. 33, Elsevier, 772 p.
\item Hengl, T., Heuvelink, G. B. M., van Loon, E. E., (2010) \href{http://dx.doi.org/10.5194/hess-14-1153-2010}{On the uncertainty of stream networks derived from elevation data: the error propagation approach}. Hydrology and Earth System Sciences, 14:1153-1165.
\item \url{http://geomorphometry.org/content/baranja-hill}
}
}
\note{Consider using the 30 m resolution grid (see \code{bargrid}) as the target resolution (output maps). 
}
\examples{
library(sp)
library(gstat)
## sampled elevations:
data(barxyz)
prj = "+proj=tmerc +lat_0=0 +lon_0=18 +k=0.9999 +x_0=6500000 +y_0=0 +ellps=bessel +units=m 
+towgs84=550.499,164.116,475.142,5.80967,2.07902,-11.62386,0.99999445824"
coordinates(barxyz) <- ~x+y
proj4string(barxyz) <- CRS(prj)
## grids:
data(bargrid)
data(barstr)
coordinates(bargrid) <- ~x+y
gridded(bargrid) <- TRUE
proj4string(bargrid) <- barxyz@proj4string
bargrid@grid
\dontrun{## Example with simulated streams:
data(R_pal)
library(rgdal)
library(RSAGA)
pnt = list("sp.points", barxyz, col="black", pch="+")
spplot(bargrid[1], sp.layout=pnt, 
  col.regions = R_pal[["blue_grey_red"]])
## Deriving stream networks using geostatistical simulations:
Z.ovgm <- vgm(psill=1831, model="Mat", range=1051, nugget=0, kappa=1.2)
sel <- runif(length(barxyz$Z))<.2
N.sim <- 5
## geostatistical simulations:
DEM.sim <- krige(Z~1, barxyz[sel,], bargrid, model=Z.ovgm, nmax=20, 
   nsim=N.sim, debug.level=-1)
## Note: this operation can be time consuming

stream.list <- list(rep(NA, N.sim))
## derive stream networks in SAGA GIS:
for (i in 1:N.sim) {
  writeGDAL(DEM.sim[i], paste("DEM", i, ".sdat", sep=""), 
     drivername = "SAGA", mvFlag = -99999)
  ## filter the spurious sinks:
  rsaga.fill.sinks(in.dem=paste("DEM", i, ".sgrd", sep=""), 
     out.dem="DEMflt.sgrd", check.module.exists = FALSE)
  ## extract the channel network SAGA GIS:
  rsaga.geoprocessor(lib="ta_channels", module=0, 
    param=list(ELEVATION="DEMflt.sgrd", 
    CHNLNTWRK=paste("channels", i, ".sgrd", sep=""), 
    CHNLROUTE="channel_route.sgrd", 
    SHAPES="channels.shp", 
    INIT_GRID="DEMflt.sgrd", 
    DIV_CELLS=3, MINLEN=40),
    check.module.exists = FALSE, 
    show.output.on.console=FALSE)
  stream.list[[i]] <- readOGR("channels.shp", "channels", 
    verbose=FALSE)
  proj4string(stream.list[[i]]) <- barxyz@proj4string
}
# plot all derived streams at top of each other:
streams.plot <- as.list(rep(NA, N.sim))
for(i in 1:N.sim){
  streams.plot[[i]] <- list("sp.lines", stream.list[[i]])
}
spplot(DEM.sim[1], col.regions=grey(seq(0.4,1,0.025)), scales=list(draw=T), 
sp.layout=streams.plot)
}
}
\keyword{datasets}
