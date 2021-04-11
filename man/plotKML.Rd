\name{plotKML-method}
\docType{methods}
\alias{plotKML-method}
\alias{plotKML}
\alias{plotKML,SpatialPointsDataFrame-method}
\alias{plotKML,SpatialLinesDataFrame-method}
\alias{plotKML,SpatialPolygonsDataFrame-method}
\alias{plotKML,SoilProfileCollection-method}
\alias{plotKML,SpatialGridDataFrame-method}
\alias{plotKML,SpatialPixelsDataFrame-method}
\alias{plotKML,RasterLayer-method}
\alias{plotKML,SpatialPhotoOverlay-method}
\alias{plotKML,STIDF-method}
\alias{plotKML,STFDF-method}
\alias{plotKML,STSDF-method}
\alias{plotKML,STTDF-method}
\alias{plotKML,SpatialPredictions-method}
\alias{plotKML,RasterBrickSimulations-method}
\alias{plotKML,SpatialVectorsSimulations-method}
\alias{plotKML,SpatialMaxEntOutput-method}
\alias{plotKML,RasterBrickTimeSeries-method}
\alias{plotKML,SpatialSamplingPattern-method}
\alias{plotKML,list-method}
\alias{plotKML,sf-method}
\alias{plotKML,stars-method}
\title{Methods for plotting results of spatial analysis in Google Earth}
\description{The method writes inputs and outputs of spatial analysis (a list of point, gridded and/or polygon data usually) to KML and opens the KML file in Google Earth (or any other default package used to view KML/KMZ files).}
\usage{
\S4method{plotKML}{sf}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))),
      file.name = paste(folder.name, ".kml", sep=""), 
      metadata = NULL, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialPointsDataFrame}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))),
      file.name = paste(folder.name, ".kml", sep=""), 
      size, colour, points_names, 
      shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
      metadata = NULL, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialLinesDataFrame}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""),
      metadata = NULL, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialPolygonsDataFrame}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      colour, plot.labpt, labels, metadata = NULL, 
      kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialPixelsDataFrame}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      colour, raster_name, metadata = NULL, kmz = FALSE, open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialGridDataFrame}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      colour, raster_name, metadata = NULL, kmz = FALSE, open.kml = TRUE, \dots)
\S4method{plotKML}{RasterLayer}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      colour, raster_name, metadata = NULL, kmz = FALSE, open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialPhotoOverlay}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      dae.name, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SoilProfileCollection}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      var.name, metadata = NULL, kmz = get("kmz", envir = plotKML.opts), 
      open.kml = TRUE, \dots)
\S4method{plotKML}{STIDF}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      colour, shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
      points_names, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{STFDF}(obj, \dots)
\S4method{plotKML}{STSDF}(obj, \dots)
\S4method{plotKML}{STTDF}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      colour, start.icon = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
      kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{RasterBrickTimeSeries}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""),
      pngwidth = 680, pngheight = 180, pngpointsize = 14, 
      kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{RasterBrickSimulations}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      obj.summary = TRUE,
      pngwidth = 680, pngheight = 200, pngpointsize = 14, 
      kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialMaxEntOutput}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), 
      html.file = obj@maxent@html, 
      iframe.width = 800, iframe.height = 800, pngwidth = 280, 
      pngheight = 280, pngpointsize = 14, colour, 
      shape = "http://plotkml.r-forge.r-project.org/icon17.png", 
      kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, 
      TimeSpan.begin = obj@TimeSpan.begin, TimeSpan.end = obj@TimeSpan.end, \dots)
\S4method{plotKML}{SpatialPredictions}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), colour, 
      grid2poly = FALSE, obj.summary = FALSE, plot.svar = FALSE, 
      pngwidth = 210, pngheight = 580, pngpointsize = 14, 
      metadata = NULL, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialSamplingPattern}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""),
      colour, kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{SpatialVectorsSimulations}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
      file.name = paste(folder.name, ".kml", sep=""), colour, 
      grid2poly = FALSE, obj.summary = TRUE, plot.svar = FALSE, 
      kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
\S4method{plotKML}{list}(obj, 
      folder.name = normalizeFilename(deparse(substitute(obj, env=parent.frame()))),
      file.name = paste(folder.name, ".kml", sep=""), 
      size = NULL, colour, points_names = "",
      shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
      plot.labpt = TRUE, labels = "", metadata = NULL, 
      kmz = get("kmz", envir = plotKML.opts), open.kml = TRUE, \dots)
}
\arguments{
  \item{obj}{input object of specific class; either some \href{https://cran.r-project.org/package=sf}{sf}, or \href{https://cran.r-project.org/package=sp}{sp}, or \href{https://cran.r-project.org/package=raster}{raster} or \href{https://cran.r-project.org/package=spacetime}{spacetime} package class object, or plotKML composite objects containing both inputs and outputs of analysis}
  \item{folder.name}{character; folder name in the KML file}
  \item{file.name}{character; output KML file name}
  \item{size}{for point objects for plotting (see \link{aesthetics})}
  \item{colour}{colour variable for plotting (see \link{aesthetics})}
  \item{points_names}{vector of characters that can be used as labels}
  \item{shape}{character; icons used for plotting (see \link{aesthetics})}
  \item{raster_name}{(optional) specify the output file name (PNG)}
  \item{var.name}{target variable name (only valid for visualization of \code{"SoilProfileCollection"} class data}
  \item{metadata}{(optional) the metadata object}
  \item{plot.labpt}{logical; specifies whether to plot centroids for polygon data}
  \item{labels}{character vector; list of labels that will attached to the centroids}
  \item{start.icon}{icon for the start position (for trajectory data)}
  \item{dae.name}{output DAE file name}
  \item{html.file}{specify the location of the html file containing report data (if the input object is of class \code{"SpatialMaxEntOutput"})}
  \item{iframe.width}{integer; width of the screen for iframe}
  \item{iframe.height}{integer; height of the screen for iframe}
  \item{TimeSpan.begin}{object of class \code{"POSIXct"}; begin of the sampling period}
  \item{TimeSpan.end}{object of class \code{"POSIXct"}; end of the sampling period}
  \item{pngwidth}{integer; width of the PNG plot (screen image)}
  \item{pngheight}{integer; height of the PNG plot (screen image)}
  \item{pngpointsize}{integer; text size in the PNG plot (screen image)}
  \item{grid2poly}{logical; specifies whether to convert gridded object to polygons}
  \item{obj.summary}{logical; specifies whether to print the object summary}
  \item{plot.svar}{logical; specifies whether to plot the model uncertainty}
  \item{kmz}{logical; specifies whether to compress the output KML file}
  \item{open.kml}{logical; specifies whether to directly open the output KML file (i.e. in Google Earth)}
  \item{\dots}{(optional) arguments passed to the lower level functions}
}
\details{This is a generic function to plot various spatial and spatio-temporal R objects that contain both inputs and outputs of spatial analysis. The resulting plots (referred to as \emph{`views'}) are expected to be cartographically complete as they should contain legends, and data and model descriptions. In principle, \code{plotKML} works with both simple spatial objects, and complex objects such as \code{"SpatialPredictions"}, \code{"SpatialVectorsSimulations"}, \code{"RasterBrickSimulations"}, \code{"RasterBrickTimeSeries"}, \code{"SpatialMaxEntOutput"} and similar. To further customize visualizations consider combining the lower level functions \code{\link{kml_open}}, \code{\link{kml_close}}, \code{\link{kml_compress}}, \code{\link{kml_screen}} into your own \code{plotKML()} method.

All ST-classes are coerced to the STIDF format and hence use the plotKML method for STIDFs.
The \code{sf} objects are processed according to the class of the geometry column. The aestethics are defined in the same way as for their {sp} counterparts. 
}
\note{To prepare a list of objects of class \code{"SpatialPointsDataFrame"}, \code{"SpatialLinesDataFrame"}, \code{"SpatialPolygonsDataFrame"}, or \code{"SpatialPixelsDataFrame"} consider using the \code{landmap::tile} function. Writting large spatial objects via plotKML can be time consuming. Please refer to the package manual for more information.
}
\seealso{ \code{\link{SpatialPredictions-class}}, \code{\link{SpatialVectorsSimulations-class}}, \code{\link{RasterBrickSimulations-class}}, \code{\link{RasterBrickTimeSeries-class}}, \code{\link{SpatialMaxEntOutput-class}}, \code{\link{SpatialSamplingPattern-class}}   
}
\examples{
plotKML.env(kmz = FALSE)
## -------------- SpatialPointsDataFrame --------- ##
library(sp)
library(rgdal)
data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
## subset to 20 percent:
eberg <- eberg[runif(nrow(eberg))<.1,]
\dontrun{## bubble type plot:
plotKML(eberg["CLYMHT_A"])
plotKML(eberg["CLYMHT_A"], colour_scale=rep("#FFFF00", 2), points_names="")
}
## plot points with a legend:
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png" 
kml.file = paste0(tempdir(), "/eberg_CLYMHT_A.kml")
leg.file = paste0(dirname(kml.file), "/kml_legend.png")
kml_open(kml.file)
kml_layer(eberg["CLYMHT_A"], colour=CLYMHT_A, z.lim=c(20,60),
          colour_scale=SAGA_pal[[1]], shape=shape, points_names="")
kml_legend.bar(x=eberg$CLYMHT_A, 
               legend.file=leg.file,
               legend.pal=SAGA_pal[[1]], z.lim=c(20,60))
kml_screen(image.file=leg.file)
kml_close(kml.file)

## ----- sf objects with sfc_POINT geometry ----- ##
eberg_sf <- sf::st_as_sf(eberg)
\dontrun{
plotKML(eberg_sf["CLYMHT_A"])
plotKML(eberg_sf["CLYMHT_A"], colour_scale = rep("#FFFF00", 2), points_names = "")
}

## -------------- SpatialLinesDataFrame --------- ##
data(eberg_contours)
\dontrun{
plotKML(eberg_contours)
## plot contour lines with actual altitudes:
plotKML(eberg_contours, colour=Z, altitude=Z)
}

## ---- sf objects with sfc_LINESTRING geometry ---- ##
eberg_contours_sf <- sf::st_as_sf(eberg_contours)
\dontrun{
plotKML(eberg_contours_sf)
plotKML(eberg_contours_sf, colour = Z, altitude = Z)
}

## -------------- SpatialPolygonsDataFrame --------- ##
data(eberg_zones)
\dontrun{
plotKML(eberg_zones["ZONES"])
## add altitude:
zmin = 230
plotKML(eberg_zones["ZONES"], altitude=zmin+runif(length(eberg_zones))*500)
}

## ------ sf objects with sfc_POLYGON geometry ------ ##
eberg_zones_sf <- sf::st_as_sf(eberg_zones)
\dontrun{
plotKML(eberg_zones_sf["ZONES"])
plotKML(eberg_zones_sf["ZONES"], altitude = zmin + runif(length(eberg_zones)) * 500)
}

## -------------- SpatialPixelsDataFrame --------- ##
library(rgdal)
library(raster)
data(eberg_grid)
gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
TWI <- reproject(eberg_grid["TWISRT6"])
data(SAGA_pal)
\dontrun{## set limits manually (increase resolution):
plotKML(TWI, colour_scale = SAGA_pal[[1]])
plotKML(TWI, z.lim=c(12,20), colour_scale = SAGA_pal[[1]])
}
## categorical data:
eberg_grid$LNCCOR6 <- as.factor(paste(eberg_grid$LNCCOR6))
levels(eberg_grid$LNCCOR6)
data(worldgrids_pal)
## attr(worldgrids_pal["corine2k"][[1]], "names")
pal = as.character(worldgrids_pal["corine2k"][[1]][c(1,11,13,14,16,17,18)])
LNCCOR6 <- reproject(eberg_grid["LNCCOR6"])
\dontrun{
plotKML(LNCCOR6, colour_scale=pal)
}

## -------------- SpatialPhotoOverlay --------- ##
\dontrun{
library(RCurl)
imagename = "Soil_monolith.jpg"
urlExists = url.exists("https://commons.wikimedia.org")
if(urlExists){
  x1 <- getWikiMedia.ImageInfo(imagename)
  sm <- spPhoto(filename = x1$url$url, exif.info = x1$metadata)
  # str(sm)
  plotKML(sm)
}
}

## -------------- SoilProfileCollection --------- ##
library(aqp)
library(plyr)
## sample profile from Nigeria:
lon = 3.90; lat = 7.50; id = "ISRIC:NG0017"; FAO1988 = "LXp" 
top = c(0, 18, 36, 65, 87, 127) 
bottom = c(18, 36, 65, 87, 127, 181)
ORCDRC = c(18.4, 4.4, 3.6, 3.6, 3.2, 1.2)
hue = c("7.5YR", "7.5YR", "2.5YR", "5YR", "5YR", "10YR")
value = c(3, 4, 5, 5, 5, 7); chroma = c(2, 4, 6, 8, 4, 3)
## prepare a SoilProfileCollection:
prof1 <- join(data.frame(id, top, bottom, ORCDRC, hue, value, chroma), 
   data.frame(id, lon, lat, FAO1988), type='inner')
prof1$soil_color <- with(prof1, munsell2rgb(hue, value, chroma))
depths(prof1) <- id ~ top + bottom
site(prof1) <- ~ lon + lat + FAO1988 
coordinates(prof1) <- ~ lon + lat
proj4string(prof1) <- CRS("+proj=longlat +datum=WGS84")
prof1
\dontrun{
plotKML(prof1, var.name="ORCDRC", color.name="soil_color")
}

## -------------- STIDF --------- ##
library(sp)
library(spacetime)
## daily temperatures for Croatia:
data(HRtemp08)
## format the time column:
HRtemp08$ctime <- as.POSIXct(HRtemp08$DATE, format="\%Y-\%m-\%dT\%H:\%M:\%SZ")
## create a STIDF object:
sp <- SpatialPoints(HRtemp08[,c("Lon","Lat")])
proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
HRtemp08.st <- STIDF(sp, time = HRtemp08$ctime, data = HRtemp08[,c("NAME","TEMP")])
## subset to first 500 records:
HRtemp08_jan <- HRtemp08.st[1:500]
str(HRtemp08_jan)
\dontrun{
plotKML(HRtemp08_jan[,,"TEMP"], LabelScale = .4)
}

## foot-and-mouth disease data:
data(fmd)
fmd0  <- data.frame(fmd)
coordinates(fmd0) <- c("X", "Y")
proj4string(fmd0) <- CRS("+init=epsg:27700")
fmd_sp <- as(fmd0, "SpatialPoints")
dates <- as.Date("2001-02-18")+fmd0$ReportedDay
library(spacetime)
fmd_ST <- STIDF(fmd_sp, dates, data.frame(ReportedDay=fmd0$ReportedDay))
data(SAGA_pal)
\dontrun{
plotKML(fmd_ST, colour_scale=SAGA_pal[[1]])
}

## -------------- STFDF --------- ##

\dontrun{
## results of krigeST:
library(gstat)
library(sp)
library(spacetime)
library(raster)
## define space-time variogram
sumMetricVgm <- vgmST("sumMetric",
                      space=vgm( 4.4, "Lin", 196.6,  3),
                      time =vgm( 2.2, "Lin",   1.1,  2),
                      joint=vgm(34.6, "Exp", 136.6, 12),
                      stAni=51.7)
## example from the gstat package:
data(air)
rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))
rr <- rural[,"2005-06-01/2005-06-03"]
rr <- as(rr,"STSDF")
x1 <- seq(from=6,to=15,by=1)
x2 <- seq(from=48,to=55,by=1)
DE_gridded <- SpatialPoints(cbind(rep(x1,length(x2)), rep(x2,each=length(x1))), 
                           proj4string=CRS(proj4string(rr@sp)))
gridded(DE_gridded) <- TRUE
DE_pred <- STF(sp=as(DE_gridded,"SpatialPoints"), time=rr@time)
DE_kriged <- krigeST(PM10~1, data=rr, newdata=DE_pred,
                     modelList=sumMetricVgm)
gridded(DE_kriged@sp) <- TRUE
#stplot(DE_kriged)
## plot in Google Earth:
z.lim = range(DE_kriged@data, na.rm=TRUE)
plotKML(DE_kriged, z.lim=z.lim)
## add observations points:
plotKML(rr, z.lim=z.lim)
}

## -------------- STTDF --------- ##
\dontrun{
library(fossil)
library(spacetime)
library(adehabitatLT)
data(gpxbtour)
## format the time column:
gpxbtour$ctime <- as.POSIXct(gpxbtour$time, format="\%Y-\%m-\%dT\%H:\%M:\%SZ")
coordinates(gpxbtour) <- ~lon+lat
proj4string(gpxbtour) <- CRS("+proj=longlat +datum=WGS84")
xy <- as.list(data.frame(t(coordinates(gpxbtour))))
gpxbtour$dist.km <- sapply(xy, function(x) { 
  deg.dist(long1=x[1], lat1=x[2], long2=xy[[1]][1], lat2=xy[[1]][2]) 
} )
## convert to a STTDF class:
gpx.ltraj <- as.ltraj(coordinates(gpxbtour), gpxbtour$ctime, id = "th")
gpx.st <- as(gpx.ltraj, "STTDF")
gpx.st$speed <- gpxbtour$speed
gpx.st@sp@proj4string <- CRS("+proj=longlat +datum=WGS84")
str(gpx.st)
plotKML(gpx.st, colour="speed")
}

## -------------- Spatial Metadata --------- ##
\dontrun{
eberg.md <- spMetadata(eberg, xml.file=system.file("eberg.xml", package="plotKML"),
  Target_variable="SNDMHT_A", Citation_title="Ebergotzen profiles")
plotKML(eberg[1:100,"CLYMHT_A"], metadata=eberg.md)
}

## -------------- RasterBrickTimeSeries --------- ##
library(raster)
library(sp)
data(LST)
gridded(LST) <- ~lon+lat
proj4string(LST) <- CRS("+proj=longlat +datum=WGS84")
dates <- sapply(strsplit(names(LST), "LST"), function(x){x[[2]]})
datesf <- format(as.Date(dates, "\%Y_\%m_\%d"), "\%Y-\%m-\%dT\%H:\%M:\%SZ")
## begin / end dates +/- 4 days:
TimeSpan.begin = as.POSIXct(unclass(as.POSIXct(datesf))-4*24*60*60, origin="1970-01-01") 
TimeSpan.end = as.POSIXct(unclass(as.POSIXct(datesf))+4*24*60*60, origin="1970-01-01")
## pick climatic stations in the area:
pnts <- HRtemp08[which(HRtemp08$NAME=="Pazin")[1],]
pnts <- rbind(pnts, HRtemp08[which(HRtemp08$NAME=="Crni Lug - NP Risnjak")[1],])
pnts <- rbind(pnts, HRtemp08[which(HRtemp08$NAME=="Cres")[1],])
coordinates(pnts) <- ~Lon + Lat
proj4string(pnts) <- CRS("+proj=longlat +datum=WGS84")
## get the dates from the file names:
LST_ll <- brick(LST[1:5])
LST_ll@title = "Time series of MODIS Land Surface Temperature images"
LST.ts <- new("RasterBrickTimeSeries", variable = "LST", sampled = pnts, 
    rasters = LST_ll, TimeSpan.begin = TimeSpan.begin[1:5], 
    TimeSpan.end = TimeSpan.end[1:5])
data(SAGA_pal)
\dontrun{## plot MODIS images in Google Earth:
plotKML(LST.ts, colour_scale=SAGA_pal[[1]])
}

## -------------- Spatial Predictions --------- ##
library(sp)
library(rgdal)
library(gstat)
data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
## load grids:
data(meuse.grid)
gridded(meuse.grid) <- ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
\dontrun{## fit a model:
library(landmap)
omm <- fit.gstatModel(observations = meuse, formulaString = om~dist, 
   family = gaussian(log), covariates = meuse.grid)
## produce SpatialPredictions:
om.rk <- predict(omm, predictionLocations = meuse.grid)
## plot the whole geostatical mapping project in Google Earth:
plotKML(om.rk, colour_scale = SAGA_pal[[1]])
## plot each cell as polygon:
plotKML(om.rk, colour_scale = SAGA_pal[[1]], grid2poly = TRUE)
}

## -------------- RasterBrickSimulations --------- ##
\dontrun{
library(sp)
library(gstat)
data(barxyz)
## define the projection system:
prj = "+proj=tmerc +lat_0=0 +lon_0=18 +k=0.9999 +x_0=6500000 +y_0=0 
  +ellps=bessel +units=m 
  +towgs84=550.499,164.116,475.142,5.80967,2.07902,-11.62386,0.99999445824"
coordinates(barxyz) <- ~x+y
proj4string(barxyz) <- CRS(prj)
data(bargrid)
coordinates(bargrid) <- ~x+y
gridded(bargrid) <- TRUE
proj4string(bargrid) <- CRS(prj)
## fit a variogram and generate simulations:
Z.ovgm <- vgm(psill=1352, model="Mat", range=650, nugget=0, kappa=1.2)
sel <- runif(length(barxyz$Z))<.2  
## Note: this operation can be time consuming
sims <- krige(Z~1, barxyz[sel,], bargrid, model=Z.ovgm, nmax=20, 
   nsim=10, debug.level=-1)
## specify the cross-section:
t1 <- Line(matrix(c(bargrid@bbox[1,1], bargrid@bbox[1,2], 5073012, 5073012), ncol=2))
transect <- SpatialLines(list(Lines(list(t1), ID="t")), CRS(prj))
## glue to a RasterBrickSimulations object:
library(raster)
bardem_sims <- new("RasterBrickSimulations", variable = "elevations", 
  sampled = transect, realizations = brick(sims))
## plot the whole project and open in Google Earth:
data(R_pal)
plotKML(bardem_sims, colour_scale = R_pal[[4]])
}

## -------------- SpatialVectorsSimulations --------- ##
\dontrun{
data(barstr)
data(bargrid)
library(sp)
coordinates(bargrid) <- ~ x+y
gridded(bargrid) <- TRUE
## output topology:
cell.size = bargrid@grid@cellsize[1]
bbox = bargrid@bbox
nrows = round(abs(diff(bbox[1,])/cell.size), 0) 
ncols = round(abs(diff(bbox[2,])/cell.size), 0)
gridT = GridTopology(cellcentre.offset=bbox[,1], 
  cellsize=c(cell.size,cell.size), 
  cells.dim=c(nrows, ncols))
bar_sum <- count.GridTopology(gridT, vectL=barstr[1:5])
## NOTE: this operation can be time consuming!
## plot the whole project and open in Google Earth:
plotKML(bar_sum)
}

## -------------- SpatialMaxEntOutput --------- ##
\dontrun{
library(maptools)
library(rgdal)
data(bigfoot)
aea.prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 
   +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
data(USAWgrids)
gridded(USAWgrids) <- ~s1+s2
proj4string(USAWgrids) <- CRS(aea.prj)
bbox <- spTransform(USAWgrids, CRS("+proj=longlat +datum=WGS84"))@bbox
sel = bigfoot$Lon > bbox[1,1] & bigfoot$Lon < bbox[1,2] &
    bigfoot$Lat > bbox[2,1] & bigfoot$Lat < bbox[2,2]
bigfoot <- bigfoot[sel,]
coordinates(bigfoot) <- ~Lon+Lat
proj4string(bigfoot) <- CRS("+proj=longlat +datum=WGS84")
library(spatstat)
bigfoot.aea <- as.ppp(spTransform(bigfoot, CRS(aea.prj)))
## Load the covariates:
sel.grids <- c("globedem","nlights03","sdroads","gcarb","twi","globcov")
library(landmap)
library(dismo)
## run MaxEnt analysis:
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if(file.exists(jar)){
  bigfoot.smo <- MaxEnt(bigfoot.aea, USAWgrids[sel.grids])
  icon = "http://plotkml.r-forge.r-project.org/bigfoot.png"
  data(R_pal)
  plotKML(bigfoot.smo, colour_scale = R_pal[["bpy_colors"]], shape = icon)
}
}
}
\keyword{spatial}
\keyword{methods}
