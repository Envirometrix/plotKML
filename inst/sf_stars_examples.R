# packages
library(sf)
library(plotKML)
library(stars)

# base - plot approach
data("eberg")
eberg_sf = st_as_sf(eberg, coords = c("X", "Y"), crs = 31467)
plot(eberg_sf["CLYMHT_A"], pch = 16) 

# plotKML approach
plotKML(eberg_sf["CLYMHT_A"], points_names = "")
plotKML(eberg_sf["CLYMHT_A"], points_names = "", colour_scale = SAGA_pal[[2]]) #Change palette

# A more complex example
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png" 
kml_file = "kml_file_point.kml"
legend_file = "kml_legend.png"
kml_open(kml_file)
kml_layer(
  eberg_sf["CLYMHT_A"], 
  colour = CLYMHT_A, 
  points_names = eberg_sf[["CLYMHT_A"]], 
  size = 1, 
  shape = shape, 
  alpha = 0.75, 
  colour_scale = SAGA_pal[[2]]
)
kml_legend.bar(eberg_sf$CLYMHT_A, legend.file = legend_file, legend.pal = SAGA_pal[[1]])
kml_screen(image.file = legend_file)
kml_close(kml_file)
kml_View(kml_file)

# LINESTRING
data(eberg_contours)
eberg_contours_sf <-  st_as_sf(eberg_contours)
plot(eberg_contours_sf["Z"]) # There are some problems in the object (see README)
plotKML(eberg_contours_sf, colour = Z, altitude = Z, width = Z)

# POLYGON
data(eberg_zones)
eberg_zones_sf <- st_as_sf(eberg_zones)
plot(eberg_zones_sf)
plotKML(eberg_zones_sf, altitude = runif(nrow(eberg_zones_sf), 230, 500))

# RasterLayer
# Start with a simple raster data cube
(g <- read_stars(system.file("external/test.grd", package = "raster")))
plotKML(g)

#  Default methods do not work if the third dimension does not represent a `Date` or `POSIXct` object: 
(L7 <- read_stars(system.file("tif/L7_ETMs.tif", package = "stars")))
plotKML(L7)

# It fails. The problems is that there is no method definition for `plotKML()`
# function for objects of class `RasterBrick`.

plotKML(raster::raster(as(L7, "Raster"), layer = 1))

#  Let's focus now on Vector Data cubes: 
data(air, package = "spacetime")
d = st_dimensions(station = st_as_sfc(stations), time = dates)
(aq = st_as_stars(list(PM10 = air), dimensions = d))
(agg = aggregate(aq, "months", mean, na.rm = TRUE))
plotKML(agg) 

#  Unfortunately there is a known bug in `plotKML`/`spacetime` and the following example fails:  
(a = aggregate(agg, st_as_sf(DE_NUTS1), mean, na.rm = TRUE))
plotKML(a) # error

#  Moreover, all the examples fail if the third dimension is not a temporal
#  object (since the methods we defined as wrappers around STFDF class):

nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
nc.df = st_set_geometry(nc, NULL)
mat = as.matrix(nc.df[c("BIR74", "SID74", "NWBIR74", "BIR79", "SID79", "NWBIR79")])
dim(mat) = c(county = 100, var = 3, year = 2) # make it a 3-dimensional array
dimnames(mat) = list(county = nc$NAME, var = c("BIR", "SID", "NWBIR"), year = c(1974, 1979))
(nc.st = st_as_stars(pop = mat))
(nc.geom <- st_set_dimensions(nc.st, 1, st_geometry(nc)))
plotKML(nc.geom)

# We can somehow fix this problem converting the `stars` object into `sf` format: 
plotKML(st_as_sf(nc.geom))

#  In some cases, it is still possible to redefine a temporal dimension (but
#  unfortunately the example fails for the same bug as the previous case):
  
(nc.geom <- st_set_dimensions(nc.geom, 3, as.Date(c("1974-01-01","1979-01-01"))))
(nc.geom <- split(aperm(nc.geom, c(1,3,2))))
plotKML(nc.geom)
