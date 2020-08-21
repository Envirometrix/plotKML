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

# A more complex example
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png" 
kml_file = "kml_file_point.kml"
legend_file = "kml_legend.png"
kml_open(kml_file)
kml_layer(eberg_sf["CLYMHT_A"], colour = CLYMHT_A, size = 1, points_names = "", shape = shape, alpha = 0.75)
kml_legend.bar(eberg_sf$CLYMHT_A, legend.file = legend_file, legend.pal = SAGA_pal[[1]])
kml_screen(image.file = legend_file)
kml_close(kml_file)
kml_View(kml_file)

# LINESTRING
data(eberg_contours)
eberg_contours_sf <-  st_as_sf(eberg_contours)
plotKML(eberg_contours_sf, colour = Z, altitude = Z, width = Z)

# POLYGON
data(eberg_zones)
eberg_zones_sf <- st_as_sf(eberg_zones)
plotKML(eberg_zones_sf, altitude = runif(nrow(eberg_zones_sf), 230, 500))

# RasterLayer
g <- read_stars(system.file("external/test.grd", package = "raster"))
plotKML(g)

