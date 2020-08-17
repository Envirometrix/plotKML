# The following tests are very simple and they check that plotKML function works
# with sf input

# Load data
library(sp)
library(rgdal)
data(eberg)
coordinates(eberg) <- ~ X + Y
proj4string(eberg) <- CRS("+init=epsg:31467")
eberg <- eberg[runif(nrow(eberg)) <.1, ]
eberg_sf <- sf::st_as_sf(eberg)
data(eberg_contours)
eberg_contours_sf <- sf::st_as_sf(eberg_contours)
data(eberg_zones)
eberg_zones_sf <- sf::st_as_sf(eberg_zones)
zmin = 230


test_that("plotKML works with sf input", {
  expect_error(plotKML(eberg_sf["CLYMHT_A"], open.kml = FALSE), NA)
  expect_error(
    plotKML(eberg_sf["CLYMHT_A"], colour_scale = rep("#FFFF00", 2), points_names = "", open.kml = FALSE), 
    NA
  )
  expect_error(plotKML(eberg_contours_sf, open.kml = FALSE), NA)
  expect_error(plotKML(eberg_contours_sf, colour = Z, altitude = Z, open.kml = FALSE), NA)
  expect_error(plotKML(eberg_zones_sf["ZONES"], open.kml = FALSE), NA)
  expect_error(
    plotKML(eberg_zones_sf["ZONES"], altitude = zmin + runif(length(eberg_zones)) * 500, open.kml = FALSE), 
    NA
  )
})


