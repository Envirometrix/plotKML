
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# plotKML

plotKML: Visualization of Spatial and Spatio-Temporal Objects in Google
Earth

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/Envirometrix/plotKML.svg?branch=master)](https://travis-ci.org/Envirometrix/plotKML)
[![R build
status](https://github.com/Envirometrix/plotKML/workflows/R-CMD-check/badge.svg)](https://github.com/Envirometrix/plotKML/actions)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/plotKML)](https://cran.r-project.org/package=plotKML)
[![Github\_Status\_Badge](https://img.shields.io/badge/Github-0.7--1-blue.svg)](https://github.com/Envirometrix/plotKML)
<!-- badges: end -->

How to cite:

  - Hengl, T., Roudier, P., Beaudette, D., & Pebesma, E. (2015).
    **plotKML: Scientific visualization of spatio-temporal data**.
    Journal of Statistical Software, 63(5), 1-25.
    <https://www.jstatsoft.org/article/view/v063i05>

## Installing

Install development versions from github:

``` r
library(devtools)
install_github("envirometrix/plotKML")
```

## Integration with sf

I will now present new `sf` functions and compare with current `sp`
functions. Start with POINTS

``` r
suppressPackageStartupMessages({
  library(plotKML)
  library(sp)
  library(rgdal)
  library(sf)
})
# Load data
data(eberg)
coordinates(eberg) <- ~ X + Y
proj4string(eberg) <- CRS("+init=epsg:31467")
## subset to 20 percent:
eberg <- eberg[runif(nrow(eberg)) < .1, ]
# sp methods
plotKML(eberg["CLYMHT_A"], open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ...
#> Writing to KML...
#> Closing  eberg__CLYMHT_A__.kml
#> Object written to: eberg__CLYMHT_A__.kml
plotKML(eberg["CLYMHT_A"], colour_scale = rep("#FFFF00", 2), points_names = "", open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ...
#> Writing to KML...
#> Closing  eberg__CLYMHT_A__.kml
#> Object written to: eberg__CLYMHT_A__.kml

# convert eberg to sf format
eberg_sf <- st_as_sf(eberg)

# and apply sf methods
plotKML(eberg_sf["CLYMHT_A"], open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_sf__CLYMHT_A__.kml
#> Object written to: eberg_sf__CLYMHT_A__.kml
plotKML(eberg_sf["CLYMHT_A"], colour_scale = rep("#FFFF00", 2), points_names = "", open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_sf__CLYMHT_A__.kml
#> Object written to: eberg_sf__CLYMHT_A__.kml
```

Recent version of sf could show a warning message relative to the old
PROJ4string specified by `get("ref_CRS", envir = plotKML.opts)`. Maybe
`"ref_CRS"` should be changed or updated.

We can now compare the two
implementations:

``` r
all.equal(readLines("eberg__CLYMHT_A__.kml"), readLines("eberg_sf__CLYMHT_A__.kml"))
#> Warning in readLines("eberg__CLYMHT_A__.kml"): incomplete final line found on
#> 'eberg__CLYMHT_A__.kml'
#> Warning in readLines("eberg_sf__CLYMHT_A__.kml"): incomplete final line found on
#> 'eberg_sf__CLYMHT_A__.kml'
#> [1] "2 string mismatches"
```

I think the warning messages are not important here. . The two string
mismatches are simply caused by the different names and
classes:

``` r
id_mismatches <- readLines("eberg__CLYMHT_A__.kml") != readLines("eberg_sf__CLYMHT_A__.kml")
readLines("eberg__CLYMHT_A__.kml")[id_mismatches]
#> [1] "    <name>eberg__CLYMHT_A__</name>"       
#> [2] "      <name>SpatialPointsDataFrame</name>"
readLines("eberg_sf__CLYMHT_A__.kml")[id_mismatches]
#> [1] "    <name>eberg_sf__CLYMHT_A__</name>"
#> [2] "      <name>sfdata.frame</name>"
```
