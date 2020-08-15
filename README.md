
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

I don’t know if that’s a problem, but, here, both approaches return an
error:

``` r
plotKML(eberg, colour = CLYMHT_A, open.kml = FALSE)
#> Error in .local(obj, ...): object 'CLYMHT_A' not found
plotKML(eberg_sf, colour = CLYMHT_A, open.kml = FALSE)
#> Error in do.call(".plotKML_sf_POINT", list(obj = obj, folder.name = folder.name, : object 'CLYMHT_A' not found
```

I can also use `kml_layer` functions:

``` r
data(eberg_grid)
gridded(eberg_grid) <- ~ x + y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
eberg_grid_sf <- st_as_sf(eberg_grid)

# sfc objects
kml_open("eberg_grids.kml")
#> KML file opened for writing...
kml_layer(st_geometry(eberg_grid_sf))
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
kml_close("eberg_grids.kml")
#> Closing  eberg_grids.kml

# sf objects
kml_open("eberg_grid_sf.kml")
#> KML file opened for writing...
kml_layer(eberg_grid_sf, colour = DEMSRT6, colour_scale = R_pal[["terrain_colors"]])
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
kml_layer(eberg_grid_sf, colour = TWISRT6, colour_scale = SAGA_pal[[1]])
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
kml_close("eberg_grid_sf.kml")
#> Closing  eberg_grid_sf.kml
```

Then I can present `sf` functions for MULTIPOINT objects. The idea is
exactly the same, but MULTIPOINT object are converted to POINT.

``` r
eberg_sf_MULTIPOINT <- eberg_sf %>% 
  dplyr::mutate(random_ID = sample(1:4, size = dplyr::n(), replace = TRUE)) %>% 
  dplyr::group_by(random_ID) %>% 
  dplyr::summarise()
plotKML(eberg_sf_MULTIPOINT["random_ID"], open.kml = FALSE)
#> Casting the input MULTIPOINT objct into POINT object.
#> Warning in st_cast.sf(obj, "POINT"): repeating attributes for all sub-geometries
#> for which they may not be constant
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_sf_MULTIPOINT__random_ID__.kml
#> Object written to: eberg_sf_MULTIPOINT__random_ID__.kml
```

Then I can work with LINESTRING, starting from `sp` example:

``` r
# sp
data(eberg_contours)
plotKML(eberg_contours, open.kml = FALSE)
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ...
#> Writing to KML...
#> Closing  eberg_contours.kml
#> Object written to: eberg_contours.kml
plotKML(eberg_contours, colour = Z, altitude = Z, open.kml = FALSE)
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ...
#> Writing to KML...
#> Closing  eberg_contours.kml
#> Object written to: eberg_contours.kml
# sf
eberg_contours_sf <- st_as_sf(eberg_contours)
plotKML(eberg_contours_sf, open.kml = FALSE)
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_contours_sf.kml
#> Object written to: eberg_contours_sf.kml
plotKML(eberg_contours_sf, colour = Z, altitude = Z, open.kml = FALSE)
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_contours_sf.kml
#> Object written to: eberg_contours_sf.kml
```

Again, the kml files are identical but for super small differences due
to rounding
errors:

``` r
all.equal(readLines("eberg_contours.kml"), readLines("eberg_contours_sf.kml"))
#> [1] "5262 string mismatches"

id_mismatches <- which(readLines("eberg_contours.kml") != readLines("eberg_contours_sf.kml"))
readLines("eberg_contours.kml")[id_mismatches[1:5]]
#> [1] "    <name>eberg_contours</name>"         
#> [2] "      <name>SpatialLinesDataFrame</name>"
#> [3] " 10.1495311715266,51.5620218482631,160"  
#> [4] " 10.1455663596992,51.5620606439621,160"  
#> [5] " 10.1402385008461,51.5652585172413,160"
readLines("eberg_contours_sf.kml")[id_mismatches[1:5]]
#> [1] "    <name>eberg_contours_sf</name>"    
#> [2] "      <name>sfdata.frame</name>"       
#> [3] " 10.1495311715266,51.562021848263,160" 
#> [4] " 10.1455663596992,51.562060643962,160" 
#> [5] " 10.1402385008461,51.5652585172412,160"
```

The same ideas can be applied to MULTILINESTRING objects. The only
problem is that some of the features in `eberg_contous_sf` are not valid
according to the simple feature definition of linestring. For example

``` r
eberg_contours@lines[[4]]
#> An object of class "Lines"
#> Slot "Lines":
#> [[1]]
#> An object of class "Line"
#> Slot "coords":
#>         [,1]    [,2]
#> [1,] 3579412 5714807
#> 
#> 
#> 
#> Slot "ID":
#> [1] "3"
```

since it would represent a LINESTRING with only 1
point:

``` r
st_is_valid(st_geometry(eberg_contours_sf)[[4]], NA_on_exception = FALSE, reason = TRUE)
#> Error in CPL_geos_is_valid_reason(x): Evaluation error: IllegalArgumentException: point array must contain 0 or >1 elements.
```

So I need to exclude these elements and create a MULTILINESTRING object,

``` r
ID_valid <- vapply(
  st_geometry(eberg_contours_sf), 
  function(x) isTRUE(st_is_valid(x)), 
  logical(1)
)
eberg_contous_sf_multi <- eberg_contours_sf %>% 
  dplyr::filter(ID_valid) %>% 
  dplyr::group_by(Z) %>% 
  dplyr::summarise()
plotKML(eberg_contous_sf_multi, colour = Z, altitude = Z, open.kml = FALSE)
#> Casting the input MULTILINESTRING objct into LINESTRING object.
#> Warning in st_cast.sf(obj, "LINESTRING"): repeating attributes for all sub-
#> geometries for which they may not be constant
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_contous_sf_multi.kml
#> Object written to: eberg_contous_sf_multi.kml
```

I can also use `kml_layer` functions:

``` r
# sfc LINESTRING object
kml_open("eberg_contours_sfc.kml")
#> KML file opened for writing...
kml_layer(st_geometry(eberg_contours_sf))
#> Casting the input MULTILINESTRING object into LINESTRING.
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
kml_close("eberg_contours_sfc.kml")
#> Closing  eberg_contours_sfc.kml
```

Then I can work with POLYGON, starting from `sp` example:

``` r
# sp 
set.seed(1) # I set the seed to compare the kml files
data(eberg_zones)
plotKML(eberg_zones["ZONES"], open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ...
#> Writing to KML...
#> Closing  eberg_zones__ZONES__.kml
#> Object written to: eberg_zones__ZONES__.kml
## add altitude:
zmin = 230
plotKML(eberg_zones["ZONES"], altitude = zmin + runif(length(eberg_zones)) * 500, open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ...
#> Writing to KML...
#> Closing  eberg_zones__ZONES__.kml
#> Object written to: eberg_zones__ZONES__.kml

# sf objects with sfc_POLYGON geometry
eberg_zones_sf <- st_as_sf(eberg_zones)
set.seed(1)
plotKML(eberg_zones_sf["ZONES"], open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_zones_sf__ZONES__.kml
#> Object written to: eberg_zones_sf__ZONES__.kml
plotKML(eberg_zones_sf["ZONES"], altitude = zmin + runif(length(eberg_zones)) * 500, open.kml = FALSE)
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_zones_sf__ZONES__.kml
#> Object written to: eberg_zones_sf__ZONES__.kml
```

I can compare the
results:

``` r
all.equal(readLines("eberg_zones__ZONES__.kml"), readLines("eberg_zones_sf__ZONES__.kml"))
#> [1] "336 string mismatches"
```

The differences here are caused by extremely small rounding
problems:

``` r
id_mismatches <- readLines("eberg_zones__ZONES__.kml") != readLines("eberg_zones_sf__ZONES__.kml")
readLines("eberg_zones__ZONES__.kml")[id_mismatches][1:5]
#> [1] "    <name>eberg_zones__ZONES__</name>"             
#> [2] "      <name>SpatialPolygonsDataFrame</name>"       
#> [3] " 10.0128475331835,51.591802082619,362.75433157105" 
#> [4] " 10.0180147411512,51.5889485309108,362.75433157105"
#> [5] " 10.0226378144009,51.5859871365548,362.75433157105"
readLines("eberg_zones_sf__ZONES__.kml")[id_mismatches][42]
#> [1] " 10.0650353504248,51.5016807295379,416.061949818395"
```

Again, the same ideas can be applied to MULTIPOLYGON objects:

``` r
eberg_zones_sf_MULTI <- eberg_zones_sf %>% 
  dplyr::group_by(ZONES) %>% 
  dplyr::summarise() %>% 
  st_cast("MULTIPOLYGON") 
plotKML(eberg_zones_sf_MULTI["ZONES"], open.kml = FALSE)
#> Casting the input MULTIPOLYGON objct into POLYGON object.
#> Warning in st_cast.sf(obj, "POLYGON"): repeating attributes for all sub-
#> geometries for which they may not be constant
#> Plotting the first variable on the list
#> KML file opened for writing...
#> Reprojecting to +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#> Writing to KML...
#> Closing  eberg_zones_sf_MULTI__ZONES__.kml
#> Object written to: eberg_zones_sf_MULTI__ZONES__.kml
```
