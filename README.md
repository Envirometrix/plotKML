
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

plotKML
=======

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

-   Hengl, T., Roudier, P., Beaudette, D., & Pebesma, E. (2015).
    **plotKML: Scientific visualization of spatio-temporal data**.
    Journal of Statistical Software, 63(5), 1-25.
    <a href="https://www.jstatsoft.org/article/view/v063i05" class="uri">https://www.jstatsoft.org/article/view/v063i05</a>

Installing
----------

Install development versions from github:

    library(devtools)
    install_github("envirometrix/plotKML")

Integration with sf
-------------------

We will now present new `sf` functions and compare the with current `sp`
approach. Start with POINTS

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
    #> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
    #> datum Deutsches_Hauptdreiecksnetz in CRS definition
    ## subset to 20 percent:
    eberg <- eberg[runif(nrow(eberg)) < .1, ]
    # sp methods
    plotKML(eberg["CLYMHT_A"], open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Warning in proj4string(obj): CRS object has comment, which is lost in output
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Writing to KML...
    #> Closing  eberg__CLYMHT_A__.kml
    #> Object written to: eberg__CLYMHT_A__.kml
    plotKML(eberg["CLYMHT_A"], colour_scale = rep("#FFFF00", 2), points_names = "", open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Warning in proj4string(obj): CRS object has comment, which is lost in output
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Writing to KML...
    #> Closing  eberg__CLYMHT_A__.kml
    #> Object written to: eberg__CLYMHT_A__.kml

    # convert eberg to sf format
    eberg_sf <- st_as_sf(eberg)

    # and apply sf methods
    plotKML(eberg_sf["CLYMHT_A"], open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_sf__CLYMHT_A__.kml
    #> Object written to: eberg_sf__CLYMHT_A__.kml
    plotKML(eberg_sf["CLYMHT_A"], colour_scale = rep("#FFFF00", 2), points_names = "", open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_sf__CLYMHT_A__.kml
    #> Object written to: eberg_sf__CLYMHT_A__.kml

Recent version of sf could show a warning message relative to the old
PROJ4string specified by `get("ref_CRS", envir = plotKML.opts)`. Maybe
`"ref_CRS"` should be changed or updated.

We can now compare the two implementations:

    all.equal(readLines("eberg__CLYMHT_A__.kml"), readLines("eberg_sf__CLYMHT_A__.kml"))
    #> Warning in readLines("eberg__CLYMHT_A__.kml"): incomplete final line found on
    #> 'eberg__CLYMHT_A__.kml'
    #> Warning in readLines("eberg_sf__CLYMHT_A__.kml"): incomplete final line found on
    #> 'eberg_sf__CLYMHT_A__.kml'
    #> [1] "2 string mismatches"

I think the warning messages are not important here. . The two string
mismatches are simply caused by the different names and classes:

    id_mismatches <- readLines("eberg__CLYMHT_A__.kml") != readLines("eberg_sf__CLYMHT_A__.kml")
    readLines("eberg__CLYMHT_A__.kml")[id_mismatches]
    #> [1] "    <name>eberg__CLYMHT_A__</name>"       
    #> [2] "      <name>SpatialPointsDataFrame</name>"
    readLines("eberg_sf__CLYMHT_A__.kml")[id_mismatches]
    #> [1] "    <name>eberg_sf__CLYMHT_A__</name>"
    #> [2] "      <name>sfdata.frame</name>"

I don’t know if that’s a problem, but, here, both approaches return an
error:

    plotKML(eberg, colour = CLYMHT_A, open.kml = FALSE)
    #> Error in .local(obj, ...): oggetto "CLYMHT_A" non trovato
    plotKML(eberg_sf, colour = CLYMHT_A, open.kml = FALSE)
    #> Error in do.call(".plotKML_sf_POINT", list(obj = obj, folder.name = folder.name, : oggetto "CLYMHT_A" non trovato

I can also use `kml_layer` functions:

    data(eberg_grid)
    gridded(eberg_grid) <- ~ x + y
    proj4string(eberg_grid) <- CRS("+init=epsg:31467")
    #> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
    #> datum Deutsches_Hauptdreiecksnetz in CRS definition
    eberg_grid_sf <- st_as_sf(eberg_grid)

    # sfc objects
    kml_open("eberg_grids.kml")
    #> KML file opened for writing...
    kml_layer(st_geometry(eberg_grid_sf))
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    kml_close("eberg_grids.kml")
    #> Closing  eberg_grids.kml

    # sf objects
    kml_open("eberg_grid_sf.kml")
    #> KML file opened for writing...
    kml_layer(eberg_grid_sf, colour = DEMSRT6, colour_scale = R_pal[["terrain_colors"]])
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    kml_layer(eberg_grid_sf, colour = TWISRT6, colour_scale = SAGA_pal[[1]])
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    kml_close("eberg_grid_sf.kml")
    #> Closing  eberg_grid_sf.kml

A more complex example:

    shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png" 
    kml_file = "kml_file_point.kml"
    legend_file = "kml_legend.png"
    kml_open(kml_file)
    kml_layer(eberg_sf["CLYMHT_A"], colour = CLYMHT_A, size = 1, points_names = "", shape = shape, alpha = 0.75)
    kml_legend.bar(eberg_sf$CLYMHT_A, legend.file = legend_file, legend.pal = SAGA_pal[[1]])
    kml_screen(image.file = legend_file)
    kml_close(kml_file)
    kml_View(kml_file)

Then I can present `sf` functions for MULTIPOINT objects. The idea is
exactly the same, but MULTIPOINT object are converted to POINT.

    eberg_sf_MULTIPOINT <- eberg_sf %>% 
      dplyr::mutate(random_ID = sample(1:4, size = dplyr::n(), replace = TRUE)) %>% 
      dplyr::group_by(random_ID) %>% 
      dplyr::summarise()
    #> `summarise()` ungrouping output (override with `.groups` argument)
    plotKML(eberg_sf_MULTIPOINT["random_ID"], open.kml = FALSE)
    #> Casting the input MULTIPOINT objct into POINT object.
    #> Warning in st_cast.sf(obj, "POINT"): repeating attributes for all sub-geometries
    #> for which they may not be constant
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_sf_MULTIPOINT__random_ID__.kml
    #> Object written to: eberg_sf_MULTIPOINT__random_ID__.kml

Then I can work with LINESTRING, starting from `sp` example:

    # sp
    data(eberg_contours)
    plotKML(eberg_contours, open.kml = FALSE)
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Warning in spTransform(xSP, CRSobj, ...): NULL source CRS comment, falling back
    #> to PROJ string
    #> Warning in spTransform(xSP, CRSobj, ...): +init dropped in PROJ string
    #> Writing to KML...
    #> Closing  eberg_contours.kml
    #> Object written to: eberg_contours.kml
    plotKML(eberg_contours, colour = Z, altitude = Z, open.kml = FALSE)
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Warning in spTransform(xSP, CRSobj, ...): NULL source CRS comment, falling back
    #> to PROJ string

    #> Warning in spTransform(xSP, CRSobj, ...): +init dropped in PROJ string
    #> Writing to KML...
    #> Closing  eberg_contours.kml
    #> Object written to: eberg_contours.kml
    # sf
    eberg_contours_sf <- st_as_sf(eberg_contours)
    #> Warning in CPL_crs_from_input(x): GDAL Message 1: +init=epsg:XXXX syntax is
    #> deprecated. It might return a CRS with a non-EPSG compliant axis order.
    plotKML(eberg_contours_sf, open.kml = FALSE)
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_contours_sf.kml
    #> Object written to: eberg_contours_sf.kml
    plotKML(eberg_contours_sf, colour = Z, altitude = Z, open.kml = FALSE)
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_contours_sf.kml
    #> Object written to: eberg_contours_sf.kml

Again, the kml files are identical but for super small differences due
to rounding errors:

    all.equal(readLines("eberg_contours.kml"), readLines("eberg_contours_sf.kml"))
    #> [1] "2 string mismatches"

    id_mismatches <- which(readLines("eberg_contours.kml") != readLines("eberg_contours_sf.kml"))
    readLines("eberg_contours.kml")[id_mismatches[1:5]]
    #> [1] "    <name>eberg_contours</name>"         
    #> [2] "      <name>SpatialLinesDataFrame</name>"
    #> [3] NA                                        
    #> [4] NA                                        
    #> [5] NA
    readLines("eberg_contours_sf.kml")[id_mismatches[1:5]]
    #> [1] "    <name>eberg_contours_sf</name>" "      <name>sfdata.frame</name>"   
    #> [3] NA                                   NA                                  
    #> [5] NA

The same ideas can be applied to MULTILINESTRING objects. The only
problem is that some of the features in `eberg_contous_sf` are not valid
according to the simple feature definition of linestring. For example

    eberg_contours@lines[[4]]
    #> An object of class "Lines"
    #> Slot "Lines":
    #> [[1]]
    #> An object of class "Line"
    #> Slot "coords":
    #>         [,1]    [,2]
    #> [1,] 3579413 5714807
    #> 
    #> 
    #> 
    #> Slot "ID":
    #> [1] "3"

since it would represent a LINESTRING with only 1 point:

    st_is_valid(st_geometry(eberg_contours_sf)[[4]], NA_on_exception = FALSE, reason = TRUE)
    #> Error in CPL_geos_is_valid_reason(x): Evaluation error: IllegalArgumentException: point array must contain 0 or >1 elements.

So I need to exclude these elements and create a MULTILINESTRING object,

    ID_valid <- vapply(
      st_geometry(eberg_contours_sf), 
      function(x) isTRUE(st_is_valid(x)), 
      logical(1)
    )
    eberg_contous_sf_multi <- eberg_contours_sf %>% 
      dplyr::filter(ID_valid) %>% 
      dplyr::group_by(Z) %>% 
      dplyr::summarise()
    #> `summarise()` ungrouping output (override with `.groups` argument)
    plotKML(eberg_contous_sf_multi, colour = Z, altitude = Z, open.kml = FALSE)
    #> Casting the input MULTILINESTRING objct into LINESTRING object.
    #> Warning in st_cast.sf(obj, "LINESTRING"): repeating attributes for all sub-
    #> geometries for which they may not be constant
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_contous_sf_multi.kml
    #> Object written to: eberg_contous_sf_multi.kml

I can also use `kml_layer` functions:

    # sfc LINESTRING object
    kml_open("eberg_contours_sfc.kml")
    #> KML file opened for writing...
    kml_layer(st_geometry(eberg_contours_sf))
    #> Casting the input MULTILINESTRING object into LINESTRING.
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    kml_close("eberg_contours_sfc.kml")
    #> Closing  eberg_contours_sfc.kml

Then I can work with POLYGON, starting from `sp` example:

    # sp 
    set.seed(1) # I set the seed to compare the kml files
    data(eberg_zones)
    plotKML(eberg_zones["ZONES"], open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Warning in spTransform(xSP, CRSobj, ...): NULL source CRS comment, falling back
    #> to PROJ string
    #> Warning in spTransform(xSP, CRSobj, ...): +init dropped in PROJ string
    #> Writing to KML...
    #> Closing  eberg_zones__ZONES__.kml
    #> Object written to: eberg_zones__ZONES__.kml
    ## add altitude:
    zmin = 230
    plotKML(eberg_zones["ZONES"], altitude = zmin + runif(length(eberg_zones)) * 500, open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Warning in spTransform(xSP, CRSobj, ...): NULL source CRS comment, falling back
    #> to PROJ string

    #> Warning in spTransform(xSP, CRSobj, ...): +init dropped in PROJ string
    #> Writing to KML...
    #> Closing  eberg_zones__ZONES__.kml
    #> Object written to: eberg_zones__ZONES__.kml

    # sf objects with sfc_POLYGON geometry
    eberg_zones_sf <- st_as_sf(eberg_zones)
    set.seed(1)
    plotKML(eberg_zones_sf["ZONES"], open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_zones_sf__ZONES__.kml
    #> Object written to: eberg_zones_sf__ZONES__.kml
    plotKML(eberg_zones_sf["ZONES"], altitude = zmin + runif(length(eberg_zones)) * 500, open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_zones_sf__ZONES__.kml
    #> Object written to: eberg_zones_sf__ZONES__.kml

I can compare the results:

    all.equal(readLines("eberg_zones__ZONES__.kml"), readLines("eberg_zones_sf__ZONES__.kml"))
    #> [1] "2 string mismatches"

The differences here are caused by extremely small rounding problems:

    id_mismatches <- readLines("eberg_zones__ZONES__.kml") != readLines("eberg_zones_sf__ZONES__.kml")
    readLines("eberg_zones__ZONES__.kml")[id_mismatches][1:5]
    #> [1] "    <name>eberg_zones__ZONES__</name>"      
    #> [2] "      <name>SpatialPolygonsDataFrame</name>"
    #> [3] NA                                           
    #> [4] NA                                           
    #> [5] NA
    readLines("eberg_zones_sf__ZONES__.kml")[id_mismatches][42]
    #> [1] NA

Again, the same ideas can be applied to MULTIPOLYGON objects:

    eberg_zones_sf_MULTI <- eberg_zones_sf %>% 
      dplyr::group_by(ZONES) %>% 
      dplyr::summarise() %>% 
      st_cast("MULTIPOLYGON") 
    #> `summarise()` ungrouping output (override with `.groups` argument)
    plotKML(eberg_zones_sf_MULTI["ZONES"], open.kml = FALSE)
    #> Casting the input MULTIPOLYGON objct into POLYGON object.
    #> Warning in st_cast.sf(obj, "POLYGON"): repeating attributes for all sub-
    #> geometries for which they may not be constant
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs
    #> Writing to KML...
    #> Closing  eberg_zones_sf_MULTI__ZONES__.kml
    #> Object written to: eberg_zones_sf_MULTI__ZONES__.kml

I will now present `plotKML` function applied to `stars` objects. We
extended the `plotKML()` function to `stars` objects representing Raster
Data Cubes creating a wrapper to RasterLayer method:

    library(stars)
    #> Loading required package: abind
    # Start with a raster data cube
    (g <- read_stars(system.file("external/test.grd", package = "raster")))
    #> stars object with 2 dimensions and 1 attribute
    #> attribute(s):
    #>    test.grd      
    #>  Min.   : 138.7  
    #>  1st Qu.: 294.0  
    #>  Median : 371.9  
    #>  Mean   : 425.6  
    #>  3rd Qu.: 501.0  
    #>  Max.   :1736.1  
    #>  NA's   :6022    
    #> dimension(s):
    #>   from  to offset delta                       refsys point values x/y
    #> x    1  80 178400    40 +proj=sterea +lat_0=52.15...    NA   NULL [x]
    #> y    1 115 334000   -40 +proj=sterea +lat_0=52.15...    NA   NULL [y]
    plotKML(g, open.kml = FALSE)
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Warning in proj4string(obj): CRS object has comment, which is lost in output
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Writing to KML...
    #> Closing  obj.kml
    #> Object written to: obj.kml

Default methods do not work if the third dimension in a `stars` object
do not represent a Date or POSIXct object:

    # Test with another example: 
    (L7 <- read_stars(system.file("tif/L7_ETMs.tif", package = "stars")))
    #> stars object with 3 dimensions and 1 attribute
    #> attribute(s):
    #>   L7_ETMs.tif    
    #>  Min.   :  1.00  
    #>  1st Qu.: 54.00  
    #>  Median : 69.00  
    #>  Mean   : 68.91  
    #>  3rd Qu.: 86.00  
    #>  Max.   :255.00  
    #> dimension(s):
    #>      from  to  offset delta                       refsys point values x/y
    #> x       1 349  288776  28.5 UTM Zone 25, Southern Hem... FALSE   NULL [x]
    #> y       1 352 9120761 -28.5 UTM Zone 25, Southern Hem... FALSE   NULL [y]
    #> band    1   6      NA    NA                           NA    NA   NULL
    plotKML(L7, open.kml = FALSE)
    #> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded datum Unknown based on GRS80 ellipsoid in CRS definition,
    #>  but +towgs84= values preserved
    #> Error in .local(obj, ...): Select only one Raster layer or provide a Time series

    # It fails. The problems is that there is no method definition for plotKML function for objects of class RasterBrick. 

    # The code behind as(x, "Raster") is defined here: 
    # https://github.com/r-spatial/stars/blob/e0c8506d8e00413721a1dfe83cc7f3d45b16c912/R/raster.R#L116-L124
    # and it says that if length(dim(x)) > 2 (i.e. there is an extra dimension other than x and y), then it creates a RasterBrick object. 

but we can still plot the raster associated to one of the layers in the
RasterBrick with a little bit of extra work:

    plotKML(raster::raster(as(L7, "Raster"), layer = 1), open.kml = FALSE)
    #> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded datum Unknown based on GRS80 ellipsoid in CRS definition,
    #>  but +towgs84= values preserved
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Warning in proj4string(obj): CRS object has comment, which is lost in output
    #> Reprojecting to +proj=longlat +datum=WGS84 +no_defs ...
    #> Writing to KML...
    #> Closing  raster__raster(as(L7,__Raster_),_layer_=_1).kml
    #> Object written to: raster__raster(as(L7,__Raster_),_layer_=_1).kml

We are working on defining an extension to `stars` object with a
temporal dimension as a wrapper around `RasterBrickTimeSeries` methods.

Let’s focus now on Vector data cubes:

    # Vector data cube
    data(air, package = "spacetime")
    d = st_dimensions(station = st_as_sfc(stations), time = dates)
    (aq = st_as_stars(list(PM10 = air), dimensions = d))
    #> stars object with 2 dimensions and 1 attribute
    #> attribute(s):
    #>      PM10        
    #>  Min.   :  0.00  
    #>  1st Qu.:  9.92  
    #>  Median : 14.79  
    #>  Mean   : 17.70  
    #>  3rd Qu.: 21.99  
    #>  Max.   :274.33  
    #>  NA's   :157659  
    #> dimension(s):
    #>         from   to     offset  delta                     refsys point
    #> station    1   70         NA     NA +proj=longlat +datum=WGS84  TRUE
    #> time       1 4383 1998-01-01 1 days                       Date FALSE
    #>                                                          values
    #> station POINT (9.585911 53.67057),...,POINT (9.446661 49.24068)
    #> time                                                       NULL
    (agg = aggregate(aq, "months", mean, na.rm = TRUE))
    #> stars object with 2 dimensions and 1 attribute
    #> attribute(s):
    #>      PM10       
    #>  Min.   : 2.99  
    #>  1st Qu.:13.01  
    #>  Median :16.43  
    #>  Mean   :17.68  
    #>  3rd Qu.:20.83  
    #>  Max.   :68.55  
    #>  NA's   :4926   
    #> dimension(s):
    #>         from  to offset delta                     refsys point
    #> time       1 144     NA    NA                       Date    NA
    #> station    1  70     NA    NA +proj=longlat +datum=WGS84  TRUE
    #>                                                          values
    #> time                                  1998-01-01,...,2009-12-01
    #> station POINT (9.585911 53.67057),...,POINT (9.446661 49.24068)
    plotKML(agg) 
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Warning in proj4string(obj): CRS object has comment, which is lost in output

    #> Warning in proj4string(obj): CRS object has comment, which is lost in output
    #> Writing to KML...
    #> Closing  obj.kml
    #> [1] 0

Unfortunately there is a known bug in `plotKML`/`spacetime` and the
following example fails:

    # Spatial aggregation: 
    (a = aggregate(aq, st_as_sf(DE_NUTS1), mean, na.rm = TRUE))
    #> although coordinates are longitude/latitude, st_intersects assumes that they are planar
    #> although coordinates are longitude/latitude, st_intersects assumes that they are planar
    #> stars object with 2 dimensions and 1 attribute
    #> attribute(s):
    #>      PM10         
    #>  Min.   :  1.075  
    #>  1st Qu.: 10.886  
    #>  Median : 15.316  
    #>  Mean   : 17.888  
    #>  3rd Qu.: 21.811  
    #>  Max.   :172.267  
    #>  NA's   :25679    
    #> dimension(s):
    #>          from   to     offset  delta                     refsys point
    #> geometry    1   16         NA     NA +proj=longlat +datum=WGS84 FALSE
    #> time        1 4383 1998-01-01 1 days                       Date FALSE
    #>                                                                     values
    #> geometry MULTIPOLYGON (((9.65046 49....,...,MULTIPOLYGON (((10.77189 51...
    #> time                                                                  NULL
    plotKML(a) # error
    #> Error in from@sp[from@index[, 1], ]: SpatialPolygons selection: can't find plot order if polygons are replicated

    # The error is in as(obj, "STIDF"), i.e. the conversion between STFDF and STIDF

Moreover, all the examples fail if the third dimension is not a temporal
object (since the methods we defined as wrappers around STFDF class):

    nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
    nc.df = st_set_geometry(nc, NULL)
    mat = as.matrix(nc.df[c("BIR74", "SID74", "NWBIR74", "BIR79", "SID79", "NWBIR79")])
    dim(mat) = c(county = 100, var = 3, year = 2) # make it a 3-dimensional array
    dimnames(mat) = list(county = nc$NAME, var = c("BIR", "SID", "NWBIR"), year = c(1974, 1979))
    (nc.st = st_as_stars(pop = mat))
    #> stars object with 3 dimensions and 1 attribute
    #> attribute(s):
    #>       pop       
    #>  Min.   :    0  
    #>  1st Qu.:    8  
    #>  Median :  538  
    #>  Mean   : 1657  
    #>  3rd Qu.: 1784  
    #>  Max.   :30757  
    #> dimension(s):
    #>        from  to offset delta refsys point              values
    #> county    1 100     NA    NA     NA    NA  Ashe,...,Brunswick
    #> var       1   3     NA    NA     NA    NA BIR  , SID  , NWBIR
    #> year      1   2     NA    NA     NA    NA          1974, 1979
    (nc.geom <- st_set_dimensions(nc.st, 1, st_geometry(nc)))
    #> stars object with 3 dimensions and 1 attribute
    #> attribute(s):
    #>       pop       
    #>  Min.   :    0  
    #>  1st Qu.:    8  
    #>  Median :  538  
    #>  Mean   : 1657  
    #>  3rd Qu.: 1784  
    #>  Max.   :30757  
    #> dimension(s):
    #>      from  to offset delta refsys point
    #> sfc     1 100     NA    NA  NAD27 FALSE
    #> var     1   3     NA    NA     NA    NA
    #> year    1   2     NA    NA     NA    NA
    #>                                                                 values
    #> sfc  MULTIPOLYGON (((-81.47276 3...,...,MULTIPOLYGON (((-78.65572 3...
    #> var                                                BIR  , SID  , NWBIR
    #> year                                                        1974, 1979
    plotKML(nc.geom)
    #> Error in SpatialPolygonsDataFrame(x, y, match.ID = match.ID, ...): Object length mismatch:
    #>      x has 100 Polygons objects, but y has 200 rows

We can somehow fix this problem converting the `stars` object into `sf`
format:

    # We can use the following approach
    plotKML(st_as_sf(nc.geom), open.kml = FALSE)
    #> Casting the input MULTIPOLYGON objct into POLYGON object.
    #> Warning in st_cast.sf(obj, "POLYGON"): repeating attributes for all sub-
    #> geometries for which they may not be constant
    #> Plotting the first variable on the list
    #> KML file opened for writing...
    #> Writing to KML...
    #> Closing  st_as_sf(nc.geom).kml
    #> Object written to: st_as_sf(nc.geom).kml

In some cases, it is still possible to redefine a temporal dimension
(but unfortunately the example fails for the same bug as the previous
case):

    (nc.geom <- st_set_dimensions(nc.geom, 3, as.Date(c("1974-01-01","1979-01-01"))))
    #> stars object with 3 dimensions and 1 attribute
    #> attribute(s):
    #>       pop       
    #>  Min.   :    0  
    #>  1st Qu.:    8  
    #>  Median :  538  
    #>  Mean   : 1657  
    #>  3rd Qu.: 1784  
    #>  Max.   :30757  
    #> dimension(s):
    #>      from  to     offset     delta refsys point
    #> sfc     1 100         NA        NA  NAD27 FALSE
    #> var     1   3         NA        NA     NA    NA
    #> year    1   2 1974-01-01 1826 days   Date    NA
    #>                                                                 values
    #> sfc  MULTIPOLYGON (((-81.47276 3...,...,MULTIPOLYGON (((-78.65572 3...
    #> var                                                BIR  , SID  , NWBIR
    #> year                                                              NULL
    (nc.geom <- split(aperm(nc.geom, c(1,3,2))))
    #> stars object with 2 dimensions and 3 attributes
    #> attribute(s):
    #>       BIR             SID             NWBIR      
    #>  Min.   :  248   Min.   : 0.000   Min.   :    1  
    #>  1st Qu.: 1177   1st Qu.: 2.000   1st Qu.:  206  
    #>  Median : 2265   Median : 5.000   Median :  742  
    #>  Mean   : 3762   Mean   : 7.515   Mean   : 1202  
    #>  3rd Qu.: 4451   3rd Qu.: 9.000   3rd Qu.: 1316  
    #>  Max.   :30757   Max.   :57.000   Max.   :11631  
    #> dimension(s):
    #>      from  to     offset     delta refsys point
    #> sfc     1 100         NA        NA  NAD27 FALSE
    #> year    1   2 1974-01-01 1826 days   Date    NA
    #>                                                                 values
    #> sfc  MULTIPOLYGON (((-81.47276 3...,...,MULTIPOLYGON (((-78.65572 3...
    #> year                                                              NULL
    plotKML(nc.geom)
    #> Error in from@sp[from@index[, 1], ]: SpatialPolygons selection: can't find plot order if polygons are replicated
