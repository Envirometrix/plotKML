setMethod(
  f = "plotKML", 
  signature = "stars", 
  definition = function(
    obj, 
    ...
  ) {
    if (is.na(stars::st_raster_type(obj))) {
      obj <- as(obj, "STFDF")
      plotKML(obj, ...)
    } else if (stars::st_raster_type(obj) == "regular") { # Raster data cube
      # Transform into Raster
      obj <- as(obj, "Raster")
      plotKML(obj, ...)
      
      # Maybe add tests for RasterBrick
    } else {
      stop("ABC")
      
    }
  }
)

