setMethod(
  f = "plotKML", 
  signature = "stars", 
  definition = function(
    obj, 
    sampled = NULL,
    ...
  ) {
    if (is.na(stars::st_raster_type(obj))) { # Vector data cube
      obj <- as(obj, "STFDF")
      plotKML(obj, ...)
    } else if (stars::st_raster_type(obj) == "regular") { # Raster data cube
      # Transform into Raster
      var <- names(obj)[1]
      obj <- as(obj, "Raster")
      if (is(obj, "RasterLayer"))
        plotKML(obj, ...)
      else {
        if (inherits(getZ(obj), c("POSIXct", "Date")) ) {
          stop(
            "We are working on the implementation of regular stars objects with temporal dimension."
          )
          # Create RasterBrickTimeSeries
          # z <- raster::getZ(obj)
          # obj <- new(
          #   "RasterBrickTimeSeries", 
          #   variable = var, 
          #   sampled = sampled, 
          #   rasters = obj,
          #   TimeSpan.begin = z, 
          #   TimeSpan.end = z
          # )
          # plotKML(obj, ...)
        } else {
          stop("Select only one Raster layer or provide a Time series")
        }
      }
    } else {
      stop("Input stars object should be Raster Data Cube or regular vector data cube")
    }
  }
)

