
kml.Spatial <- function(
  obj,
  folder.name = normalizeFilename(deparse(substitute(obj, env=parent.frame()))),
  file.name = paste(normalizeFilename(deparse(substitute(obj, env=parent.frame()))), ".kml", sep=""),
  kmz = get("kmz", envir = plotKML.opts),
  ...
){

  kml_open(folder.name = folder.name, file.name = file.name)

  kml_layer(obj = obj, ...)

  kml_close(file.name = file.name)

  if (kmz == TRUE){
      kml_compress(file.name = file.name)
  }
}

setMethod("kml", "Spatial", kml.Spatial)
setMethod("kml", "Raster", kml.Spatial)
setMethod("kml", "SoilProfileCollection", kml.Spatial)
setMethod("kml", "SpatialPhotoOverlay", kml.Spatial)
setMethod("kml", "STIDF", kml.Spatial)

# end of script;