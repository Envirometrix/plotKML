# AG: I define here a new method for plotKML, extending it to "sf" objects with
# POINT, LINESTRING, POLYGON geometry.

# The default arguments of the new method are `obj`, `folder.name`, `file.name`,
# `metadata`, `kmz`, and `open.kml`, while all aesthetic parameters are included
# in the `...` argument and processed according to the type of the geometry
# column.

setMethod(
  f = "plotKML", 
  signature = "sf", 
  definition = function(
    obj, 
    folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))), 
    file.name = paste(folder.name, ".kml", sep = ""), 
    metadata = NULL, 
    kmz = get("kmz", envir = plotKML.opts), 
    open.kml = TRUE, 
    ...
  ) {
    # AG: The behaviour of the function depends on the type of the geometry
    # column. If the geometry column is sfc_POINT (i.e. POINTs) then the
    # function calls .plotKML_sf_POINT which is defined below (and not exported)
    if (inherits(sf::st_geometry(obj), "sfc_POINT")) {
      do.call(".plotKML_sf_POINT", list(
        obj = obj, 
        folder.name = folder.name,
        file.name = file.name, 
        metadata = metadata, 
        kmz = kmz, 
        open.kml = open.kml, 
        ...
      ))
    } else {
      stop(
        "plotKML function is not defined for sf objects with", 
        class(sf::st_geometry(obj)), 
        "geometry. Please raise a new issue at ..."
      )
    }
  }
)

.plotKML_sf_POINT <- function(
  obj,
  folder.name = normalizeFilename(deparse(substitute(obj, env = parent.frame()))),
  file.name = paste(folder.name, ".kml", sep = ""),
  size,
  colour,
  points_names,
  shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
  metadata = NULL,
  kmz = get("kmz", envir = plotKML.opts),
  open.kml = TRUE,
  ...
) {
  # Guess aesthetics if missing:
  if (missing(size)) {
    # AG: I have to use obj[[1]] since sf objects have sticky geometry columns
    obj[, "size"] <- obj[[1]]
  } else {
    if (is.name(size) | is.call(size)) {
      obj[, "size"] <- eval(size, obj)
    } else {
      obj[, "size"] <- obj[[deparse(size)]]
    }
  }
  
  if (missing(colour)) {
    # AG: I have to use obj[[1]] since sf objects have sticky geometry columns
    obj[, "colour"] <- obj[[1]]
    message("Plotting the first variable on the list")
  } else {
    if (is.name(colour) | is.call(colour)) {
      obj[, "colour"] <- eval(colour, obj)
    } else {
      obj[, "colour"] <- obj[[as.character(colour)]]
    }
  }
  
  if (missing(points_names)) {
    # AG: I have to use obj[[1]] since sf objects have sticky geometry columns
    if (is.numeric(obj[[1]])) {
      points_names <- signif(obj[[1]], 3)
    } else {
      points_names <- paste(obj[[1]])
    }
  }
  
  # open for writing:
  kml_open(folder.name = folder.name, file.name = file.name)
  
  # write layer:
  if (is.numeric(obj[["colour"]])) {
    # AG: .kml_layer_sfc_POINT is defined in layer-sf.R analogously to
    # kml_layer.SpatialPoints. It is not exported, but I defined a new method
    # for kml_layer function, extending it to sfc_POINT objects, setting
    # definition = .kml_layer_sfc_POINT (analogously to the definition of
    # kml_layer method for SpatialPoints objects).
    .kml_layer_sfc_POINT(
      obj,
      size = size,
      colour = colour,
      points_names = points_names,
      shape = shape,
      metadata = metadata,
      ...
    )
  } else {
    .kml_layer_sfc_POINT(
      obj,
      colour = colour,
      points_names = points_names,
      shape = shape,
      metadata = metadata,
      ...
    )
  }
  
  # close the file:
  kml_close(file.name = file.name)
  if (kmz) {
    kml_compress(file.name = file.name)
  }
  # open KML file in the default browser:
  if (open.kml) {
    kml_View(file.name)
  } else {
    message(paste("Object written to:", file.name))
  }
}

# end of script;
