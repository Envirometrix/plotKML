# AG: The following function is used to process sf objects with sfc_POINT
# geometry
.kml_layer_sfc_POINT <- function(
  obj,
  subfolder.name = paste(class(obj)),
  extrude = TRUE,
  z.scale = 1,
  LabelScale = get("LabelScale", envir = plotKML.opts),
  metadata = NULL,
  html.table = NULL,
  TimeSpan.begin = "",
  TimeSpan.end = "",
  points_names,
  ...
) {
  # invisible file connection
  kml.out <- get('kml.out', envir = plotKML.fileIO)
  
  # Checking the projection
  # AG: The sp code here used check_projection() and reproject(). AFAICT,
  # check_projection() is used to 
  # 1) check that the input CRS is not NA; 
  # 2) check if CRS of obj is projected or not, and 
  # 3) check that CRS of obj is valid. 
  # reproject() is just a wrapper around spTransform. 
  # I think the following code perform exactly the same operations since the
  # checks on the input CRS are performed by sf::st_crs(). Moreover, AFAIK,
  # proj4strings are getting deprecated
  # (https://www.r-spatial.org/r/2020/03/17/wkt.html), so I wouldn't use the old
  # check_projection code. Moreover, using recent versions of SF/PROJ/GDAL it
  # shows a warning message.
  if (is.na(sf::st_crs(obj)) || is.null(sf::st_crs(obj))) {
    stop("CRS of obj is missing")
  }
  if (!sf::st_is_longlat(obj) || st_crs(obj) != st_crs(4326)) {
    obj <- sf::st_transform(obj, crs = 4326)
    message("Reprojecting to ", get("ref_CRS", envir = plotKML.opts))
  }
  
  # Parsing the call for aesthetics
  aes <- kml_aes(obj, ...)
  
  # Read the relevant aesthetics
  if (missing(points_names)) {
    points_names <- aes[["labels"]]
  }
  colours <- aes[["colour"]]
  shapes <- aes[["shape"]]
  sizes <- aes[["size"]]
  altitude <- aes[["altitude"]]
  altitudeMode <- aes[["altitudeMode"]]
  balloon <- aes[["balloon"]]
  
  # Parse ATTRIBUTE TABLE (for each placemark):
  if (is.null(html.table)) {
    if (
      (balloon | class(balloon) %in% c('character','numeric')) & 
      # The following code generalizes obj@data to sf/sfc objects
      (inherits(obj, "sf") ||  (isS4(obj) && "data" %in% slotNames(obj)))
    ) {
      # .data_sp_sf_sfc generalizes obj@data to sf/sfc objects
      html.table <- .df2htmltable(.data_sp_sf_sfc(obj)) 
    }
  }
  
  # Folder and name of the points folder
  pl1 <- newXMLNode("Folder", parent = kml.out[["Document"]])
  pl2 <- newXMLNode("name", subfolder.name, parent = pl1)
  
  # Insert metadata:
  if (!is.null(metadata)) {
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txt <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txt, parent = pl1)
  }
  message("Writing to KML...")
  
  # Writing points styles
  # =====================
  txts <- sprintf(
    '<Style id="pnt%s"><LabelStyle><scale>%.1f</scale></LabelStyle><IconStyle><color>%s</color><scale>%s</scale><Icon><href>%s</href></Icon></IconStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
    # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
    seq_len(.length_sp_sf(obj)), 
    rep(LabelScale, .length_sp_sf(obj)), 
    colours, 
    sizes, 
    shapes
  )
  parseXMLAndAdd(txts, parent = pl1)
  
  # Writing points coordinates
  # ==========================
  
  # with attributes:
  if (length(html.table) > 0) {
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          when = rep(when, .length_sp_sf(obj)) 
        }
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><description><![CDATA[%s]]></description><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          seq_len(.length_sp_sf(obj)), 
          when, 
          html.table, 
          rep(as.numeric(extrude), .length_sp_sf(obj)), 
          rep(altitudeMode, .length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )
      } else {
        if (length(TimeSpan.begin) == 1) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.begin = rep(TimeSpan.begin, .length_sp_sf(obj))
        }
        if (length(TimeSpan.end) == 1) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.end = rep(TimeSpan.end, .length_sp_sf(obj)) 
        }
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><description><![CDATA[%s]]></description><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          seq_len(.length_sp_sf(obj)), 
          TimeSpan.begin, 
          TimeSpan.end, 
          html.table, 
          rep(as.numeric(extrude), .length_sp_sf(obj)), 
          rep(altitudeMode, .length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )      
      }
    } else {
      txtc <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><description><![CDATA[%s]]></description><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
        points_names, 
        # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
        seq_len(.length_sp_sf(obj)), 
        html.table, 
        rep(as.numeric(extrude), .length_sp_sf(obj)), 
        rep(altitudeMode, .length_sp_sf(obj)), 
        sf::st_coordinates(obj)[, 1], 
        sf::st_coordinates(obj)[, 2], 
        altitude
      )  
    }
  } else {
    # without attributes:
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          when = rep(when, .length_sp_sf(obj))
        }
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          seq_len(.length_sp_sf(obj)),  
          when, 
          rep(as.numeric(extrude), .length_sp_sf(obj)), 
          rep(altitudeMode, .length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )
      } else {
        if (length(TimeSpan.begin) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.begin = rep(TimeSpan.begin, .length_sp_sf(obj))
        }
        if (length(TimeSpan.end) == 1L) {
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          TimeSpan.end = rep(TimeSpan.end, .length_sp_sf(obj))
        }      
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
          points_names, 
          # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
          seq_len(.length_sp_sf(obj)), 
          TimeSpan.begin, 
          TimeSpan.end, 
          rep(as.numeric(extrude), .length_sp_sf(obj)), 
          rep(altitudeMode, .length_sp_sf(obj)), 
          sf::st_coordinates(obj)[, 1], 
          sf::st_coordinates(obj)[, 2], 
          altitude
        )    
      }     
    } else {
      txtc <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%.5f,%.5f,%.0f</coordinates></Point></Placemark>', 
        points_names, 
        # .length_sp_sf generalizes length to sf/sfc objects (i.e. nrow)
        seq_len(.length_sp_sf(obj)), 
        rep(as.numeric(extrude), .length_sp_sf(obj)), 
        rep(altitudeMode, .length_sp_sf(obj)), 
        sf::st_coordinates(obj)[, 1], 
        sf::st_coordinates(obj)[, 2], 
        altitude
      )      
    }
  }
  
  parseXMLAndAdd(txtc, parent = pl1)
  
  # save results: 
  assign('kml.out', kml.out, envir = plotKML.fileIO)
}

# AG: The following function is used to process sf objects with sfc_LINESTRING
# geometry
.kml_layer_sfc_LINESTRING <- function(
  obj,
  subfolder.name = paste(class(obj)),
  extrude = FALSE,
  z.scale = 1,
  metadata = NULL,
  html.table = NULL,
  TimeSpan.begin = "",
  TimeSpan.end = "",
  ...
) {
  # invisible file connection
  kml.out <- get("kml.out", envir = plotKML.fileIO)
  
  # Checking the projection
  # AG: The sp code here used check_projection() and reproject(). AFAICT,
  # check_projection() is used to 
  # 1) check that the input CRS is not NA; 
  # 2) check if CRS of obj is projected or not, and 
  # 3) check that CRS of obj is valid. 
  # reproject() is just a wrapper around spTransform. 
  # I think the following code perform exactly the same operations since the
  # checks on the input CRS are performed by sf::st_crs(). Moreover, AFAIK,
  # proj4strings are getting deprecated
  # (https://www.r-spatial.org/r/2020/03/17/wkt.html), so I wouldn't use the old
  # check_projection code. Moreover, using recent versions of SF/PROJ/GDAL it
  # shows a warning message. 
  if (is.na(sf::st_crs(obj)) || is.null(sf::st_crs(obj))) {
    stop("CRS of obj is missing")
  }
  if (!sf::st_is_longlat(obj) || st_crs(obj) != st_crs(4326)) {
    obj <- sf::st_transform(obj, crs = 4326)
    message("Reprojecting to ", get("ref_CRS", envir = plotKML.opts))
  }
  
  # Parsing the call for aesthetics
  aes <- kml_aes(obj, ...)
  
  # Read the relevant aesthetics
  lines_names <- aes[["labels"]]
  colours <- aes[["colour"]]
  widths <- aes[["width"]]
  altitude <- aes[["altitude"]]
  altitudeMode <- aes[["altitudeMode"]]
  balloon <- aes[["balloon"]]
  
  # Parse ATTRIBUTE TABLE (for each placemark):
  if (
    balloon & 
    # The following code is used to generalize the previous version to sf
    # objects
    (inherits(obj, "sf") ||  (isS4(obj) && "data" %in% slotNames(obj)))
  ) {
    html.table <- .df2htmltable(obj)
  }
  
  message("Writing to KML...")
  # Folder name / name of the points folder
  pl1 = newXMLNode("Folder", parent = kml.out[["Document"]])
  pl2 <- newXMLNode("name", subfolder.name, parent = pl1)
  
  # Insert metadata:
  if (!is.null(metadata)) {
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txtm <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txtm, parent=pl1)
  }
  
  # process lines:
  lv <- length(sf::st_geometry(obj)) 
  # AG: I think the following code is exactly the same as the sp code. The only
  # difference is that I used sf::st_coordinates() instead of slot(...,
  # "coords")
  coords <- NULL
  for (i.line in seq_len(lv)) {
    xyz <- matrix(sf::st_coordinates(obj[i.line, ])[, 1:2], ncol = 2)
    xyz <- cbind(xyz, rep(altitude[i.line], nrow(xyz)))
    coords[[i.line]] <- paste(xyz[, 1], ',', xyz[, 2], ',', xyz[, 3], collapse='\n ', sep = "")
  }
  
  # Line styles
  # ============
  txts <- sprintf(
    '<Style id="line%s"><LineStyle><color>%s</color><width>%.0f</width></LineStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
    seq_len(lv), 
    colours, 
    widths
  )
  parseXMLAndAdd(txts, parent=pl1)
  
  # Writing lines
  # =============
  
  # with attributes:
  if (length(html.table) > 0) {
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L){
          when = rep(when, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          when, 
          html.table, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      } else {
        if (length(TimeSpan.begin) == 1L) {
          TimeSpan.begin = rep(TimeSpan.begin, lv) 
        }
        if (length(TimeSpan.end) == 1L) {
          TimeSpan.end = rep(TimeSpan.end, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          TimeSpan.begin, 
          TimeSpan.end, 
          html.table, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )    
      }
    } else {      
      txt <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><description><![CDATA[%s]]></description><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
        lines_names, 
        seq_len(lv), 
        html.table, 
        rep(as.numeric(extrude), lv), 
        rep(altitudeMode, lv), 
        paste(unlist(coords))
      )
    }
  } else {
    # without attributes:
    if (nzchar(TimeSpan.begin[1]) & nzchar(TimeSpan.end[1])) {
      if (identical(TimeSpan.begin, TimeSpan.end)) {
        when = TimeSpan.begin
        if (length(when) == 1L) {
          when = rep(when, lv) 
        }
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          when, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      } else {
        if (length(TimeSpan.begin) == 1L){
          TimeSpan.begin = rep(TimeSpan.begin, lv)
        }
        if (length(TimeSpan.end) == 1L){
          TimeSpan.end = rep(TimeSpan.end, lv)
        }   
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
          lines_names, 
          seq_len(lv), 
          TimeSpan.begin, 
          TimeSpan.end, 
          rep(as.numeric(extrude), lv), 
          rep(altitudeMode, lv), 
          paste(unlist(coords))
        )
      }     
    } else {
      txt <- sprintf(
        '<Placemark><name>%s</name><styleUrl>#line%s</styleUrl><LineString><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></LineString></Placemark>', 
        lines_names, 
        seq_len(lv), 
        rep(as.numeric(extrude), lv), 
        rep(altitudeMode, lv), 
        paste(unlist(coords))
      ) 
    }
  }
  
  parseXMLAndAdd(txt, parent=pl1)
  
  # save results: 
  assign('kml.out', kml.out, envir=plotKML.fileIO)
}

.kml_layer_sfc_POLYGON <- function(
  obj,
  subfolder.name = paste(class(obj)),
  extrude = TRUE,
  tessellate = FALSE,
  outline = TRUE,
  plot.labpt = FALSE,
  z.scale = 1,
  LabelScale = get("LabelScale", envir = plotKML.opts),
  metadata = NULL,
  html.table = NULL,
  TimeSpan.begin = "",
  TimeSpan.end = "",
  colorMode = "normal",
  ...
) {
  
  # invisible file connection
  kml.out <- get("kml.out", envir = plotKML.fileIO)
  
  # Checking the projection
  # AG: The sp code here used check_projection() and reproject(). AFAICT,
  # check_projection() is used to 
  # 1) check that the input CRS is not NA; 
  # 2) check if CRS of obj is projected or not, and 
  # 3) check that CRS of obj is valid. 
  # reproject() is just a wrapper around spTransform. 
  # I think the following code perform exactly the same operations since the
  # checks on the input CRS are performed by sf::st_crs(). Moreover, AFAIK,
  # proj4strings are getting deprecated
  # (https://www.r-spatial.org/r/2020/03/17/wkt.html), so I wouldn't use the old
  # check_projection code. Moreover, using recent versions of SF/PROJ/GDAL it
  # shows a warning message.
  if (is.na(sf::st_crs(obj)) || is.null(sf::st_crs(obj))) {
    stop("CRS of obj is missing")
  }
  if (!sf::st_is_longlat(obj) || st_crs(obj) != st_crs(4326)) {
    obj <- sf::st_transform(obj, crs = 4326)
    message("Reprojecting to ", get("ref_CRS", envir = plotKML.opts))
  }
  
  # Parsing the call for aesthetics
  aes <- kml_aes(obj, ...)
  
  # Read the relevant aesthetics
  poly_names <- aes[["labels"]]
  colours <- aes[["colour"]]
  sizes <- aes[["size"]]
  shapes <- aes[["shape"]]
  altitude <- aes[["altitude"]]
  altitudeMode <- aes[["altitudeMode"]]
  balloon <- aes[["balloon"]]
  
  # Parse ATTRIBUTE TABLE (for each placemark):
  # AG: Here I would use is.character(balloon) | is.numeric(balloon)
  if ((balloon | class(balloon) %in% c('character','numeric'))) {
    html.table <- .df2htmltable(obj)
  }
  
  # Folder and name of the points folder
  pl1 <- newXMLNode("Folder", parent = kml.out[["Document"]])
  pl2 <- newXMLNode("name", subfolder.name, parent = pl1)
  
  if (plot.labpt) {
    pl1b <- newXMLNode("Folder", parent = kml.out[["Document"]])
    pl2b <- newXMLNode("name", "labpt", parent = pl1b)
  }
  
  # Insert metadata:
  if (!is.null(metadata)) {
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txt <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txt, parent=pl1)
  }
  message("Writing to KML...")  
  
  # Prepare data for writing
  # ==============
  
  # number of polygons:
  pv <- nrow(obj)
  # number of inner polygons:
  pvn <- lengths(sf::st_geometry(obj))
  # parse coordinates:
  coords <- rep(list(NULL), pv)
  hole <- rep(list(NULL), pv)
  labpt <- rep(list(NULL), pv)
  
  # AG: AFAICT, the idea behind the following code is: 
  # 1) Loop over all the rows in obs
  # 2) For each row, loop over all the polygons/holes
  # 3a) xyz is the object representing the coordinates of each polygon/hole
  # 3b) cxyz is the coordinate of the centroid of that polygon/hole
  # 3c) hole is just a boolean value whether that polygon is a hole or not
  # I think the following code replicate that behaviour. 
  # sf objects do not have any "labpt" slot so I manually estimate the centroid for each POLYGON
  for (i.poly in seq_len(pv)) { 
    for (i.Poly in seq_len(pvn[[i.poly]])) {
      # get coordinates / hole definition:
      # AG: Extract i.Poly (i.e. the POLYGON) from i.poly (the row number)
      my_poly <- sf::st_polygon(list(sf::st_geometry(obj)[[i.poly]][[i.Poly]]))
      # AG: Extract the coordinates of the poly
      poly_coords <- sf::st_coordinates(my_poly)
      id_coords <- which(!dimnames(poly_coords)[[2]] %in% c("L1", "L2"))
      xyz <-poly_coords[, id_coords]
      # xyz <- slot(slot(obj@polygons[[i.poly]], "Polygons")[[i.Poly]], "coords")
      # AG: labpt is the centroid
      # AG: Estimate the centroid of the poly
      cxyz <- sf::st_coordinates(sf::st_centroid(my_poly))[, c(1, 2)]
      # cxyz <- slot(slot(obj@polygons[[i.poly]], "Polygons")[[i.Poly]], "labpt")
      # if altitude is missing, add the default altitudes:
      if (ncol(xyz)==2){  
        xyz <- cbind(xyz, rep(altitude[i.poly], nrow(xyz)))  
      }
      # format coords for writing to KML [https://developers.google.com/kml/documentation/kmlreference#polygon]:
      # AG: I think that, according to sf standards, the first polygon is always
      # the exterior ring
      hole[[i.poly]][[i.Poly]] <- ifelse(i.Poly > 1, TRUE, FALSE)
      coords[[i.poly]][[i.Poly]] <- paste(xyz[,1], ',', xyz[,2], ',', xyz[,3], collapse='\n ', sep = "")
      labpt[[i.poly]][[i.Poly]] <- paste(cxyz[1], ',', cxyz[2], ',', altitude[i.poly], collapse='\n ', sep = "")
    }
  }
  
  # reformatted aesthetics (one "polygons" can have multiple "Polygons"):
  poly_names.l <- list(NULL)
  for (i.poly in seq_len(pv)) {
    poly_names.l[[i.poly]] <- as.vector(rep(poly_names[i.poly], pvn[[i.poly]])) 
  }
  # polygon times (if applicable)
  TimeSpan.begin.l <- list(NULL)
  TimeSpan.end.l <- list(NULL)
  when.l <- list(NULL)
  # check if time span has been defined:
  if (all(nzchar(TimeSpan.begin)) & all(nzchar(TimeSpan.end))) {
    if (identical(TimeSpan.begin, TimeSpan.end)) {
      if (length(TimeSpan.begin) == 1L) {
        when.l <- rep(TimeSpan.begin, sum(unlist(pvn)))
      } else {
        for (i.poly in seq_len(pv)) {
          when.l[[i.poly]] <- as.vector(rep(TimeSpan.begin[i.poly], pvn[[i.poly]]))
        }
      }
    } else {
      for (i.poly in seq_len(pv)) {
        TimeSpan.begin.l[[i.poly]] <- as.vector(rep(TimeSpan.begin[i.poly], pvn[[i.poly]]))
      }
      for (i.poly in seq_len(pv)) {
        TimeSpan.end.l[[i.poly]] <- as.vector(rep(TimeSpan.end[i.poly], pvn[[i.poly]]))
      }
    }
  }         
  
  # Polygon styles
  # ==============
  if (!length(unique(colours)) == 1L | colorMode=="normal") {
    colours.l <- list(NULL)
    for (i.poly in seq_len(pv)) { 
      colours.l[[i.poly]] <- as.vector(rep(colours[i.poly], pvn[[i.poly]])) 
    }    
    txts <- sprintf(
      '<Style id="poly%s"><PolyStyle><color>%s</color><outline>%s</outline><fill>%s</fill></PolyStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
      seq_len(sum(unlist(pvn))), 
      unlist(colours.l), 
      rep(as.numeric(outline), sum(unlist(pvn))), 
      as.numeric(!(unlist(hole)))
    )
    parseXMLAndAdd(txts, parent=pl1)
  } else {
    # random colours:
    txts <- sprintf(
      '<Style id="poly%s"><PolyStyle><colorMode>random</colorMode><outline>%s</outline><fill>%s</fill></PolyStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
      seq_len(sum(unlist(pvn))), 
      rep(as.numeric(outline), sum(unlist(pvn))), 
      as.numeric(!(unlist(hole)))
    )
    parseXMLAndAdd(txts, parent=pl1)
  }
  
  # Point styles
  # ==============
  if (plot.labpt) {
    sizes.l <- list(NULL)
    shapes.l <- list(NULL)
    # reformat size / shapes:
    for (i.poly in seq_len(pv)) {
      sizes.l[[i.poly]] <- as.vector(rep(sizes[i.poly], pvn[[i.poly]]))
    }
    for (i.poly in seq_len(pv)){
      shapes.l[[i.poly]] <- as.vector(rep(shapes[i.poly], pvn[[i.poly]]))
    }    
    txtsp <- sprintf(
      '<Style id="pnt%s"><LabelStyle><scale>%.1f</scale></LabelStyle><IconStyle><color>ffffffff</color><scale>%s</scale><Icon><href>%s</href></Icon></IconStyle><BalloonStyle><text>$[description]</text></BalloonStyle></Style>', 
      seq_len(sum(unlist(pvn))), 
      rep(LabelScale, sum(unlist(pvn))), 
      unlist(sizes.l), 
      unlist(shapes.l)
    )
    parseXMLAndAdd(txtsp, parent = pl1b)
    
    # Writing labpt
    # ================  
    if (all(is.null(unlist(TimeSpan.begin.l))) & all(is.null(unlist(TimeSpan.end.l)))) {
      if (all(is.null(unlist(when.l)))) {
        # time span undefined:
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></Point></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(labpt))
        )
      } else {
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></Point></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          unlist(when.l), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(labpt))
        )  
      } } else{
        # fixed begin/end times:
        txtc <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#pnt%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><Point><extrude>%.0f</extrude><altitudeMode>%s</altitudeMode><coordinates>%s</coordinates></Point></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          unlist(TimeSpan.begin.l), 
          unlist(TimeSpan.end.l), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(labpt))
        )
      }
    
    parseXMLAndAdd(txtc, parent=pl1b)
  }
  # finished writing the labels
  
  # Writing polygons
  # ================
  
  if (length(html.table) > 0) {   
    html.table.l <- list(NULL)
    for (i.poly in seq_len(pv)) { 
      html.table.l[[i.poly]] <- as.vector(rep(html.table[i.poly], pvn[[i.poly]])) 
    }    
    
    # with attributes:
    if (all(is.null(unlist(TimeSpan.begin.l))) & all(is.null(unlist(TimeSpan.end.l)))) {
      if (all(is.null(unlist(when.l)))) {
        # time span undefined:
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#poly%s</styleUrl><description><![CDATA[%s]]></description><Polygon><extrude>%.0f</extrude><tessellate>%.0f</tessellate><altitudeMode>%s</altitudeMode><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          unlist(html.table.l), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(as.numeric(tessellate), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(coords))
        )
      } else { 
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#poly%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><description><![CDATA[%s]]></description><Polygon><extrude>%.0f</extrude><tessellate>%.0f</tessellate><altitudeMode>%s</altitudeMode><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          unlist(when.l), 
          unlist(html.table.l), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(as.numeric(tessellate), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(coords))
        )
      }} else {
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#poly%s</styleUrl><description><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><![CDATA[%s]]></description><Polygon><extrude>%.0f</extrude><tessellate>%.0f</tessellate><altitudeMode>%s</altitudeMode><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          unlist(TimeSpan.begin.l), 
          unlist(TimeSpan.end.l), 
          unlist(html.table.l), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(as.numeric(tessellate), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(coords))
        )
      }
  }
  
  # without attributes:
  else {
    if (all(is.null(unlist(TimeSpan.begin.l))) & all(is.null(unlist(TimeSpan.end.l)))) {
      if (all(is.null(unlist(when.l)))) {
        # time span undefined:
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#poly%s</styleUrl><Polygon><extrude>%.0f</extrude><tessellate>%.0f</tessellate><altitudeMode>%s</altitudeMode><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(as.numeric(tessellate), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(coords))
        )
      } else {
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#poly%s</styleUrl><TimeStamp><when>%s</when></TimeStamp><Polygon><extrude>%.0f</extrude><tessellate>%.0f</tessellate><altitudeMode>%s</altitudeMode><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          unlist(when.l), 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(as.numeric(tessellate), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(coords))
        )  
      }} else {   
        txt <- sprintf(
          '<Placemark><name>%s</name><styleUrl>#poly%s</styleUrl><TimeSpan><begin>%s</begin><end>%s</end></TimeSpan><Polygon><extrude>%.0f</extrude><tessellate>%.0f</tessellate><altitudeMode>%s</altitudeMode><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>', 
          unlist(poly_names.l), 
          seq_len(sum(unlist(pvn))), 
          TimeSpan.begin, 
          TimeSpan.end, 
          rep(as.numeric(extrude), sum(unlist(pvn))), 
          rep(as.numeric(tessellate), sum(unlist(pvn))), 
          rep(altitudeMode, sum(unlist(pvn))), 
          paste(unlist(coords))
        )     
      }
  }
  
  parseXMLAndAdd(txt, parent=pl1)
  
  # save results: 
  assign("kml.out", kml.out, envir=plotKML.fileIO)
  
}

.kml_layer_sf <- function(
  obj, 
  ...
) {
  if (inherits(sf::st_geometry(obj), "sfc_POINT")) {
    .kml_layer_sfc_POINT(obj, ...)
  } else if (inherits(sf::st_geometry(obj), "sfc_MULTIPOINT")) {
    message("Casting the input MULTIPOINT object into POINT.")
    obj <- st_cast(obj, "POINT")
    .kml_layer_sfc_POINT(obj, ...)
  } else if (inherits(sf::st_geometry(obj), "sfc_LINESTRING")) {
    .kml_layer_sfc_LINESTRING(obj, ...)
  } else if (inherits(sf::st_geometry(obj), "sfc_MULTILINESTRING")) {
    message("Casting the input MULTILINESTRING object into LINESTRING.")
    obj <- st_cast(obj, "LINESTRING")
    .kml_layer_sfc_LINESTRING(obj, ...)
  } else if (inherits(sf::st_geometry(obj), "sfc_POLYGON")) {
    .kml_layer_sfc_POLYGON(obj, ...)
  } else if (inherits(sf::st_geometry(obj), "sfc_MULTIPOLYGON")) {
    message("Casting the input MULTIPOLYGON object into POLYGON.")
    obj <- st_cast(obj, "POLYGON")
    .kml_layer_sfc_POLYGON(obj, ...)
  } else {
    stop("There is no method for sf object with geometry not in POINT, LINESTRING, POLYGON")
  }
}

setMethod("kml_layer", "sfc_POINT", .kml_layer_sfc_POINT)
setMethod("kml_layer", "sfc_MULTIPOINT", function(obj, ...) {
  message("Casting the input MULTIPOINT object into POINT.")
  obj <- st_cast(obj, "POINT")
  .kml_layer_sfc_POINT(obj, ...)
})
setMethod("kml_layer", "sfc_LINESTRING", .kml_layer_sfc_LINESTRING)
setMethod("kml_layer", "sfc_MULTILINESTRING", function(obj, ...) {
  message("Casting the input MULTILINESTRING object into LINESTRING.")
  obj <- st_cast(obj, "LINESTRING")
  .kml_layer_sfc_LINESTRING(obj, ...)
})
setMethod("kml_layer", "sfc_POLYGON", .kml_layer_sfc_POLYGON)
setMethod("kml_layer", "sfc_MULTIPOLYGON", function(obj, ...) {
  message("Casting the input MULTIPOLYGON object into POLYGON.")
  obj <- st_cast(obj, "POLYGON")
  .kml_layer_sfc_POLYGON(obj, ...)
})
setMethod("kml_layer", "sf", .kml_layer_sf)

# end of script;
