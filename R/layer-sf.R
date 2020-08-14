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
  if (!sf::st_is_longlat(obj)) {
    obj <- sf::st_transform(obj, crs = get("ref_CRS", envir = plotKML.opts))
    # AG: Maybe here we should consider redefine ref_CRS according to PROJ/GDAL
    # versions (i.e. proj4string or EPSG)
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
  if (!sf::st_is_longlat(obj)) {
    obj <- sf::st_transform(obj, crs = get("ref_CRS", envir = plotKML.opts))
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

setMethod("kml_layer", "sfc_POINT", .kml_layer_sfc_POINT)
setMethod("kml_layer", "sfc_LINESTRING", .kml_layer_sfc_LINESTRING)

# end of script;
