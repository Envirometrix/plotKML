
kml_altitude <- function(obj, altitude = NULL) {

  # Testing what has been given by the user
  if (is.character(altitude)) {
    # The character describes the name of a column
    altitude <- obj[[altitude]]
  }
  else if (is.numeric(altitude)) {
    # If it is numeric this is a single altitude for all points
    altitude <- rep(altitude, length.out = length(obj))
  }
  else if (is.null(altitude)) {
    altitude <- rep(.all_kml_aesthetics[["altitude"]], length.out = length(obj))
  }
  else
    stop("Incorrect altitude value")

  altitude
}

# Guesses the appropriate altitudeMode tag
kml_altitude_mode <- function(altitude, GroundOverlay=FALSE){
  
  altitude = as.numeric(altitude)
  if (all(altitude[!is.na(altitude)] > 0)) {
    if(GroundOverlay == TRUE){
      altitude_mode <- "absolute"
    } else {
      altitude_mode <- "relativeToGround"
    }

  }
  else {
    altitude_mode <- "clampToGround"
  }
  altitude_mode
}

# end of script;