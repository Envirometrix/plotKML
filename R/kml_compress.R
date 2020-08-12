
kml_compress <- function(file.name, zip = Sys.getenv("R_ZIPCMD", "zip"), files = "", rm = FALSE, ...){

  # Changing the extension to KMZ
  extension <- tools::file_ext(file.name)
  kmz <- stringr::str_replace(file.name, extension, "kmz") # switch the extension to kmz
	
  # use R's zip wrapper
  if(.Platform$OS.type == "windows") {
    try( x <- zip(zipfile = utils::shortPathName(kmz), files= utils::shortPathName(file.name), zip = zip) )
  } else {
    try( x <- zip(zipfile = kmz, files= file.name, zip = zip) )
  }
  # Error handling
  if(is(.Last.value, "try-error")| x==127) {
    if(zip==""|!nzchar(zip)){
       warning("KMZ generation failed. No zip utility has been found.")
  } else {
       warning("KMZ generation failed. Wrong command passed to 'zip = ... option'.")
  }
  }  
  
  # clean-up
  if (file.exists(kmz) & rm==TRUE) {
  	x <- file.remove(file.name, files)
  }

}

# end of script;
