# Note: for more info see [http://cran.r-project.org/doc/manuals/R-exts.html];

.onLoad <- function(libname, pkgname) {
  data("SAGA_pal", "R_pal", package=pkgname, envir=parent.env(environment()))
}

.onAttach <- function(libname, pkgname)  {
  
  ## print on start-up:
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkgname,  lib.loc=libname), fields=c("Version","Date")))
	packageStartupMessage(paste(pkgname, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")", sep=""))

	tst <- try( removeTmpFiles(), silent=TRUE )

  ## create env variables:
  plotKML.env(show.env = FALSE, silent = TRUE)
  metadata.env()
  
  packageStartupMessage("URL: https://github.com/Envirometrix/plotKML/")

  return(invisible(0))
  	
}

# end of script;