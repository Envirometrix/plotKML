\name{SpatialMaxEntOutput-class}
\docType{class}
\alias{SpatialMaxEntOutput-class}
\title{A class for outputs of analysis produced using the dismo package (MaxEnt)}
\description{A class containing input and output data produced by running the \code{maxent} (Maximum Entropy) species distribution modeling algorithm. Object of this class can be directly visualized in Google Earth by using the \code{\link{plotKML-method}}.}
\section{Slots}{
  \describe{
    \item{\code{sciname}:}{object of class \code{"character"}; vector of species name compatible with the \code{rgbif} package; usually latin "genus" and "species" name}
    \item{\code{occurrences}:}{object of class \code{"SpatialPoints"}; occurrence-only records}
    \item{\code{TimeSpan.begin}:}{object of class \code{"POSIXct"}; begin of the sampling period}
    \item{\code{TimeSpan.end}:}{object of class \code{"POSIXct"}; end of the sampling period}
    \item{\code{maxent}:}{object of class \code{"MaxEnt"} (species distribution model); produced as an output of the \code{dismo::maxent} function or similar}
    \item{\code{sp.domain}:}{object of class \code{"Spatial"} (ideally \code{"SpatialPolygonsDataFrame"} or \code{"SpatialPixelsDataFrame"}); assumed spatial domain that can be set by the user or it will be estimated by MaxEnt (see examples below)}
    \item{\code{predicted}:}{object of class \code{"RasterLayer"}; contains results of prediction produced using the MaxEnt software}
  }
}
\section{Methods}{
  \describe{
	\item{plotKML}{\code{signature(obj = "SpatialMaxEntOutput")}: plots all MaxEnt output objects in Google Earth}
  }
}
\note{MaxEnt requires the \code{maxent.jar} file to be in the 'java' folder of the dismo package (see: \code{system.file("java", package="dismo")}). For more info refer to the dismo package documentation. Alternatively use the \code{maxlike} package (Royle et al. 2012), which does not require Java.
}
\references{
\itemize{
\item Hijmans, R.J, Elith, J., (2012) \href{https://CRAN.R-project.org/package=dismo}{Species distribution modeling with R}. CRAN, Vignette for the dismo package, 72 p. 
\item Royle, J.A., Chandler, R.B., Yackulic, C. and J. D. Nichols. (2012) \href{http://dx.doi.org/10.1111/j.2041-210X.2011.00182.x}{Likelihood analysis of species occurrence probability from presence-only data for modelling species distributions}. Methods in Ecology and Evolution.
\item dismo package (\url{https://CRAN.R-project.org/package=dismo})
\item maxlike package (\url{https://CRAN.R-project.org/package=maxlike})
\item rgbif package (\url{https://CRAN.R-project.org/package=rgbif})
}
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{plotKML-method}}, \code{dismo::maxent}, \code{maxlike::maxlike}, \code{rgbif::taxonsearch} 
}
\keyword{classes}