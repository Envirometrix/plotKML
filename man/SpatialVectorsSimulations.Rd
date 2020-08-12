\name{SpatialVectorsSimulations-class}
\docType{class}
\alias{SpatialVectorsSimulations-class}
\alias{plotKML,SpatialVectorsSimulations}
\title{A class for spatial simulations containing equiprobable line, point or polygon features}
\description{A class containing input and output maps generated as equiprobable simulations of the same discrete object (for example multiple realizations of stream networks). Objects of this type can be directly visualized in Google Earth by using the \code{\link{plotKML-method}}.}
\section{Slots}{
  \describe{    
    \item{\code{realizations}:}{object of class \code{"list"}; multiple realizations of the same feature e.g. multiple realizations of stream network}
    \item{\code{summaries}:}{object of class \code{"SpatialGridDataFrame"}; summary measures}
  }
}
\section{Methods}{
  \describe{
	\item{plotKML}{\code{signature(obj = "SpatialVectorsSimulations")}: plots simulated vector objects and summaries (grids) in Google Earth}
  }
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{RasterBrickSimulations-class}}, \code{\link{plotKML-method}}
}
\examples{ 
## load a list of equiprobable streams:
data(barstr)
data(bargrid)
library(sp)
coordinates(bargrid) <- ~ x+y
gridded(bargrid) <- TRUE
## output topology:
cell.size = bargrid@grid@cellsize[1]
bbox = bargrid@bbox
nrows = round(abs(diff(bbox[1,])/cell.size), 0) 
ncols = round(abs(diff(bbox[2,])/cell.size), 0)
gridT = GridTopology(cellcentre.offset=bbox[,1], cellsize=c(cell.size,cell.size), 
  cells.dim=c(nrows, ncols))
\dontrun{## derive summaries (observed frequency and the entropy or error):
bar_sum <- count.GridTopology(gridT, vectL=barstr[1:5]) 
## NOTE: this operation can be time consuming!
## plot the whole project and open in Google Earth:
plotKML(bar_sum, grid2poly = TRUE)
}
}
\keyword{classes}
