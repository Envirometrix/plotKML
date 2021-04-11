\name{SpatialPredictions-class}
\docType{class}
\alias{SpatialPredictions-class}
\alias{plot.SpatialPredictions}
\alias{plot,SpatialPredictions-method}
\title{A class for spatial predictions produced using \code{gstat} package}
\description{A class containing input and output maps generated through the process of geostatistical mapping. Object of this class can be directly visualized in Google Earth by using the \code{\link{plotKML-method}}.}
\section{Slots}{
  \describe{
    \item{\code{variable}:}{object of class \code{"character"}; variable name}
    \item{\code{observed}:}{object of class \code{"SpatialPointsDataFrame"} (must be 2D); see \code{sp::SpatialPointsDataFrame} }
    \item{\code{regModel.summary}:}{contains the summary of the regression model}
    \item{\code{vgmModel}:}{object of class \code{"data.frame"}; contains the variogram parameters passed from gstat}
    \item{\code{predicted}:}{object of class \code{"SpatialPixelsDataFrame"}; see \code{sp::SpatialPixelsDataFrame}}
    \item{\code{validation}:}{object of class \code{"SpatialPointsDataFrame"} containing results of validation}
  }
}
\section{Methods}{
  \describe{
  \item{plot}{\code{signature(x = "SpatialPredictions")}: spatial predictions, regression model (observed vs predicted), original variogram and variogram for residuals}
	\item{plotKML}{\code{signature(obj = "SpatialPredictions")}: plots all objects in Google Earth}
	\item{summary}{\code{signature(obj = "SpatialPredictions")}: summarize object by showing the mapping accuracy (cross-validation) and the amount of variation explained by the model }
  }
}
\references{
\itemize{
\item Hengl, T. (2009) \href{http://spatial-analyst.net/book/}{A Practical Guide to Geostatistical Mapping}, 2nd Edt. University of Amsterdam, www.lulu.com, 291 p.
\item Hengl, T., Nikolic, M., MacMillan, R.A., (2012) Mapping efficiency and information content. International Journal of Applied Earth Observation and Geoinformation, special issue Spatial Statistics Conference. \doi{10.1016/j.jag.2012.02.005}
}
}
\note{ \code{"SpatialPredictions"} saves results of predictions for a single target variable, which can be of type numeric or factor. Multiple variables can be combined into a list.
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{plotKML-method}}, \code{gstat::gstat-class}, \code{\link{RasterBrickSimulations-class}}
}
\keyword{classes}
