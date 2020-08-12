\name{whitening}
\alias{whitening}
\title{whitening}
\description{Derives a \emph{`whitenned'} color based on the Hue-Saturation-Intensity color model. This method can be used to visualize uncertainty: the original color is \emph{leached} proportionally to the uncertainty (white color indicates maximum uncertainty).}
\usage{
whitening(z, zvar, zlim = c(min(z, na.rm=TRUE), max(z, na.rm=TRUE)),  
  elim = c(.4,1), global.var = var(z, na.rm=TRUE), col.type = "RGB")
}
\arguments{
  \item{z}{numeric; target variable (e.g. predicted values)}
  \item{zvar}{numeric; prediction error (variance)}
  \item{zlim}{upper and lower limits for target variable}
  \item{elim}{upper and lower limits for the normalized error}
  \item{global.var}{global variance (either estimated from the data or specified)}
  \item{col.type}{characted; \code{"RGB"} or \code{"HEX"}}
}
\details{The HSI is a psychologically appealing color model for visualization of uncertainty: hue is used to visualize values and \emph{whitening} (\emph{paleness} or \emph{leaching} percentage) is used to visualize the uncertainty, or in other words the map is incomplete in the areas of high uncertainty. Unlike standard legends for continuous variables, this legend has two axis --- one for value range and one for uncertainty range (see also \code{\link{kml_legend.whitening}}).\cr
The standard range for \code{elim} is 0.4 and 1.0 (maximum). This assumes that a satisfactory prediction is when the model explains more than 85\% of the total variation (normalized error = 40\%). Otherwise, if the value of the normalized error get above 80\%, the model accounts for less than 50\% of variability.\cr
Whitening is of special interest for visualization of the prediction errors in geostatistics. Formulas to derive the whitening color are explained in \href{http://dx.doi.org/10.1016/j.geoderma.2003.08.018}{Hengl et al. (2004)}. }
\references{
\itemize{
\item Hengl, T., Heuvelink, G.M.B., Stein, A., (2004) \href{http://dx.doi.org/10.1016/j.geoderma.2003.08.018}{A generic framework for spatial prediction of soil variables based on regression-kriging}. Geoderma 122 (1-2): 75-93.
\item Hue-Saturation-Intensity color model (\url{http://en.wikipedia.org/wiki/HSL_and_HSV})
}
}
\author{ Tomislav Hengl and Pierre Roudier }
\seealso{ \code{\link{kml_legend.whitening}}
}
\examples{
whitening(z=15, zvar=5, zlim=c(10,20), global.var=7)
# significant color;
whitening(z=15, zvar=5, zlim=c(10,20), global.var=4)
# error exceeds global.var -> totally white;
}
