\name{bigfoot}
\docType{data}
\alias{bigfoot}
\alias{USAWgrids}
\title{Bigfoot reports (USA)}
\description{2984 observations of bigfoot (with attached dates). The field occurrence records have been obtained from the \href{http://www.bfro.net/news/google_earth.asp}{BigFoot Research Organization (BFRO) website}. The BFRO reports generally consist of a description of the event and where it occurred, plus the quality classification. Similar data set has been used by Lozier et al. (2009) to demonstrate possible miss-interpretations of the results of species distribution modeling. The maps in the \code{USAWgrids} data set represent typical gridded environmental covariates used for species distribution modeling.
}
\format{
The \code{bigfoot} data frame contains the following columns:
  \describe{
	\item{\code{Lon}}{a numeric vector; x-coordinate / longitude in the WGS84 system}
	\item{\code{Lat}}{a numeric vector; y-coordinate / latitude in the WGS84 system}
	\item{\code{NAME}}{name assigned by the observer (usually referent month / year)}
	\item{\code{DATE}}{'POSIXct' class vector}
	\item{\code{TYPE}}{confidence levels; according to the BFRO website: \emph{"Class A" reports involve clear sightings in circumstances where misinterpretation or misidentification of other animals can be ruled out with greater confidence; "Class B" and "Class C" reports are less credible}.}
  }
The \code{USAWgrids} data frame (46,018 pixels; Washington, Oregon, Nevada and California state) contains the following columns:
  \describe{
	\item{\code{globedem}}{a numeric vector; elevations from the ETOPO1 Global Relief Model}
	\item{\code{nlights03}}{an integer vector; lights at night image for 2003 (Version 2 DMSP-OLS Nighttime Lights Time Series)}
	\item{\code{sroads}}{a numeric vector; distance to main roads and railroads (National Atlas of the United States)}
	\item{\code{gcarb}}{a numeric vector; Global Biomass Carbon Map (New IPCC Tier-1 Global Biomass Carbon Map for the Year 2000)}
	\item{\code{dTRI}}{a numeric vector; density of pollutant releases (North American Pollutant Releases and Transfers database)}
	\item{\code{twi}}{a numeric vector; Topographic Wetness Index based on the globedem}
	\item{\code{states}}{an integer vector; USA states}
	\item{\code{globcov}}{land cover classes based on the MERIS FR images (GlobCover Land Cover version V2.2)}
	\item{\code{s1}}{a numeric vector; x-coordinates in the Albers equal-area projection system}
  \item{\code{s2}}{a numeric vector; y-coordinates in the Albers equal-area projection system}
}
} 
\usage{data(bigfoot)}
\author{ Tomislav Hengl }
\references{
\itemize{
\item Lozier, J.D., Aniello, P., Hickerson, M.J., (2009) Predicting the distribution of Sasquatch in western North America: anything goes with ecological niche modelling. Journal of Biogeography, 36(9):1623-1627. \doi{10.1111/j.1365-2699.2009.02152.x}
\item BigFoot Research Organization (\url{http://www.bfro.net})
}
}
\note{According to the \href{https://newsfeed.time.com/2011/10/11/scientists-95-sure-bigfoot-lives-in-russian-tundra/}{Time.com}, a team of a dozen-plus experts from as far afield as Canada and Sweden have proclaimed themselves 95 percent certain of the mythical animal's existence on Kemerovo region territory some 3,000 kilometers east of Moscow (announced at the Tashtagol conference in 2011).
}
\examples{
\dontrun{# Load the BFRO records:
library(sp)
data(bigfoot)
aea.prj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 
+x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
library(sp)
coordinates(bigfoot) <- ~Lon+Lat
proj4string(bigfoot) <- CRS("+proj=latlon +datum=WGS84")
library(rgdal)
bigfoot.aea <- spTransform(bigfoot, CRS(aea.prj))
# Load the covariates:
data(USAWgrids)
gridded(USAWgrids) <- ~s1+s2
proj4string(USAWgrids) <- CRS(aea.prj)
# Visualize data:
data(SAGA_pal)
pnts <- list("sp.points", bigfoot.aea, pch="+", col="yellow")
spplot(USAWgrids[2], col.regions=rev(SAGA_pal[[3]]), sp.layout=pnts)
}
}
\keyword{datasets}
