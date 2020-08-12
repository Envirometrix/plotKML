\name{eberg}
\docType{data}
\encoding{latin1}
\alias{eberg}
\alias{eberg_grid}
\alias{eberg_grid25}
\alias{eberg_zones}
\alias{eberg_contours}
\title{Ebergotzen --- soil mapping case study}
\description{\enc{Ebergötzen}{Ebergoetzen} is 10 by 10 km study area in the vicinity of the city of \enc{Göttingen}{Goettingen} in Central Germany. This area has been extensively surveyed over the years, mainly for the purposes of developing operational digital soil mapping techniques (Gehrt and \enc{Böhner}{Boehner}, 2001), and has been used by the SAGA GIS development team to demonstrate various processing steps.\cr
\code{eberg} table contains 3670 observations (augers) of soil textures at five depths (0--10, 10--30, 30--50, 50--70, and 70--90), and field records of soil types according to the German soil classification system. \code{eberg_grid} contains gridded maps at 100 m resolution that can be used as covariates for spatial prediction of soil variables. \code{eberg_grid25} contains grids at finer resolution (25 m). \code{eberg_zones} is a polygon map showing the distribution of parent material (Silt and sand, Sandy material, Clayey derivats, Clay and loess). \code{eberg_contours} shows contour lines derived from the 25 m DEM of the area using 10 m equidistance.}
\usage{data(eberg)}
\format{
The \code{eberg} data frame (irregular points) contains the following columns:
  \describe{
	\item{\code{ID}}{universal identifier}
	\item{\code{soiltype}}{a vector containing factors; soil classes according to the German soil classification system: \code{"A"} (Auenboden), \code{"B"} (Braunerde), \code{"D"} (Pelosol), \code{"G"} (Gley), \code{"Ha"} (Moor), \code{"Hw"} (HMoor), \code{"K"} (Kolluvisol), \code{"L"} (Parabraunerde), \code{"N"} (Ranker), \code{"Q"} (Regosol), \code{"R"} (Rendzina), \code{"S"} (Pseudogley), \code{"Z"} (Pararendzina)}
	\item{\code{TAXGRSC}}{a vector containing factors; full soil class names according to the German soil classification system (see \code{soiltype} column)}
	\item{\code{X}}{a numeric vector; x-coordinate (m) in DHDN / Gauss-Krueger zone 3 (German coordinate system) }
	\item{\code{Y}}{a numeric vector; y-coordinate (m) in DHDN / Gauss-Krueger zone 3 (German coordinate system) }
	\item{\code{UHDICM_*}}{a numeric vector; upper horizon depth in cm per horizon}
	\item{\code{LHDICM_*}}{a numeric vector; lower horizon depth in cm per horizon}
	\item{\code{SNDMHT_*}}{a numeric vector; sand content estimated by hand per horizon (0-100 percent); see Ad-hoc-AG Boden (2005) for more details}
	\item{\code{SLTMHT_*}}{a numeric vector; silt content estimated by hand per horizon (0-100 percent)}
	\item{\code{CLYMHT_*}}{a numeric vector; clay content estimated by hand per horizon (0-100 percent)}
} 
The \code{eberg_grid} data frame (regular grid at 100 m resolution) contains the following columns:
  \describe{
  \item{\code{PRMGEO6}}{a vector containing factors, parent material classes from the geological map (mapping units)}
  \item{\code{DEMSRT6}}{a numeric vector; elevation values from the SRTM DEM}
  \item{\code{TWISRT6}}{a numeric vector; Topographic Wetness Index derived using the SAGA algorithm}
  \item{\code{TIRAST6}}{a numeric vector; Thermal Infrared (TIR) reflection values from the ASTER L1 image band 14 (2010-06-05T10:26:50Z) obtained via the NASA's \href{http://glovis.usgs.gov}{GloVis browser}}
  \item{\code{LNCCOR6}}{a vector containing factors; \href{http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1}{Corine Land Cover 2006} classes}
	\item{\code{x}}{a numeric vector; x-coordinate (m) in DHDN / Gauss-Krueger zone 3 (German coordinate system)}
	\item{\code{y}}{a numeric vector; y-coordinate (m) in DHDN / Gauss-Krueger zone 3 (German coordinate system)} 
}
The \code{eberg_grid25} data frame (regular grid at 25 m resolution) contains the following columns:
  \describe{
  \item{\code{DEMTOPx}}{a numeric vector; elevation values from the topographic map}
  \item{\code{HBTSOLx}}{a vector containing factors; main soil type according to the German soil classification system (see column "soiltype" above) estimated per crop field} 
  \item{\code{TWITOPx}}{a numeric vector; Topographic Wetness Index derived using the SAGA algorithm }
  \item{\code{NVILANx}}{a numeric vector; NDVI image derived using the Landsat image from the \href{https://ec.europa.eu/jrc/en}{Image 2000 project}}
	\item{\code{x}}{a numeric vector; x-coordinate (m) in DHDN / Gauss-Krueger zone 3 (German coordinate system) }
	\item{\code{y}}{a numeric vector; y-coordinate (m) in DHDN / Gauss-Krueger zone 3 (German coordinate system) } 
} 
} 
\author{The \enc{Ebergötzen}{Ebergoetzen} dataset is courtesy of Gehrt Ernst (\email{Ernst.Gehrt@niedersachsen.de}), the State Authority for Mining, Energy and Geology, Hannover, Germany and Olaf Conrad, University of Hamburg (\email{conrad@geowiss.uni-hamburg.de}). The original data set has been prepared for this exercise by Tomislav Hengl (\email{tom.hengl@opengeohub.org}).}
\references{
\itemize{ 
\item Ad-hoc-AG Boden, (2005) Bodenkundliche Kartieranleitung. 5th Ed, Bundesanstalt \enc{für}{fur} Geowissenschaften und Rohstoffe und Niedersaechsisches Landesamt \enc{für}{fur} Bodenforshung, Hannover, p. 423.
\item \enc{Böhner}{Boehner}, J., McCloy, K. R. and Strobl, J. (Eds), (2006) \href{http://www.saga-gis.org/}{SAGA --- Analysis and Modelling Applications}. \enc{Göttinger}{Goettinger} Geographische Abhandlungen, Heft 115. Verlag Erich Goltze GmbH, \enc{Göttingen}{Goettingen}, 117 pp.
\item Gehrt, E., \enc{Böhner}{Boehner}, J., (2001) Vom punkt zur flache --- probleme des 'upscaling' in der bodenkartierung. In: Diskussionsforum Bodenwissenschaften: Vom Bohrstock zum Bildschirm. FH, \enc{Osnabrück}{Osnabruck}, pp. 17-34.
\item Skaggs, T. H., Arya, L. M., Shouse, P. J., Mohanty, B. P., (2001) \href{https://www.soils.org/publications/sssaj/articles/65/4/1038}{Estimating Particle-Size Distribution from Limited Soil Texture Data}. Soil Science Society of America Journal 65 (4): 1038-1044.
\item \url{http://geomorphometry.org/content/ebergotzen}
}
}
\note{Texture by hand method can be used to determine the content of soil earth fractions only to an accuracy of \enc{±}{+-}5--10\% (\href{https://www.soils.org/publications/sssaj/articles/65/4/1038}{Skaggs et al. 2001}). A surveyor distinguishes to which of the 32 texture classes a soil samples belongs to, and then estimates the content of fractions; e.g. texture class \code{St2} has 10\% clay, 25\% silt and 65\% sand (Ad-hoc-AG Boden, 2005).}
\examples{
data(eberg)
data(eberg_grid)
data(eberg_zones)
data(eberg_contours)
library(sp)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
# visualize the maps:
data(SAGA_pal)
l.sp <- list("sp.lines", eberg_contours, col="black")
\dontrun{
spplot(eberg_grid["DEMSRT6"], col.regions = SAGA_pal[[1]], sp.layout=l.sp)
spplot(eberg_zones, sp.layout=list("sp.points", eberg, col="black", pch="+"))
}
}
\keyword{datasets}
