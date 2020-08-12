\name{kml_layer.SoilProfileCollection}
\alias{kml_layer.SoilProfileCollection}
\alias{SoilProfileCollection}
\title{Writes a list of soil profiles to KML}
\description{Writes object of type \code{"SoilProfileCollection"} (a number of soil profiles with site and horizon data) to KML. Several attributes such as horizontal and vertical exaggeration can be passed via arguments. }
\usage{
kml_layer.SoilProfileCollection(obj, 
     var.name, var.min = 0, var.scale, 
     site_names = profile_id(obj), 
     method = c("soil_block", "depth_function")[1], 
     block.size = 100, 
     color.name, z.scale = 1, x.min, max.depth = 300,
     plot.points = TRUE, 
     LabelScale = get("LabelScale", envir = plotKML.opts) * 0.7,
     IconColor = "#ff0000ff", 
     shape = paste(get("home_url", envir = plotKML.opts), 
         "circlesquare.png", sep = ""), 
     outline = TRUE, visibility = TRUE, extrude = TRUE, tessellate = TRUE, 
     altitudeMode = "relativeToGround", camera.distance = 0.01, 
     tilt = 90, heading = 0, roll = 0, 
     metadata = NULL, html.table = NULL, plot.scalebar = TRUE,
     scalebar = paste(get("home_url", envir = plotKML.opts), 
         "soilprofile_scalebar.png", sep = ""), 
     \dots )
}
\arguments{
  \item{obj}{object of class \code{"SoilProfileCollection"} (package \href{https://CRAN.R-project.org/package=aqp}{aqp})}
  \item{var.name}{target column name in the \code{horizons} slot}
  \item{var.min}{smallest value}
  \item{var.scale}{exaggeration in vertical dimension}
  \item{site_names}{site names as listed in the site table}
  \item{method}{visualization type (soil block or depth-function)}  
  \item{block.size}{(optional) size of the block of land}  
  \item{color.name}{(optional) column name carrying the color information for each horizon}
  \item{z.scale}{exaggeration in horizontal direction}
  \item{x.min}{offset in longitude direction (in decimal degrees)}  
  \item{max.depth}{maximum height/depht of a profile in cm}
  \item{plot.points}{logical; specifies whether to plot horizon centres with attribute values}
  \item{LabelScale}{numeric; specifies size of the labels for each horizon}
  \item{IconColor}{colors for the labels for each horizon}
  \item{shape}{default icon for Google placemarks} 
  \item{outline}{logical; specifies whether to draw outline for the soil-depth functions (or simply a line)}  
  \item{visibility}{logical; specifies whether to make the layer visible}
  \item{extrude}{logical; specifies whether to extrude horizon centers}
  \item{tessellate}{logical; specifies whether to tessellate polygons}
  \item{altitudeMode}{by default \code{relativeToGround}}    
  \item{camera.distance}{distance from a profile in arc degrees}
  \item{tilt}{angle between the direction of the LookAt position and the normal to the surface of the earth} 
  \item{heading}{orientation towards north} 
  \item{roll}{rotation about the y axis}
  \item{metadata}{(optional) spatial metadata for the input object}
  \item{html.table}{(optional) tabular content (attributes) for each horizon}
  \item{plot.scalebar}{logical; specifies whether to plot a scale bar next to the profile plot}
  \item{scalebar}{default icon for the scale bar}
  \item{\dots}{additional style arguments}
}
\details{Horizon depths are typically expressed in cm, hence the default exaggeration factor (\code{z.scale}) is 10. It is highly recommended to turn off the terrain layer in Google Earth, otherwise Google Earth will deform the plots in areas of high relief. }
\note{The spatial exaggeration needs to be used because often the detail in the background imagery in Google Earth is limited to a spatial accuracy of 2--20 m, hence there is no point of zooming into objects of size of few meters. These exaggeration factors were selected empirically and will need to be adjusted as the detail in the background imagery increases.
}
\references{
\itemize{
\item Algorithms for Quantitative Pedology (\url{https://CRAN.r-project.org/package=aqp})
}
}
\author{Tomislav Hengl, Dylan Beaudette and Pierre Roudier}
\seealso{ \code{\link{kml_layer.SpatialPhotoOverlay}}, \code{\link{plotKML-method}} }
\examples{
\dontrun{## install.packages("aqp", repos="http://R-Forge.R-project.org")
library(aqp)
library(fossil)
library(plyr)
data(ca630)
## Promote to SoilProfileCollection
ca <- join(ca630$lab, ca630$site, type='inner')
depths(ca) <- pedon_key ~ hzn_top + hzn_bot
## extract site data
site(ca) <- ~ mlra + ssa + lon + lat + cntrl_depth_to_top + cntrl_depth_to_bot + sampled_taxon_name
# generate SpatialPoints
library(sp)
coordinates(ca) <- ~ lon + lat
## assign CRS data
proj4string(ca) <- "+proj=longlat +datum=NAD83"
## plot changes in base saturation by sum of cations method (pH 8.2):
kml(ca, method = "depth_function", file.name = "ca_bs_8_2.kml", 
  var.name="bs_8.2", balloon = TRUE)
## plot changes in cation exchange capacity by sum of cations method (pH 8.2):
kml(ca, file.name = "ca_CEC8_2.kml", var.name="CEC8.2", IconColor = "#ff009000")
## plot soil profile as 'block':
kml(ca, file.name = "ca_CEC8_2_block.kml", var.name="CEC8.2", balloon = TRUE)
}
}
\keyword{spatial}


