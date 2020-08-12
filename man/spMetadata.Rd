\name{spMetadata-methods}
\docType{methods}
\alias{spMetadata}
\alias{spMetadata,RasterLayer-method}
\alias{spMetadata,Spatial-method}
\alias{spMetadata.Raster}
\alias{spMetadata.Spatial}
\alias{metadata}
\alias{metadata.env}
\alias{read.metadata}
\title{Methods to generate spatial metadata}
\description{
The \code{spMetadata} function will try to generate missing metadata (bounding box, location info, session info, metadata creator info and similar) for any \code{Spatial*} object (from the sp package) or \code{Raster*} object (from the raster package). The resulting object of class \code{\link{SpatialMetadata-class}} can be used e.g. to generate a Layer description documents (\code{<description>} tag).\cr
The \code{read.metadata} function reads the formatted metadata (\code{.xml}), prepared following e.g. the \href{http://www.fgdc.gov/metadata/csdgm/}{Federal Geographic Data Committee (FGDC) Content Standard for Digital Geospatial Metadata} or \href{http://inspire.ec.europa.eu/}{INSPIRE} standard, and converts them to a data frame.
}
\usage{ 
\S4method{spMetadata}{RasterLayer}(obj, bounds, color, \dots )
\S4method{spMetadata}{Spatial}(obj, xml.file, out.xml.file, 
    md.type = c("FGDC", "INSPIRE")[1],
    generate.missing = TRUE, GoogleGeocode = FALSE,
    signif.digit = 3, colour_scale, color = NULL, bounds,
    legend_names, icons, validate.schema = FALSE, \dots)
}
\arguments{
  \item{obj}{some \code{"Spatial"} or \code{"Raster"} class object with \code{"data"} slot}
  \item{xml.file}{character; optional input XML metadata file}
  \item{out.xml.file}{character; optional output XML metadata file}
  \item{md.type}{character; metadata standard \href{http://geology.usgs.gov/tools/metadata/}{FGDC} or \href{http://inspire.ec.europa.eu/}{INSPIRE}}
  \item{generate.missing}{logical; specifies whether to automatically generate missing fields}
  \item{GoogleGeocode}{logical; specifies whether the function should try to use GoogleGeocoding functionality to determine the location name}
  \item{signif.digit}{integer; the default number of significant digits (in the case of rounding)}  
  \item{colour_scale}{the color scheme used to visualize this data}
  \item{color}{character; list of colors (\code{rgb()}) that can be passed instead of using the pallete}  
  \item{bounds}{numeric vector; upper and lower bounds used for visualization}  
  \item{legend_names}{character; legend names in the order of bounds}
  \item{icons}{character; file name or URL used for icons (if applicable)}
  \item{validate.schema}{logical; specifies whether to validate the schema using the \code{xmlSchemaValidate}}
  \item{\dots}{additional arguments to be passed e.g. via the \code{metadata.env()}}
}
\details{
\code{spMetadata} tries to locate a metadata file in the working directory (it looks for a metadata file with the same name as the object name). If no \code{.xml} file exists, it will load the template xml file available in the system folder (e.g. \code{system.file("FGDC.xml", package="plotKML")} or \code{system.file("INSPIRE_ISO19139.xml", package="plotKML")}). The \code{FGDC.xml}/\code{INSPIRE_ISO19139.xml} files contain typical metadata entries with description and examples. For practical purposes, one metadata object in plotKML can be associated with only one variable i.e. one column in the \code{"data"} slot (the first column by default). To prepare a metadata xml file following the FGDC standard, consider using e.g. the \href{http://geology.usgs.gov/tools/metadata/tools/doc/tkme.html}{Tkme} software: Another editor for formal metadata, by Peter N. Schweitzer (U.S. Geological Survey). Before commiting the metadata file, try also running a \href{https://mrdata.usgs.gov/validation/}{validation test}. Before committing the metadata file following the INSPIRE standard, try running the \href{http://inspire.ec.europa.eu/validator/about/}{INSPIRE Geoportal Metadata Validator}.\cr
\code{spMetadata} tries to automatically generate the most usefull information, so that a user can easily find out about the input data and procedures followed to generate the visualization (KML). Typical metadata entries include e.g. (FGDC):
\itemize{
\item \code{metadata[["idinfo"]][["native"]]} --- Session info e.g.: Produced using R version 2.12.2 (2011-02-25) running on Windows 7 x64.
\item \code{metadata[["spdoinfo"]][["indspref"]]} --- Indirect spatial reference estimated using the \href{https://developers.google.com/maps/web-services/}{Google Maps API Web Services}.
\item \code{metadata[["idinfo"]][["spdom"]][["bounding"]]} --- Bounding box in the WGS84 geographical coordinates estimated by reprojecting the original bounding box.
}
and for INSPIRE metadata:
\itemize{
\item \code{metadata[["fileIdentifier"]][["CharacterString"]]} --- Metadata file identifier (not mandatory for INSPIRE-compl.) created by UUIDgenerate from package UUID (version 4 UUID). 
\item \code{metadata[["dateStamp"]][["Date"]]} --- Metadata date stamp created using Sys.Date().
\item \code{metadata[["identificationInfo"]][["MD_DataIdentification"]]
[["extent"]][["EX_Extent"]][["geographicElement"]][["EX_GeographicBoundingBox"]]} --- Bounding box in the WGS84 geographical coordinates estimated by reprojecting the original bounding box.
}
By default, plotKML uses the Creative Commons license, but this can be adjusted by setting the \code{Use_Constraints} argument.
}
\references{
\itemize{
\item The Federal Geographic Data Committee, (2006) FGDC Don't Duck Metadata --- A short reference guide for writing quality metadata. Vers. 1, \url{http://www.fgdc.gov/metadata/documents/MetadataQuickGuide.pdf}
\item Content Standard for Digital Geospatial Metadata (\url{http://www.fgdc.gov/metadata/csdgm/})
\item Tkme metadata editor (\url{http://geology.usgs.gov/tools/metadata/tools/doc/tkme.html})
\item INSPIRE, INS MD, Commission Regulation (EC) No 1205/2008 of 3 December 2008 implementing Directive 2007/2/EC of the European Parliament and of the Council as regards metadata (Text with EEA relevance). See also Corrigendum to INSPIRE Metadata Regulation.
\item INSPIRE, INS MDTG, (2013) INSPIRE Metadata Implementing Rules: Technical Guidelines based on EN ISO 19115 and EN ISO 19119, v1.3
}
}
\author{Tomislav Hengl and Michael Blaschek}
\seealso{ \code{\link{kml_metadata}}, \code{\link{SpatialMetadata-class}}, \code{sp::Spatial}, \code{\link{kml_open}} }
\examples{
\dontrun{
library(sp)
library(uuid)
library(rjson)
## read metadata from the system file:
x <- read.metadata(system.file("FGDC.xml", package="plotKML"))
str(x)
## generate missing metadata
data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
## no metadata file specified:
eberg.md <- spMetadata(eberg["SNDMHT_A"])
## this generates some metadata automatically e.g.:
xmlRoot(eberg.md@xml)[["eainfo"]][["detailed"]][["attr"]]
## combine with localy prepared metadata file:
eberg.md <- spMetadata(eberg["SNDMHT_A"], 
    xml.file=system.file("eberg.xml", package="plotKML"))
## Additional metadat entries can be added by using e.g.:
eberg.md <- spMetadata(eberg["SNDMHT_A"], 
  md.type="INSPIRE", 
  CI_Citation_title = 'Ebergotzen data set',
  CI_Online_resource_URL = 'http://geomorphometry.org/content/ebergotzen')
## the same using the FGDC template:
eberg.md <- spMetadata(eberg["SNDMHT_A"], 
  Citation_title = 'Ebergotzen data set',
  Citation_URL = 'http://geomorphometry.org/content/ebergotzen')
## Complete list of names:
mdnames <- read.csv(system.file("mdnames.csv", package="plotKML"))
mdnames$field.names
## these can be assigned to the "metadata" environment by using:
metadata.env(CI_Citation_title = 'Ebergotzen data set')
get("CI_Citation_title", metadata)

## write data and metadata to a file:
library(rgdal)
writeOGR(eberg["SNDMHT_A"], "eberg_SAND.shp", ".", "ESRI Shapefile")
saveXML(eberg.md@xml, "eberg_SAND.xml")
## export to SLD format:
metadata2SLD(eberg.md, "eberg.sld")
## plot the layer with the metadata:
kml(eberg, file.name = "eberg_md.kml", colour = SNDMHT_A, metadata = eberg.md, kmz = TRUE)
}
}
\keyword{spatial}
\keyword{methods}