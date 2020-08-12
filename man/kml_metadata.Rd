\name{kml_metadata-methods}
\alias{kml_metadata}
\docType{methods}
\alias{kml_metadata,SpatialMetadata-method}
\title{Add metadata table to the active layer}
\description{Adds a selection of metadata to the description box of an active layer.}
\usage{
\S4method{kml_metadata}{SpatialMetadata}(obj, cwidth = 150, twidth = 500, asText = FALSE)
}
\arguments{
  \item{obj}{object of class \code{"SpatialMetadata"}}
  \item{cwidth}{html column width for the field names}
  \item{twidth}{html total table width}
  \item{asText}{logical; return the output as XML or characters}
}
\details{The \code{kml_metadata} function, by default, prints out only a number of selected metadata fields: 
\enumerate{
 \item \code{"Citation_title"},
 \item \code{"Abstract"},
 \item \code{"Object_Count"},
 \item \code{"Beginning_Date"},
 \item \code{"Ending_Date"},
 \item \code{"Data_Order_URL"},
 \item \code{"Other_Citation_Details"},
 \item \code{"Citation_URL"},
 \item \code{"Data_Set_Credit"},
 \item \code{"Data_Distributing_Organization"},
 \item \code{"Format_Information_Content"}, 
 \item \code{"Native_Data_Set_Environment"} 
}
See \code{data(mdnames)} for a complete list of metadata fields. }
\author{ Tomislav Hengl }
\seealso{ \code{\link{spMetadata}}}
\keyword{methods}
