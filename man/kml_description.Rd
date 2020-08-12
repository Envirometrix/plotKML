\name{kml_description}
\alias{kml_description}
\title{Generate a table description from a data frame}
\description{Converts a two-column data frame to a table in HTML format. This can then parsed to a KML file as the layer description.}
\usage{
kml_description(x, iframe = NULL, caption = "Object summary", 
              fix.enc = TRUE, cwidth = 150, twidth = 300, 
              delim.sign = "_", asText = FALSE)
}
\arguments{
  \item{x}{object of class \code{"data.frame"} with two columns}
  \item{iframe}{(optional) iframe content}
  \item{caption}{character; table caption}
  \item{fix.enc}{logical; specify whether to fix encoding}
  \item{cwidth}{numeric; first column width}
  \item{twidth}{numeric; table width}
  \item{delim.sign}{character; delimiter sign}
  \item{asText}{logical; specifies whether to return the formatted table as text or XML}
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{kml-methods}}
}
