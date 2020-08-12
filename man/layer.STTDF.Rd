\name{kml_layer.STTDF}
\encoding{latin1}
\alias{kml_layer.STTDF}
\alias{STTDF}
\title{Write a space-time trajectory to KML}
\description{Writes an object of class \code{"STTDF"} to a KML file with a possibility to parse attribute variables using several aesthetics arguments.
}
\usage{
kml_layer.STTDF(obj, id.name = names(obj@data)[which(names(obj@data)== "burst")],
    dtime, extrude = FALSE, 
    start.icon = paste(get("home_url", envir = plotKML.opts),
         "3Dballyellow.png", sep = ""), 
    end.icon = paste(get("home_url", envir = plotKML.opts), 
          "golfhole.png", sep = ""), 
    LabelScale = 0.8 * get("LabelScale", envir = plotKML.opts), z.scale = 1,
    metadata = NULL, html.table = NULL, \dots )
}
\arguments{
  \item{obj}{space-time object of class \code{"STTDF"} (spatio-temporal irregular data.frames trajectory)}
  \item{id.name}{trajectory ID column name}
  \item{dtime}{temporal support size (in seconds)}
  \item{extrude}{logical; extrude GPS vertices?}
  \item{start.icon}{start icon name (\code{3Dballyellow.png})}
  \item{end.icon}{destination icon name (\code{golfhole.png})}
  \item{LabelScale}{the default size of icons}
  \item{z.scale}{vertical exaggeration}
  \item{metadata}{(optional) specify the metadata object}
  \item{html.table}{optional description block (html) for each GPS point (vertices)}
  \item{\dots}{other optional arguments}
}
\details{The dateTime is defined as \code{yyyy-mm-ddThh:mm:sszzzzzz}, where \code{T} is the separator between the date and the time, and the time zone is either \code{Z} (for UTC) or \code{zzzzzz}, which represents \enc{±}{+-}hh:mm in relation to UTC. For more info on how Time Stamps work see \url{https://developers.google.com/kml/documentation/kml_tut}. If the time is measured at block support, then:\cr 

\code{<TimeStamp><begin> </begin><end> </end></TimeStamp>} \cr

tags will be inserted. Temporal support for any spacetime class, if not specified by the user, is determined as a difference between the \code{"time"} (indicating begin time) and \code{"endTime"} slots.
}
\references{
\itemize{
\item \item Pebesma, E. (2012) \href{http://www.jstatsoft.org/v51/i07}{Classes and Methods for Spatio-Temporal Data in R}. Journal of Statistical Software. 51(7): 1-30.
\item spacetime package (\url{https://CRAN.R-project.org/package=spacetime})
}
}
\author{Tomislav Hengl }
\seealso{ \code{\link{readGPX}}, \code{\link{plotKML-method}}  }
\keyword{spatial}

