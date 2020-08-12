\name{SAGA_pal}
\docType{data}
\encoding{latin1}
\alias{color_palettes}
\alias{SAGA_pal}
\alias{R_pal}
\title{Colour palettes for numeric variables}
\description{\code{SAGA_pal} contains 22 colour palettes imported from SAGA GIS (\href{http://webdoc.sub.gwdg.de/diss/2007/conrad/conrad.pdf}{Conrad, 2007}). \code{R_pal} 12 standard colour palettes used in R to visualize continuous and binary variables. Each colour palette consists of 20 colours in the hexadecimal system. Use \code{display.pal} function to plot different sets of palettes.}
\usage{
data(SAGA_pal)
data(R_pal)
}
\author{\href{http://www.saga-gis.org}{SAGA GIS} has been created by the SAGA GIS development team (lead by J. \enc{Böhner}{Boehner} and O. Conrad, from the Institute of Geography, University of Hamburg, Germany). The colour palettes have been exported from SAGA (as \code{".sprm"} SAGA parameter files) and ported to R. All palletes described here were prepared for R by Tomislav Hengl (\email{tom.hengl@opengeohub.org}).}
\references{
\itemize{
\item Conrad, O., (2007). \href{http://webdoc.sub.gwdg.de/diss/2007/conrad/conrad.pdf}{SAGA --- Entwurf, Funktionsumfang und Anwendung eines Systems \enc{für}{fur} Automatisierte Geowissenschaftliche Analysen}. Electronic doctoral dissertation, University of \enc{Göttingen}{Goettingen}.
\item Rogowitz, B.E., Treinish, L.A., (1998, December). \href{http://dx.doi.org/10.1109/6.736450}{Data visualization: the end of the rainbow}. Spectrum, IEEE, 35(12):52-59 
\item Borland, D. and Russell, M. T. II, (2007). \href{http://dx.doi.org/10.1109/MCG.2007.323435}{Rainbow Color Map (Still) Considered Harmful}. Computer Graphics and Applications, IEEE, 27(2):14-17.
\item \url{https://cran.r-project.org/package=RColorBrewer}
\item \url{https://cran.r-project.org/package=colorspace}
}
}
\note{\code{rainbow_75}, \code{heat colors}, \code{terrain_colors}, \code{topo_colors}, and \code{bpy_colors} are the standard color palettes used in R to visualize numeric/continuous variables. \code{soc_pal}, \code{pH_pal}, \code{tex_pal}, \code{BS_pal} and \code{CEC_pal} palettes are suitable for visualization of soil variables (soil organic carbon, pH, soil texture fractions, Base Saturation and Cation Exchange Capacity). \code{blue_grey_red} palette is recommended for visualization of binary variables (values in the range 0-1), and \code{grey_black} is a white-to-black type color palette that contains no white color (hence it will not confuse low values with NA values in the PNG/GIF files).\cr 
Possibly the most used palettes for visualization of numeric variables are \code{rev(rainbow(65)[1:48])} and \code{SAGA_pal[[1]]} (the SAGA GIS default palette). It is however worth mentioning that in the data visualization literature (and the cartography literature in particular), the rainbow (sometimes also called spectral) color ramp is generally recognized as a \emph{bad choice} for visualization of sequential/continuous variables (\href{http://dx.doi.org/10.1109/6.736450}{Rogowitz and Treinish, 1998}; \href{http://dx.doi.org/10.1109/MCG.2007.323435}{Borland and Russell, 2007}).  
}
\seealso{\code{\link{worldgrids_pal}}, \code{RColorBrewer::display.brewer.all}}
\examples{
data(SAGA_pal)
data(R_pal)
\dontrun{# visualize SAGA GIS palettes:
display.pal(pal=SAGA_pal, sel=c(1,2,7,8,10,11,17,18,19,21,22))
dev.off()
display.pal(R_pal)
names(R_pal)
dev.off()
}
}
\keyword{color}
\keyword{datasets}
