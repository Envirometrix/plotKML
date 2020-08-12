\name{worldgrids_pal}
\alias{worldgrids_pal}
\docType{data}
\encoding{latin1}
\title{Standard global color palettes for factor variables}
\description{
A number of color palettes used to visualize various environmental categorical / factor variables: land cover classes, water types, anthroms, soil types and similar. Each colour palette consists of a variable number of colours (hexadecimal system). Factor levels names are attached as attributes to the palette.
}
\format{
  The list contains:
  \describe{
    \item{\code{anthroms}}{Color palette used for the global map of anthroms (Ellis and Ramankutty, 2008).}
    \item{\code{bodemfgr}}{A simplified color palette for soil types.}
     \item{\code{corine2k}}{Color palette used in the Corine 2000 project for land cover classes (\enc{Büttner}{Buttner}, et al., 2002).}
     \item{\code{glc2000}}{Color palette used for the Global Land Cover 2000 mapping project (Global Land Cover 2000).}
     \item{\code{globcov}}{Color palette used for the ENVISAT-based Global Land Cover map at resolution of 300 m (GlobCover Land Cover version V2.2).}
     \item{\code{gtkaart}}{Color palette used for the Ground water levels map of the Netherlands (Gaast et al. 2005).}
     \item{\code{IGBP}}{Color palette for 17 land cover classes defined by the International Geosphere Biosphere Programme (IGBP).}
     \item{\code{lgn3}}{Color palette used for the Dutch land use map (Hazeu, 2005).}
     \item{\code{t250vlak}}{Color palette used for the most general land use classes at scale 1:250k (TOP250NL).}
     \item{\code{watert}}{Color palette used for the water types (generalized) in the Netherlands.}
  }
}
\usage{
data(worldgrids_pal)
}
\author{Tomislav Hengl }
\references{
\itemize{
\item Bicheron, P. et al. (2008) GLOBCOVER: Products Description and Validation Report. MEDIAS France, Toulouse, 47 p.
\item \enc{Büttner}{Buttner}, G. et al. (2002) Corine Land Cover update 2000, Technical guidelines. EEA (European Environment Agency), Kopenhagen. 
\item Ellis, E.C., Ramankutty, N. (2008) Putting people in the map: anthropogenic biomes of the world. Frontiers in Ecology and the Environment, Vol. 6, No. 8, pp. 439-447.
\item Fritz, S. et al. (2003) Harmonisation, mosaicing and production of the Global Land Cover 2000 database. JRC report EUR 20849 EN, Luxembourg, 41 p.
\item Gaast, J.W.J. van der, H.R.J. Vroon en M. Pleijter, (2006) De grondwaterdynamiek in het waterschap Regge en Dinkel. Wageningen, Alterra-rapport 1335.
\item Hazeu, G.W., (2005) Landelijk Grondgebruiksbestand Nederland (LGN5). Vervaardiging, nauwkeurigheid en gebruik. Wageningen, Alterra. Alterra-report 1213, 92 pp.
\item Puijenbroek, P. van; Clement, J., (2008) Het oppervlaktewater getypeerd: de eerste Nederlandse watertypenkaart. Agro informatica 21(3): 21-25.
}
}
\note{These colour palettes are only valid for factor-type variables. The names of classes used in the legend can be obtained by loading the palette list.}
\seealso{\code{\link{SAGA_pal}}, \code{\link{R_pal}}}
\examples{
data(worldgrids_pal)
\dontrun{# globcov palette with class names:
display.pal(worldgrids_pal)
dev.off()
display.pal(worldgrids_pal, sel=5, names=TRUE)
}
}
\keyword{color}