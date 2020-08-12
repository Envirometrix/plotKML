\name{normalizeFilename}
\alias{normalizeFilename}
\title{Normalize filename string}
\description{Remove all reserved characters from the file name.}
\usage{
normalizeFilename(x, form = c("default", "8.3")[1], 
                 fix.encoding = TRUE, sub.sign = "_")
}
\arguments{
  \item{x}{input character}
  \item{form}{target format (standard or the short 8.3 file name)}
  \item{fix.encoding}{logical; specifies whether to fix the encoding}
  \item{sub.sign}{substitution symbol}
}
\details{This function removes all \href{http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx}{reserved characters}: (less than), (greater than), (colon), (double quote),  (forward slash), (backslash), (vertical bar or pipe), (question mark), (asterisk), and empty spaces, from the file name. This is important when writing a list of objects to an external file (e.g. KML) as it prevents from creating erroneous file names.}
\author{Tomislav Hengl}
\seealso{\code{utils::shortPathName}, \code{RSAGA:set.file.extension}}
\examples{
normalizeFilename("name[\%].txt")
normalizeFilename("name .txt")
}
\keyword{ utilities }

