\name{opMetaTestingInformation}
\alias{opMetaTestingInformation}
\title{
Package functions information
}
\description{
A reminder of available functions from this package, and, most common usage
intent. A poor man CLI cheat sheet.
}
\usage{
opMetaTestingInformation()
}
\value{
A \code{data.table} with following column names
\item{function}{a function name provided by this package}
\item{elaboration}{a boolean stating if function could/should be use for elaboration purpose}
\item{verification}{a boolean stating if function could/should be use for verification purpose}
\item{exploitation}{a boolean stating if function could/should be use for exploitation purpose}
\item{information}{a boolean stating if function could/should be use for informative purpose}
\item{kind}{the classification kind of the chosen function}
\item{user}{a typical user role/profile that may use the function}
\item{domain}{internal domain function belongs to}
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\seealso{
Refer also to package vignettes.
}
\examples{
##---- typical case ----
opMetaTestingInformation()
}
\keyword{ programming }
\keyword{ utilities }
\concept{ meta testing }
