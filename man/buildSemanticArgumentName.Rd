\name{buildSemanticArgumentName}
\alias{buildSemanticArgumentName}
\title{
Build semantic argument name
}
\description{
Build a semantic argument name from the suffix you provide.
}
\usage{
buildSemanticArgumentName(suffix_s_1, prefix_s_1 = "x_")
}
\arguments{
  \item{suffix_s_1}{ one legal suffix. See \code{\link{DataFactory}} to get a vector
  of known suffixes. Legal suffixes are known suffixes, augmented by suffixes
  you registered.
}
  \item{prefix_s_1}{ the variable prefix name you want to use. }
}
\details{
 Know that no checks are done on \code{suffix_s_1}. Value you provide will be trusted,
 regular or irregular one.
}
\value{
A single string.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\seealso{
Refer to \code{\link{testFunction}}
}
\examples{
# typical example
buildSemanticArgumentName('i') # x_i

buildSemanticArgumentName('ui_1', 'numberOfItems') # numberOfItems_ui_1
}
\keyword{ programming }
\keyword{ utilities }
\concept{ meta testing }
