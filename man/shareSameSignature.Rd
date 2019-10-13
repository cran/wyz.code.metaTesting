\name{shareSameSignature}
\alias{shareSameSignature}
\title{
Share same signature
}
\description{
Determine if two function share the same signature of arguments
}
\usage{
shareSameSignature(function_f_1, functionTemplate_f_1)
}
\arguments{
  \item{function_f_1}{
A \R function to check arguments from
}
  \item{functionTemplate_f_1}{ A \R function to check arguments with }
}
\value{
A \code{boolean} value.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
# typical examples
shareSameSignature(cos, sin) # TRUE

shareSameSignature(is.logical, sin) # TRUE

shareSameSignature(sum, cos) # FALSE

}
\keyword{ programation}
\keyword{ utilities }
\concept{ meta testing }