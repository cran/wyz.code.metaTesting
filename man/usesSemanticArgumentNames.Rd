\name{usesSemanticArgumentNames}
\alias{usesSemanticArgumentNames}
\title{Uses semantic argument names.}
\description{
Determine if the given function uses semantic argument names.
}
\usage{
usesSemanticArgumentNames(fun_f_1)
}
\arguments{
\item{fun_f_1}{A single \emph{\code{function}}}
}
\value{
A \emph{\code{TRUE}} when arguments used by function are all semantic names.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
f <- function(x_) x_

usesSemanticArgumentNames(f)
# TRUE

usesSemanticArgumentNames(sum)
# FALSE
}
\keyword{ programation}
\keyword{ utilities }
\concept{ meta testing }
