\name{computeArgumentsCombination}
\alias{computeArgumentsCombination}
\title{Compute Function Arguments Combination }
\description{
Computes a priori legal combinations of function arguments, according to the
function definition (see \code{\link{formals}}).
}
\usage{
computeArgumentsCombination(fun_f_1)
}
\arguments{
  \item{fun_f_1}{ an \R \emph{\code{function}} }
}
\details{
Computes an a priori legal \emph{\code{list}} of argument signatures for the provided
function.

Allows to foresee test complexity for a function, as this is in narrow relationship,
with the number of various call signatures that should be tested. The number of signatures
is in itself a good indicator of complexity.
}
\value{
A \emph{\code{list}} containing following named \bold{\code{list}}
\tabular{ll}{
\bold{\code{names}} \tab names of mandatory \bold{\code{arguments}}, \bold{\code{ellipsis} (\ldots)}
arguments and of \bold{\code{default}} arguments. \cr
\bold{\code{number}} \tab The \code{number} provides the number of replacements per argument. \cr
\bold{\code{signatures}} \tab The \code{signatures} are the resulting textual argument combinations. \cr
}
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
computeArgumentsCombination(append)

computeArgumentsCombination(kronecker)
}
\keyword{ programming }
\keyword{ utilities }
\concept{ meta testing }
