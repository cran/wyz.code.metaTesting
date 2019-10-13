\name{computeArgumentsCombination}
\alias{computeArgumentsCombination}
\title{
Compute Function Arguments Combination
}
\description{
Computes a priori legal combinations of function arguments, according to the
function definition (see \code{\link{formals}}).

Allows to foresee test complexity for a function, as this is in narrow relationship,
with the number of various call signatures that could be used. The number of signatures
is in itself a good indicator of complexity.
}
\usage{
computeArgumentsCombination(fun_f_1)
}

\arguments{
  \item{fun_f_1}{ an \R function }
}
\details{
Computes an a priori legal \code{list} of argument signatures for the provided
function.
}
\value{
A \code{list} holding \code{names}, \code{number} and \code{signatures}, each of
them are \code{list}.

The \code{names} provides names of mandatory \code{arguments}, \code{ellipsis} (\ldots)
arguments and of \code{default} arguments.

The \code{number} provides the number of replacements per argument.

The \code{signatures} are the resulting textual argument combinations.
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
