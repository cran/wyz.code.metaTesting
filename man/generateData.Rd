\name{generateData}
\alias{generateData}
\title{
Generate Data
}
\description{
Function to generate data.
}
\usage{
generateData(function_f_1,
             argumentsTypeRestrictions_l = list(),
             replacementContext_l = setGenerationContext(),
             ellipsisReplacementContext_l = setGenerationContext(),
             defaultArgumentsContext_l = setDefaultArgumentsGenerationContext(),
             functionName_s_1 = deparse(substitute(function_f_1))
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{function_f_1}{ a single \R function, offensive programming ready, with
  using semantic argument names }
  \item{argumentsTypeRestrictions_l}{ a named \code{list}. Each name must match
  a function argument name. Each content must be a vector of strings, each of them
  matching a \code{\link{DataFactory}} known suffix.
}
  \item{replacementContext_l}{ a generation context, as defined by
  \code{\link{setGenerationContext}} function, applicable to standard arguments of the
  function, if any.
}
  \item{ellipsisReplacementContext_l}{ a generation context, as defined by
  \code{\link{setGenerationContext}} function, applicable to \ldots arguments of the
  function.
}
  \item{defaultArgumentsContext_l}{
a generation context, as defined by
  \code{\link{setDefaultArgumentsGenerationContext}} function, applicable to default arguments of the
  function.
}
  \item{functionName_s_1}{ A character vector of length 1, holding the function name.
  This is particularly useful, in scripts. }
}
\details{
Generate a driven aleatory set of data to be used as argument in a call to function
\code{fun_f_1}. Generation is driven by the \code{argumentsTypeRestrictions_l} argument.
}
\value{
A \code{list} with names \code{generation}, \code{data}, \code{context}, and \code{n},
representing respectively, argument name generation, data, data type generation context, and
the number of data generated at the first level.

Generated data are ready for use and accessible using the \code{data} name of
the \code{list}.
}
\seealso{
Refer to \code{\link{DataFactory}} and to \code{\link{testFunction}}.
}
\examples{
# typical example
op_sum <- opwf(sum, c('...', 'removeNA_b_1'))
op_sum_atr <- list('...' = c('i', 'd', 'c'))
ec <- setGenerationContext(0, TRUE, FALSE)
gd <- generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[1]], dac$none)
}
\keyword{ programation}
\keyword{ utilities }
\concept{ data generation }
\concept{ meta testing }

