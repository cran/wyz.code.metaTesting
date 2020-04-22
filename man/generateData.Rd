\name{generateData}
\alias{generateData}
\title{ Generate Data }
\description{ Function to generate data. }
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
  \item{function_f_1}{ a single \R \emph{\code{function}}, offensive programming ready,
  therefore using semantic argument names }
  \item{argumentsTypeRestrictions_l}{ a named \emph{\code{list}}. Each name must match
  a function argument name. Each content must be a vector of strings, each of them
  matching a \code{retrieveDataFactory()$getKnownSuffixes()} known suffix.
}
  \item{replacementContext_l}{ a generation context \emph{\code{object}}, as defined by
  \bold{\emph{\code{\link{setGenerationContext}}}} function, applicable to standard
  arguments of the function, if any.
}
  \item{ellipsisReplacementContext_l}{ an ellipsis replacement context \emph{\code{object}},
  as defined by
  \emph{\bold{\code{\link{setGenerationContext}}}} function, applicable to \ldots
  arguments of the function.
}
  \item{defaultArgumentsContext_l}{
a default argument context \emph{\code{object}}, as defined by
  \emph{\bold{\code{\link{setDefaultArgumentsGenerationContext}}}} function,
  applicable to default arguments of the function.
}
  \item{functionName_s_1}{ A character \emph{\code{vector}} of length 1,
  holding the function name. Particularly useful in \R scripts. }
}
\details{
Generate a driven aleatory set of data to be used as argument in a call to \emph{\code{function}}
\code{fun_f_1}. Generation is driven by the \code{argumentsTypeRestrictions_l} argument.
}
\value{
A \emph{\code{object}} with following names
\tabular{ll}{
\bold{\code{generation}} \tab argument name generation \cr
\bold{code{data}} \tab the generated data \cr
\bold{\code{context}} \tab data type generation context \cr
\bold{\code{n}} \tab number of first level data generations \cr
}
}
\seealso{
Refer to \bold{\emph{code{\link{retrieveDataFactory}}}} and to
\bold{\emph{\code{\link{testFunction}}}}.
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

