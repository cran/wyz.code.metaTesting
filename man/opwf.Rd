\name{opwf}
\alias{opwf}
\alias{offensiveProgrammingWrapFunction}
\title{
Offensive Programming Wrap Function
}
\description{
Create an offensive programming function, wrapping a standard \R function.
}
\usage{
opwf(fun_f_1, parameterNames_s, functionName_s_1 = NA_character_)
}
\arguments{
  \item{fun_f_1}{ a single \R \emph{\code{function}} }
  \item{parameterNames_s}{ the new names of the parameter function, must be
  semantic argument names.  Must be a bijection to actual \bold{\code{fun_f_1}} argument names. }
  \item{functionName_s_1}{ A \emph{\code{string}} holding the function name.
  Default value, implies evaluation using \emph{\code{deparse(substitute(fun_f_1))}}
  }
}
\details{
If any arguments default values are present, they are managed transparently and
should be correctly and automatically substituted.
}
\value{
A \R \emph{\code{function}} which takes given \bold{\code{parameterNames_s}} as arguments.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\seealso{
Refer to \bold{\emph{\code{\link{testFunction}}}}
}
\examples{
# typical example
op_sum <- opwf(sum, c('...', 'removeNA_b_1'))

# example with substituted argument in existing default valued arguments
op_append <- opwf(append, c('originalValues_', 'valuesToInsert_', 'afterIndex_ui_1'))
}
\keyword{ programming }
\keyword{ utilities }
\concept{ meta testing }

