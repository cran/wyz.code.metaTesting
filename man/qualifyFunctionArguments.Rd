\name{qualifyFunctionArguments}
\alias{qualifyFunctionArguments}
\title{Qualify function arguments.}
\description{
Use method \code{qualifyFunctionArguments} to retrieve information about
function arguments.
}
\usage{
qualifyFunctionArguments(fun_f_1)
}
\arguments{
\item{fun_f_1}{A constrained vector of function values. Vector length must be 1.}
}
\value{
A \code{list} with following names
\item{argument_names}{ a character vector of all the function argument names}
\item{owns_ellipsis}{ a boolean. Is \code{TRUE} when \ldots belongs to argument names}
\item{symbol_names}{ a character vector of argument names that are symbols }
\item{symbol_indexes}{ the integer indexes of symbol names in the argument names}
\item{stripped_symbol_names }{ a character vector of argument names that are symbols,
not considering \ldots}
\item{stripped_symbol_indexes }{ the integer indexes of stripped symbol names
in the argument names}
\item{default_names }{  a character vector of argument names that owns default values }
\item{default_indexes}{ the integer indexes of default valued arguments names in
the argument names}
\item{arguments}{ a \code{pairList} of argument names and values. See \code{\link{formals}}}
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
# typical examples

qualifyFunctionArguments(Sys.Date)

qualifyFunctionArguments(cos)

qualifyFunctionArguments(sum)

}
\keyword{ programation}
\keyword{ utilities }
\concept{ meta testing }