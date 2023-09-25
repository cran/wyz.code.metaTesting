\name{qualifyFunctionArguments}
\alias{qualifyFunctionArguments}
\title{Qualify function arguments.}
\description{
Retrieve information about function arguments.
}
\usage{
qualifyFunctionArguments(fun_f_1)
}
\arguments{
\item{fun_f_1}{A single \code{function}, not a string. }
}
\value{
A emph{\code{list}} with following names
\item{\code{argument_names}}{ a character \code{vector} of all the function argument names}
\item{\code{owns_ellipsis}}{ a boolean. Is \code{TRUE} when \ldots belongs to argument names}
\item{\code{symbol_names}}{ a character \code{vector}  of argument names that are symbols }
\item{\code{symbol_indexes}}{ the integer indexes of symbol names in the argument names}
\item{\code{stripped_symbol_names }}{ a character \code{vector} of argument names that are symbols,
not considering \ldots}
\item{\code{stripped_symbol_indexes }}{ the integer indexes of stripped symbol names
in the argument names}
\item{\code{default_names}}{  a character \code{vector} of argument names that owns default values }
\item{\code{default_indexes}}{ the integer indexes of default valued arguments names in
the argument names}
\item{code{arguments}}{a \code{pairList} of argument names and values.
Refer to \code{\link{formals}} for more information}
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