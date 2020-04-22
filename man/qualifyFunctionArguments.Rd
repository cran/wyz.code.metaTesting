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
\item{fun_f_1}{A single \emph{\code{function}}, not a string. }
}
\value{
A emph{\code{list}} with following names
\item{\bold{\code{argument_names}}}{ a character \emph{\code{vector}} of all the function argument names}
\item{\bold{\code{owns_ellipsis}}}{ a boolean. Is \emph{\code{TRUE}} when \ldots belongs to argument names}
\item{\bold{\code{symbol_names}}}{ a character \emph{\code{vector}}  of argument names that are symbols }
\item{\bold{\code{symbol_indexes}}}{ the integer indexes of symbol names in the argument names}
\item{\bold{\code{stripped_symbol_names }}}{ a character \emph{\code{vector}} of argument names that are symbols,
not considering \ldots}
\item{\bold{\code{stripped_symbol_indexes }}}{ the integer indexes of stripped symbol names
in the argument names}
\item{\bold{\code{default_names }}}{  a character \emph{\code{vector}} of argument names that owns default values }
\item{\bold{\code{default_indexes}}}{ the integer indexes of default valued arguments names in
the argument names}
\item{\bold{\code{arguments}}}{ a \emph{\code{pairList}} of argument names and values.
Refer to \emph{\bold{\code{\link{formals}}}} for more information}
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