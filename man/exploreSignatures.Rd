\name{exploreSignatures}
\alias{exploreSignatures}
\title{
Explore Signatures
}
\description{
Test an offensive programming wrapper function, applying various argument
signatures.
}
\usage{
exploreSignatures(fun_f_1,
                  argumentsTypeRestrictions_l = list(),
                  signaturesRestrictions_l = list())
}
\arguments{
  \item{fun_f_1}{ a single \R function. Must be an offensive programming wrapper
  function. See \code{\link{opwf}}.
}
  \item{argumentsTypeRestrictions_l}{
a named \code{list}. Each name must match
  a function argument name. Each content must be a vector of strings, each of them
  matching a \code{retrieveDataFactory()$getKnownSuffixes()} known suffix.
}
  \item{signaturesRestrictions_l}{
an unnamed \code{list} of single strings, each of them
  matching one of \code{computeArgumentsCombination(fun_f_1)$signature}.
}
}
\details{
This function offers a really convenient way to test your own functions, without
the burden of building the execution context, that is much trickier than one can imagine at first glance.

Moreover it provides argument signature analysis, which is not provided by \code{\link{testFunction}}.

Arguments restriction parameter \emph{\code{argumentsTypeRestrictions_l}} allows to restrict on demand, value types exploration. It is very useful and convenient to reduce the exploration tree, and to shorten execution time.

By default, a total of 768 tests will run for a single function, when no
\code{signaturesRestrictions_l} is set. This may requires some time to achieve.

When working interactively, a good practice is to use \code{\link{computeArgumentsCombination}} prior to use function \code{\link{computeArgumentsCombination}}, as it will provide complexity information about the function you wish to test. The number of signature is a good metric of function call complexity. Know that each of them will be tested, and data generation has to be achieved for each parameter according to global or restricted scheme, depending on your \emph{\code{argumentsTypeRestrictions_l}} inputs.

}
\value{
A \code{list} with names \emph{\code{info}}, \emph{\code{success}}, \emph{\code{failure}}, each of them
being a \code{list}.

The \emph{\code{info}} sub-list holds execution results. It holds following entries
\itemize{
  \item{\emph{\code{raw}}} {is a \code{list}, providing capture of execution context, data and results.}
  \item{\emph{\code{good}}} {is a \code{list}, providing same information as \emph{\code{raw}}, filtered to retain only tests that do not generate any error.}
  \item{\emph{\code{bad}}} {is a \code{list}, providing same information as \emph{\code{raw}}, filtered to retain only tests that do generate error.
}
}

The \emph{\code{success}} sub-list holds analysis results for tests which do
not generate errors. It holds following entries
\itemize{
  \item{\emph{\code{code}}} {is a \code{data.table}, providing used call code and results.}
  \item{\emph{\code{table}}} {is a \code{data.table}, providing used argument signatures and execution context information.}
  \item{\emph{\code{synthesis}}} {is a \code{list}, providing synthesis information. Much easier to read, than \code{table} entry.
}
}

The \emph{\code{failure}} subsist holds analysis results for tests which do
generate errors. It holds following entries
\itemize{
  \item{\emph{\code{table}}} {is a \code{data.table}, providing encountered error messages and execution
context information}
  \item{\emph{\code{synthesis}}} {is a \code{list}, providing synthesis information. Much easier to read,
than \code{table} entry.
}
}
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\seealso{
Refer to \code{\link{testFunction}} and to \code{\link{generateData}}.
}
\examples{
\donttest{
# typical use case
op_sum <- opwf(sum, c('...', 'removeNA_b_1'))

rv_sum <- exploreSignatures(op_sum, list(... = c('im', 'r', 'cm')))

# which are the errors of exploration and in what context do they occur?
print(rv_sum$failure$synthesis)

# which are the good behaviors of exploration and in what context do they occur?
print(rv_sum$success$synthesis)

# Restrict signatures to use for exploration testing on op_sum
# Consider only two cases: no argument and ellipsis1_, ellispsis2_
cac_sum <- computeArgumentsCombination(op_sum)
rv_sum_f <- exploreSignatures(op_sum, list(... = c('im', 'r', 'cm')),
                              cac_sum$signatures[c(1, 5)])
}
}
\keyword{ programation}
\keyword{ utilities }
\concept{ meta testing }
