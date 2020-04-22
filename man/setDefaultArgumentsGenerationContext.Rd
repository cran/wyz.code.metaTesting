\name{setDefaultArgumentsGenerationContext}
\alias{setDefaultArgumentsGenerationContext}
\alias{default_arguments_context}
\alias{dac}
\title{Set default arguments generation context.}
\description{
Set default arguments generation context
}
\usage{
setDefaultArgumentsGenerationContext(useDefaultArguments_b_1 = TRUE,
                                     useAllDefaultArguments_b_1 = FALSE)
}
\arguments{
\item{useDefaultArguments_b_1}{a single \emph{\code{boolean}} value to specify the usage of
default arguments in generated function call}
\item{useAllDefaultArguments_b_1}{A single \emph{\code{boolean}}  value to specify usage of all
default valued arguments in generated function call. Second argument is
considered only when first argument is \emph{\code{TRUE}}.}
}
\value{
A \emph{\code{list}} holding the provided values, allowing easy reuse either interactively
or programmatically, accessible through names
\bold{\code{use}},  and \bold{\code{use_all}}.

Predefined variables named \bold{\code{default_arguments_context}} and \bold{\code{dac}}
hold most common definition cases. Very helpfull as it simplifies reuses and reduces code
length.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
# a typical instanciation
mydgc <- list(
  setDefaultArgumentsGenerationContext(FALSE, FALSE),
  setDefaultArgumentsGenerationContext(TRUE, FALSE),
  setDefaultArgumentsGenerationContext(TRUE, TRUE)
)

# uses predefined variable
print(dac$partial)
}
\keyword{ programation}
\keyword{ utilities }
\concept{ data generation }
\concept{ meta testing }
