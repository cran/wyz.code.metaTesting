\name{setGenerationContext}
\alias{setGenerationContext}
\alias{established_replacement_context}
\alias{erc}
\title{Set generation context.}
\description{
Use this function to set generation context.
}
\usage{
 setGenerationContext(replacementNumber_ui_1 = sample(0:3L, 1),
                      homogeneousTypeReplacement_b_1 = FALSE,
                      allowList_b_1 = TRUE,
                      forceList_b_1 = FALSE)
}
\arguments{
\item{replacementNumber_ui_1}{ a single unsigned integer expressing the number
of arguments to generate.}
\item{homogeneousTypeReplacement_b_1}{A single boolean expressing willingness to
replace chosen argument with same type arguments, or not. Useful when dealing with
\ldots. }
\item{allowList_b_1}{a single boolean, expressing the desired result. When \code{TRUE}
result is a \code{list}, a \code{vector} otherwise. }
\item{forceList_b_1}{a single boolean, expressing the desire to get the result as
a \code{list}. }
}
\value{
A \code{list} containing all the provided arguments, accessible through names
\code{homogeneous_type}, \code{number_replacements}, and \code{allow_list}.

Predefined variables named \code{established_replacement_context} and \code{erc}
hold most common definition cases, and therefore greatly eases use and simplify writing.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
# a typical instanciation
egc <- list(
  setGenerationContext(homogeneous = TRUE),
  setGenerationContext(allowList = FALSE)
)

# uses predefined variable
print(erc$homo_vector[[2]])
}
\keyword{ programation}
\keyword{ utilities }
\concept{ data generation }
\concept{ meta testing }

