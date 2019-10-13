\name{retrieveDataFactory}
\alias{retrieveDataFactory}
\title{
Retrieve Data Factory
}
\description{
As the data factory may be modified, this function allows you to make changes and to record them in
your own specialized data generation factory, to match various needs and ease reuse. }
\usage{
retrieveDataFactory()
}
\details{
Retrieves a \code{\link{DataFactory}} from environment variable
\code{OP_DATA_FACTORY} or provides a default type factory.
}
\value{
An \R object that is a \code{\link{DataFactory}}.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
##---- typical case ----

draw_integer_array_dim2 <- function(n, replace_b_1 = TRUE) {
  m <- n + sample(0:3, 1)
  matrix(seq(1, n * m), byrow = TRUE, nrow = n,
         dimnames = list(paste('row_', 1:n), paste('col_', 1:m)))
}

df <- DataFactory()
df$addSuffix('a', "array", draw_integer_array_dim2)

Sys.setenv("OP_DATA_FACTORY" = "df")
fg <- retrieveDataFactory() # retrieves the factory pointed by R variable df
fg$getRecordedTypes()[suffix == 'a'] # right behavior !

# wrong behavior as retrieveDataFactory will provide the default factory and not yours!
Sys.setenv("OP_DATA_FACTORY" = "")
fh <- retrieveDataFactory() # retrieves the default factory
fh$getRecordedTypes()[suffix == 'a']
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
