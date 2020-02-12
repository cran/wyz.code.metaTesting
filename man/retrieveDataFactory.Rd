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
Provides a data factory.

Retrieves a \code{DataFactory} from options variable \code{op_mt_data_factory}
to consider your customized data factory.
}
\value{
An \R object that is a \code{DataFactory}.
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

df <- retrieveDataFactory()
df$addSuffix('a', "array", draw_integer_array_dim2)

options(op_mt_data_factory = df)
fg <- retrieveDataFactory() # retrieves the user defined data factory
fg$getRecordedTypes()[suffix == 'a'] # right behavior !

# wrong behavior as retrieveDataFactory will provide the default factory and not yours!
options(op_mt_data_factory = NULL)
fh <- retrieveDataFactory() # retrieves the default factory
fh$getRecordedTypes()[suffix == 'a']
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
