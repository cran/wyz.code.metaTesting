\name{DataFactory}
\alias{DataFactory}
\title{Data Factory}
\description{
Class \code{DataFactory} from package \code{wyz.code.metaTesting}.
}
\usage{
DataFactory()
}
\details{
It allows to generate data easily, either using generic data generation function
named \code{drawValues} or using specific type generation function as \code{drawRaw}
for example. All the data generation function names start with \code{draw}.

It is an extensible data factory. You may register your own data types and be able
to use it as easily as using provided data generation functions. Use \code{}

See examples below.
}
\value{
An object of class \code{DataFactory}, that is an \code{environment}.
}
\note{
This object is intended for programmers, not for end-users, although playable on
command line ;-).
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
# typical use case
df <- DataFactory()

# discover data generation function names
ls(df, pattern = 'draw*')

# discover recorded types
print(df$getRecordedTypes())

# discover known suffixes
print(df$retrieveKnownSuffixes())

# draw integer values using generic data generation function
# note that a semantic identifier is required, not just a known suffix extension
df$drawValues('x_i', 3)

df$drawValues('x_i_7', 16) # will draw 7 values. Semantic identifier contract wins.

# draw 8 values from aleatory chosen type
df$drawValues(paste0('x_', sample(df$retrieveKnownSuffixes(), 1)), 8)

# draw specified type (raw) using specifc data generation function
v <- df$drawRaw(10)
typeof(v) # raw
}
\keyword{ programation}
\keyword{ utilities }
\concept{ data generation }
\concept{ meta testing }

