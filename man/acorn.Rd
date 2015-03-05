\name{acorn}
\alias{acorn}
\alias{ahead}
\alias{acorn.default}
\title{Return a corner of an array object (like head)}
\description{
  Return a small corner of an array object, like \code{head()} or \code{tail()} but
  taking only a few slices on each dimension.
}
\usage{
acorn(x, n=6, m=5, r=1, ...)
}
\arguments{
  \item{x}{ An array (including a matrix or a data frame) }
  \item{n,m,r}{ Numbers of items on each dimension.  A negative number
   is interpreted as this many items at the end (like tail).}
  \item{...}{Further arguments specifying numbers of slices to return on
  each dimension.}
}
\details{
  Like \code{head()} for multidimensional arrays, with two differences:
  (1) returns just a few items on each dimension, and (2) negative
  numbers are treated like \code{tail()}.
}
\value{
An object like \code{x} with fewer elements on each dimension.
}
\author{Tony Plate \email{tplate@acm.org}}
\examples{
x <- array(1:24,dim=c(4,3,2),dimnames=rev(list(letters[1:2],LETTERS[1:3],letters[23:26])))
acorn(x)
acorn(x, 3)
acorn(x, -3)
acorn(x, 3, -2)
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ array }% __ONLY ONE__ keyword per line
