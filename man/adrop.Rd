\name{adrop}
\alias{adrop}
\alias{adrop.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Drop dimensions of an array object }
\description{
  Drop degenerate dimensions of an array object.  Offers less
  automaticity and more control than the base \code{drop()} function.
  \code{adrop()} is a S3 generic, with one method, \code{adrop.default},
  supplied in the \code{abind} package.
}
\usage{
adrop(x, drop = TRUE, named.vector = TRUE, one.d.array = FALSE, ...)
}

\arguments{
  \item{x}{ An array (including a matrix) }
  \item{drop}{ A logical or numeric vector describing exactly which dimensions to
    drop. It is intended that this argument be supplied always.  The
  default is very rarely useful (\code{drop=TRUE} means drop the first
  dimension of a 1-d array).}
  \item{named.vector}{Optional, defaults to \code{TRUE}.  Controls
    whether a vector result has names derived from the \code{dimnames}
    of \code{x}.}
  \item{one.d.array}{Optional, defaults to \code{FALSE}.  If
    \code{TRUE}, a one-dimensional array result will be an object with a
    \code{dim} attribute of
    length 1, and possibly a \code{dimnames} attribute.  If
    \code{FALSE}, a one-dimensional result will be a vector object
    (named if \code{named.vector==TRUE}).}
  \item{...}{There are no additional arguments allowed for
  \code{adrop.default} but other methods may use them.}
}
\details{
  Dimensions can only be dropped if their extent is one, i.e., dimension
  \code{i} of array \code{x} can be dropped only if \code{dim(x)[i]==1}.
  It is an error to request \code{adrop} to drop a dimension whose
  extent is not 1.

  A 1-d array can be converted to a named vector by supplying
  \code{drop=NULL} (which means drop no dimensions, and return a 1-d
  array result as a named vector).
}
\value{
If \code{x} is an object with a \code{dim} attribute (e.g., a matrix or
     \code{array}), then \code{adrop} returns an object like \code{x},
     but with the requested
     extents of length one removed.  Any accompanying \code{dimnames}
     attribute is adjusted and returned with \code{x}.
}

\author{Tony Plate \email{tplate@acm.org}}
% \note{ ~~further notes~~ }

\seealso{ \code{\link{abind}} }
\examples{
x <- array(1:24,dim=c(2,3,4),dimnames=list(letters[1:2],LETTERS[1:3],letters[23:26]))
adrop(x[1,,,drop=FALSE],drop=1)
adrop(x[,1,,drop=FALSE],drop=2)
adrop(x[,,1,drop=FALSE],drop=3)
adrop(x[1,1,1,drop=FALSE],drop=1)
adrop(x[1,1,1,drop=FALSE],drop=2)
adrop(x[1,1,1,drop=FALSE],drop=3)
adrop(x[1,1,1,drop=FALSE],drop=1:2)
adrop(x[1,1,1,drop=FALSE],drop=1:2,one.d=TRUE)
adrop(x[1,1,1,drop=FALSE],drop=1:2,named=FALSE)
dim(adrop(x[1,1,1,drop=FALSE],drop=1:2,one.d=TRUE))
dimnames(adrop(x[1,1,1,drop=FALSE],drop=1:2,one.d=TRUE))
names(adrop(x[1,1,1,drop=FALSE],drop=1:2,one.d=TRUE))
dim(adrop(x[1,1,1,drop=FALSE],drop=1:2))
dimnames(adrop(x[1,1,1,drop=FALSE],drop=1:2))
names(adrop(x[1,1,1,drop=FALSE],drop=1:2))
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ array }% __ONLY ONE__ keyword per line
