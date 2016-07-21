\name{asub}
\alias{asub}
\alias{asub.default}
\title{ Arbitrary subsetting of array-like objects at specified indices }
\description{
Subset array-like objects at specified indices.  \code{asub()} is a S3 generic, with
  one method, \code{asub.default}, supplied in the \code{abind} package.
}
\usage{
asub(x, idx, dims = seq(len = max(length(dim(x)), 1)), drop = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The object to index }
  \item{idx}{ A list of indices (e.g., a list of a mixture of integer,
    character, and logical vectors, but can actually be anything).  Can
    be just a vector in the case that \code{length(dims)==1}.
    \code{NULL} entries in the list will be treated as empty indices.}
  \item{dims}{ The dimensions on which to index (a numeric or integer
    vector).  The default is all of the dimensions.  }
  \item{drop}{ The 'drop' argument to index with (the default is to not
    supply a 'drop' argument }
  \item{...}{There are no additional arguments allowed for
  \code{asub.default} but other methods may use them.}
}
\details{
  Constructs and evaluates an expression to do the requested indexing.
  E.g., for \code{x} with \code{length(dim(x))==4} the call
  \code{asub(x, list(c("a","b"), 3:5), 2:3)} will construct and evaluate
  the expression \code{x[, c("a","b"), 3:5, ]}, and the call
  \code{asub(x, 1, 2, drop=FALSE)} will construct and evaluate
  the expression \code{x[, 1, , , drop=FALSE]}.

  \code{asub} checks that the elements of \code{dims} are in the range 1
  to \code{length(dim(x))} (in the case that \code{x} is a vector,
  \code{length(x)} is used for \code{dim(x)}).  Other than that, no
  checks are made on the suitability of components of \code{idx} as
  indices for \code{x}.  If the  components of \code{idx} have any
  out-of-range values or unsuitable types, this will be left to the
  subsetting method for \code{x} to catch.
}
\value{
  A subset of \code{x}, as returned by \code{x[...]}.
}
\references{ ~put references to the literature/web site here ~ }
\author{Tony Plate \email{tplate@acm.org}}
% \note{ ~~further notes~~ }
\seealso{ \code{\link{Extract}} }
\examples{
x <- array(1:24,dim=c(2,3,4),dimnames=list(letters[1:2],LETTERS[1:3],letters[23:26]))
asub(x, 1, 1, drop=FALSE)
asub(x, list(1:2,3:4), c(1,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ array }
