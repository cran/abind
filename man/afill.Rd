\name{afill}
\alias{afill<-}
\alias{afill<-.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fill an array with subarrays }
\description{
  Fill an array with subarrays.  \code{afill}
  uses the dimension names
  in the value in determining how to fill the LHS, unlike standard array
  assignment, which ignores dimension names in the value.
\code{afill()} is a S3 generic, with
one method, \code{afill.default}, supplied in the \code{abind} package.
}
\usage{
afill(x, ..., excess.ok = FALSE, local = TRUE) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An array to be changed }
  \item{\dots}{ Arguments that specify indices for \code{x}.  If
    \code{length(dim(value)) < length(dim(x))}, then exactly \code{length(dim(x))}
    anonymous arguments must be supplied, with empty ones corresponding to
    dimensions of \code{x} that are supplied in \code{value}. }
  \item{excess.ok}{ If there are elements of the dimensions of
    \code{value} that are not found in the corresponding dimensions
     of x, they will be discarded if \code{excess.ok=TRUE}.}
  \item{local}{ Should the assignment be done in on a copy of x, and the
    result returned (normal behavior).  If \code{local=FALSE} the
    assignment will be done directly on the actual argument supplied as
    \code{x}, which can be more space efficient.}
  \item{value}{ A vector or array, with dimension names that match some dimensions of
    \code{x} }
}
\details{
  The simplest use of \code{afill} is to fill a sub-matrix. Here is an
  example of this usage:
  \code{
> (x <- matrix(0, ncol=3, nrow=4, dimnames=list(letters[1:4], LETTERS[24:26])))
  X Y Z
a 0 0 0
b 0 0 0
c 0 0 0
d 0 0 0
> (y <- matrix(1:4, ncol=2, nrow=2, dimnames=list(letters[2:3], LETTERS[25:26])))
  Y Z
b 1 3
c 2 4
> afill(x) <- y
> x
  X Y Z
a 0 0 0
b 0 1 3
c 0 2 4
d 0 0 0
>
}
The above usage is equivalent (when x and y have appropriately matching
dimnames) to
\code{
  x[match(rownames(y), rownames(x)), match(colnames(y), colnames(x))] <- y
  }

  A more complex usage of \code{afill} is to fill a sub-matrix in a
  slice of a higher-dimensional array.  In this case, indices for
  \code{x} must be supplied as arguments to \code{afill}, with the
  dimensions corresponding to those of \code{value} being empty, e.g.:
  \code{
> x <- array(0, dim=c(2,4,3), dimnames=list(LETTERS[1:2], letters[1:4], LETTERS[24:26]))
> y <- matrix(1:4, ncol=2, nrow=2, dimnames=list(letters[2:3], LETTERS[25:26]))
> afill(x, 1, , ) <- y
> x[1,,]
  X Y Z
a 0 0 0
b 0 1 3
c 0 2 4
d 0 0 0
> x[2,,]
  X Y Z
a 0 0 0
b 0 0 0
c 0 0 0
d 0 0 0
> 
}

  The most complex usage of \code{afill} is to fill a sub-matrix in multiple
  slice of a higher-dimensional array.  Again, indices for
  \code{x} must be supplied as arguments to \code{afill}, with the
  dimensions corresponding to those of \code{value} being empty.
  Indices in which all slices should be filled can be supplied as
  \code{TRUE}.  E.g.:
  \code{
> x <- array(0, dim=c(2,4,3), dimnames=list(LETTERS[1:2], letters[1:4], LETTERS[24:26]))
> y <- matrix(1:4, ncol=2, nrow=2, dimnames=list(letters[2:3], LETTERS[25:26]))
> afill(x, TRUE, , ) <- y
> x[1,,]
  X Y Z
a 0 0 0
b 0 1 3
c 0 2 4
d 0 0 0
> x[2,,]
  X Y Z
a 0 0 0
b 0 1 3
c 0 2 4
d 0 0 0
> 
}

In the above usage, \code{afill} takes care of replicating \code{value}
in the appropriate fashion (which is not straghtforward in some cases).
}
\value{
  The object \code{x} is changed.  The return value of the assignment is
  the parts of the object \code{x} that are changed.  This is similar to
  how regular subscript-replacement behaves, e.g., the expression
  \code{x[2:3] <- 1:2} returns the vector \code{1:2}, not the entire
  object \code{x}.  However, note that there can be differences
}
% \references{ ~put references to the literature/web site here ~ }
\author{Tony Plate \email{tplate@acm.org}}
% \note{ ~~further notes~~ }
\seealso{ \code{\link{Extract}} }
\examples{
# fill a submatrix
(x <- matrix(0, ncol=3, nrow=4, dimnames=list(letters[1:4], LETTERS[24:26])))
(y <- matrix(1:4, ncol=2, nrow=2, dimnames=list(letters[2:3], LETTERS[25:26])))
afill(x) <- y
x
# fill a slice in a higher dimensional array
x <- array(0, dim=c(2,4,3), dimnames=list(LETTERS[1:2], letters[1:4], LETTERS[24:26]))
y <- matrix(1:4, ncol=2, nrow=2, dimnames=list(letters[2:3], LETTERS[25:26]))
afill(x, 1, , ) <- y
x[1,,]
x[2,,]
# fill multiple slices
x <- array(0, dim=c(2,4,3), dimnames=list(LETTERS[1:2], letters[1:4], LETTERS[24:26]))
y <- matrix(1:4, ncol=2, nrow=2, dimnames=list(letters[2:3], LETTERS[25:26]))
afill(x, TRUE, , ) <- y
x[1,,]
x[2,,]

}

\keyword{ manip }
\keyword{ array }
