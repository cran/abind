\name{abind}
\alias{abind}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Combine multi-dimensional arrays}
\description{
Combine multi-dimensional arrays.  This is a
 generalization of cbind and rbind.  Takes a sequence of
 vectors, matrices, or arrays and produces a single array of
 the same or higher dimension.
}
\usage{
abind(..., along=N, new.names=NULL, force.array=TRUE, use.anon.names=TRUE, use.first.dimnames=F)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{\dots}{
Vectors, arrays, or data frames.  Missing values (NAs) are
allowed.  The dimensions of all the arrays must match,
except on one dimension (specified by along=).  If these
arguments are named, the name will be used for the name of
the dimension along which the arrays are joined.
}

\item{along}{ (optional)
The dimension along which to bind the arrays.  The default
is the last dimension, i.e., N = the maximum length of the
dim attribute of the supplied arrays.  along= can take any
non-negative value up to N+1.  When along= has a fractional
value, a value less than 1, or a value greater than N, a new
dimension is created in the result.  In these cases, the
dimensions of all arguments must be identical.
}
  \item{new.names}{ (optional)
If new.names is a list, it is the first choice for the
dimnames attribute of the result.  It should have the same
structure as a dimnames attribute.  If the names for a
particular dimension are NULL, names for this dimension are
constructed in other ways.

If new.names is a character vector, it is used for dimension
names in the same way as argument names are used.  Zero
length ("") names are ignored.
}
  \item{force.array}{ (optional)
If false, rbind or cbind are called when possible, i.e., when
the arguments are all vectors, and along is not 1, or when the
arguments are vectors or matrices or data frames and along
is 1 or 2.  If rbind or cbind are used, they will preserve
the data.frame classes (or any other class that r/cbind
preserve).  Otherwise, abind will convert objects to class
array.  Thus, to guarantee that an array object is returned,
supply the argument force.array=TRUE.  Note that the use of
rbind or cbind introduces some subtle changes in the way
default dimension names are constructed: see the examples
below.
}
  \item{use.anon.names}{ (optional)
If true, the last resort for dimnames for the along
dimension will be the deparsed versions of anonymous
arguments.  This can result in cumbersome names when
arguments are expressions.
}
  \item{use.first.dimnames}{ (optional)
    When dimension names are present on more than one
argument, should dimension names for the result be take from
the first available (the default is to take them from the
last available, which is the same behavior as
\code{rbind} and \code{cbind}.)
}
}
\details{
The dimensions of the supplied vectors or arrays do not need
to be identical, e.g., arguments can be a mixture of vectors
and matrices.  \code{abind} coerces arguments by the addition
of one dimension in order to make them consistent with other
arguments and along=.  The extra dimension is
added in the place specified by along=.

The default action of abind is to concatenate on the last
dimension, rather than increase the number of dimensions.
For example, the result of calling abind with vectors is a
longer vector (see first example below).  This differs from
the action of \code{rbind} and cbind which is to return a matrix when
called with vectors.  abind can be made to behave like cbind
on vectors by specifying along=2, and like rbind by
specifying along=0.

The dimnames of the returned object are pieced together
from the dimnames of the arguments, and the names of the
arguments.  Names for each dimension are searched for in the
following order: new.names, argument name, dimnames (or
names) attribute of last argument, dimnames (or names)
attribute of second last argument, etc.  (Supplying the
argument \code{use.first.dimnames=TRUE} changes this to
cause \code{abind} to use dimnames or names from the
first argument first.  The default behavior is the same as
for \code{rbind} and \code{cbind}: use dimnames
from later arguments.)  If some names are supplied for the
along dimension (either as argument names or dimnames in
arguments), names are constructed for anonymous arguments
unless use.anon.names=F.

The function abind.nonames() is identical to abind() except
that it ignores all dimnames information.  It is included
for people to read in order to understand the algorithm.
The argument list for abind.nonames() includes arguments
regarding dimnames so that it can be called with the same
list of arguments as abind().

}
\value{
An array with a dim attribute of length \code{length(dim(...))} if
\code{along} refers to an existing dimension, or
\code{length(dim(...))+1} if \code{along} does not refer to an existing dimension.

\code{rbind} or \code{cbind} are
called to compute the result if (a)
\code{force.array=F}; and (b) the result will be a
two-dimensional object.
}
% \references{ ~put references to the literature/web site here ~ }
\author{Tony Plate tplate@attglobal.net and Richard Heiberger }
% \note{ ~~further notes~~ }

%  ~Make other sections like WARNING with \section{WARNING }{....} ~

% \seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
# unlike cbind or rbind
abind(x=1:4,y=5:8)
# like cbind
abind(x=1:4,y=5:8,along=2)                   
abind(x=1:4,matrix(5:20,nrow=4),along=2)
abind(1:4,matrix(5:20,nrow=4),along=2)
# like rbind
abind(x=1:4,matrix(5:20,nrow=4),along=1)
abind(1:4,matrix(5:20,nrow=4),along=1)
# create a 3-d array out of two matrices
abind(x=matrix(1:16,nrow=4),y=matrix(17:32,nrow=4),along=3)
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ array }% __ONLY ONE__ keyword per line
