adrop <- function(x, drop=TRUE, named.vector=TRUE, one.d.array=FALSE) {
    if (is.null(dim(x)))
        stop("require an object with a dim attribute")
    x.dim <- dim(x)
    if (is.logical(drop)) {
        if (length(drop) != length(x.dim))
            stop("length of drop is not equal length of dim(x)")
        drop <- which(drop)
    } else if (is.character(drop)) {
        if (any(is.na(i <- match(drop, names(x.dim)))))
            stop("dimension names ", paste("'", drop[is.na(i)], "'", sep="", collapse=" "), " not found in x")
        drop <- i
    }

    if (!is.numeric(drop) || any(is.na(drop)) || any(drop<1 | drop>length(x.dim)))
        stop("drop must contain dimension numbers")
    if (!all(x.dim[drop]==1))
        stop("dimensions to drop do not have length 1")

    x.dimnames <- dimnames(x)
    dimnames(x) <- NULL
    dim(x) <- NULL
    if (length(x.dim[-drop])>1 || (length(x.dim[-drop])==1 && one.d.array)) {
        # array result
        dim(x) <- x.dim[-drop]
        if (!is.null(x.dimnames))
            dimnames(x) <- x.dimnames[-drop]
    } else if (length(x.dim[-drop])==1 && named.vector) {
        # named vector result
        names(x) <- x.dimnames[-drop][[1]]
    } else {
        # unnamed vector or single element result
    }
    x
}
