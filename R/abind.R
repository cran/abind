abind <-
    function(..., along=N, new.names=NULL, force.array=TRUE, use.anon.names=TRUE, use.first.dimnames=FALSE)
{
    do.aperm <- function(x, perm) {
        if (any(perm != (1:length(perm)))) aperm(x, perm) else x
    }
    # N will be length(dim(out))
    N <- max(1, sapply(list(...), function(x) length(dim(x))))
    if (along == 0 || (along > floor(along) && along < ceiling(along))) {
        N <- N + 1
        along <- if (along == 0) 1 else ceiling(along)
    }
    if (length(along) > 1 || along < 1 || along > N + 1)
        stop(paste("\"along\" must specify one dimension of the array,",
                   "or interpolate between two dimensions of the array",
                   sep="\n"))
    if (along > N) N <- along

    if (!force.array && N == 2) {
        if (along == 2) return(cbind(...))
        if (along == 1) return(rbind(...))
    }

    pre <- seq(from=1, len=along - 1)
    post <- seq(to=N - 1, len=N - along)
    # "perm" specifies permutation to put join dimension (along) last
    perm <- c((1:N)[ - along], along)

    arg.list <- list(...)
    arg.names <- names(arg.list)
    if (is.null(arg.names)) arg.names <- rep("", length(arg.list))
    # if new.names is a character vector, treat it as argument names
    if (is.character(new.names)) {
        arg.names[seq(along=new.names)[nchar(new.names)>0]] <-
            new.names[nchar(new.names)>0]
        new.names <- NULL
    }
    use.along.names <- any(arg.names!="")

    # Be careful with dot.args, because if abind was called
    # using do.call(), and had anonymous arguments, the expressions
    # returned by match.call() are for the entire structure
    # E.g., compare:
    # > (function(...)browser())(1:10,letters)
    # Called from: (function(...)  browser()).... 
    # b()> match.call(expand.dots=FALSE)$...
    # list(1:10, letters)
    # With:
    # > test <- function(...) browser()
    # > do.call("test", list(1:3,letters[1:4]))
    # Called from: test(c(1, 2, 3), c("a", "b.... 
    # b(test)> match.call(expand.dots=FALSE)$...
    # list(c(1, 2, 3), c("a", "b", "c", "d")    

    # Create deparsed versions of actual arguments in arg.alt.names
    # These are used for error messages
    if (any(arg.names=="")) {
        if (use.anon.names) {
            # dot.args is the calling expression
            dot.args <- match.call(expand.dots=FALSE)$...
            # the test of length(dot.args) is necessary for things to work
            # with R1.5 (prerelease) -- dot.args loses the "list" functor with R
            # (and dot.args is not a call object), whereas with S-plus, dot.args
            # must have the list functor removed
            if (length(dot.args)>length(arg.list))
                dot.args <- dot.args[-1]
            arg.alt.names <- arg.names
            for (i in seq(along=arg.names)) {
                if (arg.alt.names[i]=="") {
                    if (object.size(dot.args[[i]])<1000)
                        arg.alt.names[i] <- paste(deparse(dot.args[[i]], 40), collapse=";")
                    else
                        arg.alt.names[i] <- paste("X", i, sep="")
                    arg.names[i] <- arg.alt.names[i]
                }
            }
        } else {
            arg.names[arg.names==""] <- paste("X", seq(along=arg.names), sep="")[arg.names==""]
            arg.alt.names <- arg.names
        }
    } else {
        arg.alt.names <- arg.names
    }
    # need to have here: arg.names, arg.alt.names, don't need dot.args

    names(arg.list) <- arg.names
    # dimnames.all is a matrix of dimension names: dimnames.all[j,i] is
    # the vector of names for dimension j of arg i
    dimnames.all <- matrix(vector("list", N*length(arg.names)), nrow=N, ncol=length(arg.names))
    dimnames(dimnames.all) <- list(NULL, arg.names)
    dimnames.new <- vector("list", N)

    # coerce all arguments to have the same number of dimensions
    # (by adding one, if necessary) and permute them to put the
    # join dimension last
    for (i in 1:length(arg.list)) {
        m <- arg.list[[i]]

        # be careful with conversion to array: as.array converts data frames badly
        if (is.data.frame(m)) {
            m <- data.matrix(m)
        } else if (!is.array(m)) {
            # make sure to get the names of a vector and attach them to the array
            dn <- names(m)
            m <- as.array(m)
            if (!is.null(dn))
                dimnames(m) <- list(dn)
        }
        new.dim <- dim(m)
        if (length(new.dim) == N) {
            # Assign the dimnames of this argument to the i'th column of dimnames.all.
            # If dimnames(m) is NULL, would need to do dimnames.all[,i] <- list(NULL)
            # to set all elts to NULL, as dimnames.all[,i] <- NULL does not actually
            # change anything in Splus (leaves whatever is there) and illegal in R.
            # Since dimnames.all has NULL entries to begin with, don't need to do
            # anything when dimnames(m) is NULL
            if (!is.null(dimnames(m)))
                dimnames.all[,i] <- dimnames(m)[perm]
        } else if (length(new.dim) == N - 1) {
            # add another dimension (first set dimnames to NULL to prevent errors)
            if (!is.null(dimnames(m))) {
                # dimnames.all[,i] <- c(dimnames(m)[pre], list(NULL), dimnames(m))[post]
                # is equivalent to dimnames.all[-N,i] <- dimnames(m)
                dimnames.all[-N,i] <- dimnames(m)
                # remove the dimnames so that we can assign a dim of an extra length
                dimnames(m) <- NULL
            }
            dim(m) <- c(new.dim[pre], 1, new.dim[post])
        }
        else stop(paste("'", arg.alt.names[i],
                        "' does not fit: should have `length(dim())'=",
                        N, " or ", N-1, sep=""))

        if (any(perm!=seq(along=perm)))
            arg.list[[i]] <- aperm(m, perm)
        else
            arg.list[[i]] <- m
    }

    # arg.dim is a matrix with length(dim) rows and length(arg.list)
    # columns: arg.dim[j,i]==dim(arg.list[[i]])[j]
    arg.dim <- do.call("cbind", lapply(arg.list, dim))
    if (!all(arg.dim[-N,1] == arg.dim[-N,-1]))
        stop(paste("'", arg.alt.names[seq(2,length(arg.alt.names))
                                  [!apply(arg.dim[-N,1]==arg.dim[-N,-1,drop=FALSE], 2, all)][1]],
                   "' does not fit: arrays must be same size except on dimension ",
                   along, sep=""))

    # find the last (or first) names for each dimensions except the join dimension
    if (N>1)
        for (dd in 1:(N-1)) {
            for (i in (if (use.first.dimnames) seq(along=arg.names) else rev(seq(along=arg.names)))) {
                if (length(dimnames.all[[dd,i]]) > 0) {
                    dimnames.new[[dd]] <- dimnames.all[[dd,i]]
                    break
                }
            }
        }

    # find or create names for the join dimension
    for (i in 1:length(arg.names)) {
        dnmN <- dimnames.all[[N,i]]
        if (length(dnmN) == dim(arg.list[[i]])[N])
            use.along.names <- TRUE
        else {
            # make up names for the along dimension
            if (dim(arg.list[[i]])[N]==1)
                dnmN <- arg.names[i]
            else if (arg.names[i]=="")
                dnmN <- rep("", dim(arg.list[[i]])[N])
            else
                dnmN <- paste(arg.names[i], seq(length=dim(arg.list[[i]])[N]), sep="")
        }
        dimnames.new[[N]] <- c(dimnames.new[[N]], dnmN)
    }
    # if no names at all were given for the along dimension, use none
    if (!use.along.names) dimnames.new[N] <- list(NULL)

    # construct the output array from the pieces
    # don't use names in unlist because this can quickly exhaust memory
    # when abind is called with "do.call" (which creates horrendous names)
    out <- array(unlist(arg.list, use.names=FALSE),
                 dim=c( arg.dim[-N,1], sum(arg.dim[N,])),
                 dimnames=dimnames.new)
    # permute the output array to put the join dimension back in the right place
    if (any(order(perm)!=seq(along=perm)))
        out <- aperm(out, order(perm))
    # if new.names is list of character vectors, use whichever are non-null
    # for dimension names, checking that they are the right length
    if (!is.null(new.names) && is.list(new.names)) {
        for (dd in 1:N)
            if (!is.null(new.names[[dd]]))
                if (length(new.names[[dd]])==dim(out)[dd])
                    dimnames(out)[[dd]] <- new.names[[dd]]
                else
                    warning(paste("Component ", dd,
                                  " of new.names ignored: has length ",
                                  length(new.names[[dd]]), ", should be ",
                                  dim(out)[dd], sep=""))
    }
    out
}
