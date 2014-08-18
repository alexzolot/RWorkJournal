\name{dtt}
\alias{dtt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function dtt..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dtt(..., keep.rownames = FALSE, check.names = FALSE, key = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{keep.rownames}{
%%     ~~Describe \code{keep.rownames} here~~
}
  \item{check.names}{
%%     ~~Describe \code{check.names} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (..., keep.rownames = FALSE, check.names = FALSE, key = NULL) 
{
    x <- list(...)
    if (!.R.listCopiesNamed) 
        .Call(CcopyNamedInList, x)
    if (identical(x, list(NULL))) 
        return(null.data.table())
    tt <- as.list(substitute(list(...)))[-1L]
    vnames = names(tt)
    if (is.null(vnames)) 
        vnames = rep.int("", length(x))
    vnames[is.na(vnames)] = ""
    novname = vnames == ""
    if (any(!novname)) {
        if (any(vnames[!novname] == ".SD")) 
            stop("A column may not be called .SD. That has special meaning.")
    }
    for (i in which(novname)) {
        if (is.null(ncol(x[[i]]))) {
            if ((tmp <- deparse(tt[[i]])[1]) == make.names(tmp)) 
                vnames[i] <- tmp
        }
    }
    tt = vnames == ""
    if (any(tt)) 
        vnames[tt] = paste("V", which(tt), sep = "")
    n <- length(x)
    if (n < 1L) 
        return(null.data.table())
    if (length(vnames) != n) 
        stop("logical error in vnames")
    vnames <- as.list.default(vnames)
    nrows = integer(n)
    numcols = integer(n)
    for (i in seq_len(n)) {
        xi = x[[i]]
        if (is.null(xi)) 
            stop("column or argument ", i, " is NULL")
        if (is.matrix(xi) || is.data.frame(xi)) {
            xi = as.data.table(xi, keep.rownames = keep.rownames)
            x[[i]] = xi
            numcols[i] = length(xi)
        }
        nrows[i] <- NROW(xi)
        if (numcols[i] > 0L) {
            namesi <- names(xi)
            if (length(namesi) == 0L) 
                namesi = rep.int("", ncol(xi))
            namesi[is.na(namesi)] = ""
            tt = namesi == ""
            if (any(tt)) 
                namesi[tt] = paste("V", which(tt), sep = "")
            if (novname[i]) 
                vnames[[i]] = namesi
            else vnames[[i]] = paste(vnames[[i]], namesi, sep = ".")
        }
    }
    nr <- max(nrows)
    ckey = NULL
    recycledkey = FALSE
    for (i in seq_len(n)) {
        xi = x[[i]]
        if (is.data.table(xi) && haskey(xi)) {
            if (nrows[i] < nr) 
                recycledkey = TRUE
            else ckey = c(ckey, key(xi))
        }
    }
    for (i in which(nrows < nr)) {
        xi <- x[[i]]
        if (identical(xi, list())) {
            x[[i]] = vector("list", nr)
            next
        }
        if (nrows[i] == 0L) 
            stop("Item ", i, " has no length. Provide at least one item (such as NA, NA_integer_ etc) to be repeated to match the ", 
                nr, " rows in the longest column. Or, all columns can be 0 length, for insert()ing rows into.")
        if (nr\%\%nrows[i] != 0L) 
            warning("Item ", i, " is of size ", nrows[i], " but maximum size is ", 
                nr, " (recycled leaving remainder of ", nr\%\%nrows[i], 
                " items)")
        if (is.data.frame(xi)) {
            ..i = rep(seq_len(nrow(xi)), length.out = nr)
            x[[i]] = xi[..i, , drop = FALSE]
            next
        }
        if (is.atomic(xi) || is.list(xi)) {
            x[[i]] = rep(xi, length.out = nr)
            next
        }
        stop("problem recycling column ", i, ", try a simpler type")
        stop("argument ", i, " (nrow ", nrows[i], ") cannot be recycled without remainder to match longest nrow (", 
            nr, ")")
    }
    if (any(numcols > 0L)) {
        value = vector("list", sum(pmax(numcols, 1L)))
        k = 1L
        for (i in seq_len(n)) {
            if (is.list(x[[i]]) && !is.ff(x[[i]])) {
                for (j in seq_len(length(x[[i]]))) {
                  value[[k]] = x[[i]][[j]]
                  k = k + 1L
                }
            }
            else {
                value[[k]] = x[[i]]
                k = k + 1L
            }
        }
    }
    else {
        value = x
    }
    vnames <- unlist(vnames)
    if (check.names) 
        vnames <- make.names(vnames, unique = TRUE)
    setattr(value, "names", vnames)
    setattr(value, "row.names", .set_row_names(nr))
    setattr(value, "class", c("data.table", "data.frame"))
    if (!is.null(key)) {
        if (!is.character(key)) 
            stop("key argument of data.table() must be character")
        if (length(key) == 1L) {
            key = strsplit(key, split = ",")[[1L]]
        }
        setkeyv(value, key)
    }
    else {
        if (length(ckey) && !recycledkey && !any(duplicated(ckey)) && 
            all(ckey \%in\% names(value)) && !any(duplicated(names(value)[names(value) \%in\% 
            ckey]))) 
            setattr(value, "sorted", ckey)
    }
    alloc.col(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
