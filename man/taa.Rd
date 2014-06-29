\name{taa}
\alias{taa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function taa..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
taa(mb = TRUE, order.col = "NAME", width = 80, env = parent.frame(), silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mb}{
%%     ~~Describe \code{mb} here~~
}
  \item{order.col}{
%%     ~~Describe \code{order.col} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
}
  \item{silent}{
%%     ~~Describe \code{silent} here~~
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
function (mb = TRUE, order.col = "NAME", width = 80, env = parent.frame(), 
    silent = FALSE) 
{
    tt = objects(envir = env, all.names = TRUE)
    ss = which(as.logical(sapply(tt, function(x) is.data.table(get(x, 
        envir = env)))))
    if (!length(ss)) {
        if (!silent) 
            cat("No objects of class data.table exist in .GlobalEnv\n")
        return(invisible(data.table(NULL)))
    }
    tab = tt[ss]
    info = data.table(NAME = tab, NROW = format(sprintf("\%4s", 
        prettyNum(as.character(sapply(tab, function(x) nrow(get(x, 
            envir = env)))), big.mark = ",")), justify = "right"))
    if (mb) {
        s = sapply(tab, function(x) ceiling(object.size(get(x, 
            envir = env))/1024^2))
        info$MB = format(s, justify = "right")
        total = sum(s)
    }
    info$COLS = as.vector(sapply(tab, function(x) paste(colnames(get(x, 
        envir = env)), collapse = ",")))
    info$KEY = as.vector(sapply(tab, function(x) paste(attr(get(x, 
        envir = env), "sorted"), collapse = ",")))
    if (!order.col \%in\% names(info)) 
        stop("order.col='", order.col, "' not a column name of info")
    info = info[base::order(info[[order.col]])]
    m = as.matrix(info)
    colnames(m)[2] = sprintf(paste("\%", nchar(m[1, "NROW"]), 
        "s", sep = ""), "NROW")
    if (mb) 
        colnames(m)[3] = sprintf(paste("\%", nchar(m[1, "MB"]), 
            "s", sep = ""), "MB")
    m[, "COLS"] = substring(m[, "COLS"], 1, width)
    m[, "KEY"] = substring(m[, "KEY"], 1, width)
    if (!silent) {
        print(m, quote = FALSE, right = FALSE)
        if (mb) 
            cat("Total: ", prettyNum(as.character(total), big.mark = ","), 
                "MB\n", sep = "")
    }
    invisible(info)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
