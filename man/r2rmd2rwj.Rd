\name{r2rmd2rwj}
\alias{r2rmd2rwj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function r2rmd2rwj..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
r2rmd2rwj(theFile = get.theFile(), base64 = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theFile}{
%%     ~~Describe \code{theFile} here~~
}
  \item{base64}{
%%     ~~Describe \code{base64} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (theFile = get.theFile(), base64 = FALSE, ...) 
{
    catf("\nr2rmd2rwj: \%s\n", theFile)
    rmd = code2rmd(theFile, ...)
    strr(rmd)
    out = treat.knit.html(rmd$fout, rFile = theFile)
    if (base64) 
        rwjFig2base64(out$fout.htm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
