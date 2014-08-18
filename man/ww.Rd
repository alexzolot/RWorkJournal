\name{ww}
\alias{ww}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function ww..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ww(width, height, pointsize, record, rescale, xpinch, ypinch, bg, canvas, gamma, xpos, ypos, buffered, title, restoreConsole, clickToConfirm, fillOddEven, family = "", antialias)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{pointsize}{
%%     ~~Describe \code{pointsize} here~~
}
  \item{record}{
%%     ~~Describe \code{record} here~~
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
}
  \item{xpinch}{
%%     ~~Describe \code{xpinch} here~~
}
  \item{ypinch}{
%%     ~~Describe \code{ypinch} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{canvas}{
%%     ~~Describe \code{canvas} here~~
}
  \item{gamma}{
%%     ~~Describe \code{gamma} here~~
}
  \item{xpos}{
%%     ~~Describe \code{xpos} here~~
}
  \item{ypos}{
%%     ~~Describe \code{ypos} here~~
}
  \item{buffered}{
%%     ~~Describe \code{buffered} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{restoreConsole}{
%%     ~~Describe \code{restoreConsole} here~~
}
  \item{clickToConfirm}{
%%     ~~Describe \code{clickToConfirm} here~~
}
  \item{fillOddEven}{
%%     ~~Describe \code{fillOddEven} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{antialias}{
%%     ~~Describe \code{antialias} here~~
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
function (width, height, pointsize, record, rescale, xpinch, 
    ypinch, bg, canvas, gamma, xpos, ypos, buffered, title, restoreConsole, 
    clickToConfirm, fillOddEven, family = "", antialias) 
{
    check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
    msg <- "screen devices should not be used in examples etc"
    if (identical(check, "stop")) 
        stop(msg, domain = NA)
    else if (identical(check, "warn")) 
        warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)
    new <- list()
    if (!missing(width)) 
        new$width <- as.double(width)
    if (!missing(height)) 
        new$height <- as.double(height)
    if (!missing(pointsize)) 
        new$pointsize <- as.double(pointsize)
    if (!missing(record)) 
        new$record <- record
    if (!missing(rescale)) 
        new$rescale <- rescale
    if (!missing(xpinch)) 
        new$xpinch <- as.double(xpinch)
    if (!missing(ypinch)) 
        new$ypinch <- as.double(ypinch)
    if (!missing(bg)) 
        new$bg <- bg
    if (!missing(canvas)) 
        new$canvas <- canvas
    if (!missing(gamma)) 
        new$gamma <- gamma
    if (!missing(xpos)) 
        new$xpos <- as.integer(xpos)
    if (!missing(ypos)) 
        new$ypos <- as.integer(ypos)
    if (!missing(buffered)) 
        new$buffered <- buffered
    if (!missing(title)) 
        new$title <- title
    if (!missing(restoreConsole)) 
        new$restoreConsole <- restoreConsole
    if (!missing(clickToConfirm)) 
        new$clickToConfirm <- clickToConfirm
    if (!missing(fillOddEven)) 
        new$fillOddEven <- fillOddEven
    if (!missing(antialias)) 
        new$antialias <- match.arg(antialias, aa.win)
    d <- check.options(new = new, envir = .WindowsEnv, name.opt = ".Windows.Options", 
        reset = FALSE, assign.opt = FALSE)
    rescale <- match(d$rescale, c("R", "fit", "fixed"))
    if (is.na(rescale)) 
        stop("invalid value for 'rescale'")
    antialias <- pmatch(d$antialias, aa.win)
    invisible(.External(C_devga, "", d$width, d$height, d$pointsize, 
        d$record, rescale, d$xpinch, d$ypinch, d$canvas, d$gamma, 
        d$xpos, d$ypos, d$buffered, .PSenv, d$bg, d$restoreConsole, 
        d$title, d$clickToConfirm, d$fillOddEven, family, antialias))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
