\name{saveGraphics}
\alias{saveGraphics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function saveGraphics..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
saveGraphics(capt = .main, Width = dev.size(units = "px")[1], off = TRUE, Height = dev.size(units = "px")[2], GraphPointSize = 12, dirr = "../img", type = "cairo", res = 96, dev = 0, fNameWithCapt = FALSE, gg = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{capt}{
%%     ~~Describe \code{capt} here~~
}
  \item{Width}{
%%     ~~Describe \code{Width} here~~
}
  \item{off}{
%%     ~~Describe \code{off} here~~
}
  \item{Height}{
%%     ~~Describe \code{Height} here~~
}
  \item{GraphPointSize}{
%%     ~~Describe \code{GraphPointSize} here~~
}
  \item{dirr}{
%%     ~~Describe \code{dirr} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{dev}{
%%     ~~Describe \code{dev} here~~
}
  \item{fNameWithCapt}{
%%     ~~Describe \code{fNameWithCapt} here~~
}
  \item{gg}{
%%     ~~Describe \code{gg} here~~
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
function (capt = .main, Width = dev.size(units = "px")[1], off = TRUE, 
    Height = dev.size(units = "px")[2], GraphPointSize = 12, 
    dirr = "../img", type = "cairo", res = 96, dev = 0, fNameWithCapt = FALSE, 
    gg = FALSE, ...) 
{
    op = options()
    on.exit(options(op))
    if (!file.exists(dirr)) 
        dir.create(dirr)
    .iFig = firstFreeFigN(dirr)
    GraphFileName = if (fNameWithCapt) 
        sf("Fig_\%s. \%s", .iFig, capt)
    else sf("Fig_\%s", .iFig)
    AbsGraphFileName = sf("\%s/\%s/\%s.png", gw(), dirr, GraphFileName)
    catt("HHp: printing to ", AbsGraphFileName)
    if (dev > 0) 
        dev.set(dev)
    if (gg) {
        try({
            ggsave(AbsGraphFileName)
        }, s = FALSE)
    }
    else {
        dev.copy2pdf(file = sub("png$", "pdf", AbsGraphFileName))
        dev.print(device = png, file = AbsGraphFileName, width = Width, 
            height = Height, pointsize = GraphPointSize, units = "px", 
            type = type, ...)
    }
    if (exists(".HTML.file")) 
        cat(sf("<p align=\"left\"><img src=\"img/\%s.png\"  border=\"0\" width=\"\%s\" height=\"\%s\"/><br/>\n\t\t\t\t\t\t\t\t<span class=\"caption>\%s</span><br/></p>/n", 
            GraphFileName, Width, Height, capt), file = .HTML.file, 
            append = TRUE)
    if (off) 
        dev.off()
    options(op)
    catf("Saved to: \%s. \%s\n", AbsGraphFileName, capt)
    catf("\%s. \%s\n", GraphFileName, capt)
    invisible(sf("\%s. \%s", GraphFileName, capt))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
