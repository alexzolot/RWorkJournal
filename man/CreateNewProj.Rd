\name{CreateNewProj}
\alias{CreateNewProj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function CreateNewProj..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateNewProj(newProj.name = "newProjTemplName", Templ.dir = system.file("newProjTemplName", package = "WorkJournal"), root = gw(), R2wd = FALSE, overOut = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newProj.name}{
%%     ~~Describe \code{newProj.name} here~~
}
  \item{Templ.dir}{
%%     ~~Describe \code{Templ.dir} here~~
}
  \item{root}{
%%     ~~Describe \code{root} here~~
}
  \item{R2wd}{
%%     ~~Describe \code{R2wd} here~~
}
  \item{overOut}{
%%     ~~Describe \code{overOut} here~~
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
function (newProj.name = "newProjTemplName", Templ.dir = system.file("newProjTemplName", 
    package = "WorkJournal"), root = gw(), R2wd = FALSE, overOut = FALSE) 
{
    sw(sf("\%s/\%s", root, newProj.name))
    dir.create("in")
    dir.create("out")
    gw()
    for (f in dir(Templ.dir, patt = "newProjTemplName.*|README.*")) {
        catt(60, f, sf("\%s/\%s/\%s", root, newProj.name, sub("zz", 
            newProj.name, f)))
        catt("f=", f)
        if (grepl("newProjTemplName|README", f) & !grepl("doc.?$", 
            f)) {
            catt("fsub")
            fsub(fin = fp(Templ.dir, f), fout = sub("newProjTemplName", 
                newProj.name, f), fileShow = FALSE, overOut = overOut, 
                newProjTemplName = sf("\%s", newProj.name), `00-00-00` = DT())
        }
        else file.copy(fp(Templ.dir, f), fp(gw(), sub("newProjTemplName", 
            newProj.name, f)), overwrite = overOut)
    }
    if (R2wd) {
        try({
            libra(R2wd)
            wdGet(newProj.name \%+\% ".docx")
            wdTitle(newProj.name)
            wdWrite(sf("Author: Alex Zolot"), TRUE)
            wdWrite(sf("Created  : \%s", DT()), TRUE)
            wdNormal("Chapter 1")
            wdHeading(2, "Chapter 2")
            wdWrite(sf("Chapter 3"), TRUE)
            wdSave(fpa(newProj.name \%+\% ".docx"))
            wdQuit()
        }, silent = TRUE)
    }
    expl()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
