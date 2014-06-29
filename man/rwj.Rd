\name{rwj}
\alias{RWorkJournal}
\alias{r2rmd2rwj}
\alias{rwj}
\title{rwj= RWorkJournal= r2rmd2rwj}
\usage{
rwj(theFile = get.theFile(), base64 = FALSE, ...)
}
\description{
R Work Journal .r -> .r.rwd + .r.nu.rwd -(knit)-> .r.kn.htm
+ .r.kn.nu.htm -> .r.htm (or .r.html with base64 figs)
}
\examples{
\dontrun{
 r2rmd2rwj(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r')
}
}

