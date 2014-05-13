\name{c2r}
\alias{Comments2Roxygen}
\alias{c2r}
\title{c2r= Comments2Roxygen}
\usage{
c2r(file = get.theFile(), suffOut = "-copy", fout = fout <-
  gsub("\\\\.r$", suffOut \%+\% ".r", file), exec = TRUE, ...)
}
\description{
transform Comments to Roxygen
}
\examples{
\dontrun{
 c2r(file= get.theFile(),  suffOut='-copy',  exec=FALSE)
}
}

