\name{gff}
\alias{gff}
\title{gff}
\usage{
gff(patt = " ===", pattNeg = "gff", f = get.theFile(),
  withLineNumb = TRUE)
}
\description{
grep pattern in the file
}
\examples{
gff('= func',  '^\\\\s*#',  f=system.file('rcode/WorkJournal.r',  package ='WorkJournal'))
}

