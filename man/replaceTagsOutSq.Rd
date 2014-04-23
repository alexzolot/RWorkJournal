\name{replaceTagsOutSq}
\alias{replaceTagsOutSq}
\title{replaceTagsOutSq}
\usage{
replaceTagsOutSq(s = readLines("M:/85_Otto/85_Otto.r", warn = FALSE))
}
\description{
for Code2HTML: prevent < and > in R code render as HTML
tags.
}
\examples{
s= replaceTagsOutSq(c(" <aa '<bb'   `<ee`  <cc", " <aa '<bb'   `<ee`  <cc")); prr(s)
}

