\name{getQuoteCommentStatus}
\alias{getQuoteCommentStatus}
\title{getQuoteCommentStatus}
\usage{
getQuoteCommentStatus(s, verb = FALSE)
}
\description{
set tags for single, double quotes, backticks and comments
}
\examples{
s2= getQuoteCommentStatus(s= c('1abs#bb\\n2#a"cc"\\n3aa"bb#cc\\n4`aa"b`b', '5aa"bb#cc\\n6a\\'aa#"bb')); prr(s2)
}

