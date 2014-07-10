\name{MakeRWJournals}
\alias{MakeRWJournals}
\title{MakeRWJournals}
\usage{
MakeRWJournals(root = ".", patt = ".*", pattNeg = "^$", exec = FALSE,
  ...)
}
\description{
= make RWJournals = res<<- is produced as a side effect for
case of error in cycle!!!
}
\examples{
\dontrun{
 RWJournals= MakeRWJournals(root='M:', patt='71_UseR-2013-Tutorial.*59.zz', pattNeg='zExtraPacks|999|scripts|library|lib|fun|Base|code2HTML|86_testShiny', exec=FALSE, show=TRUE, toSave=TRUE, outSuffix='.b.htm')
}
}

