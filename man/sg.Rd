\name{sg}
\alias{saveGraphics}
\alias{sg}
\title{sg= saveGraphics}
\usage{
sg(capt = .main, Width = dev.size(units = "px")[1], off = TRUE,
  Height = dev.size(units = "px")[2], GraphPointSize = 12,
  dirr = "../img", type = "cairo", res = 96, dev = 0,
  fNameWithCapt = FALSE, gg = FALSE, ...)
}
\arguments{
  \item{gg}{= is.ggplot}
}
\description{
wrapper for dev.print - save graphics to .png file
}

