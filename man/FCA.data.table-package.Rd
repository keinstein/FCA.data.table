\name{FCA.data.table-package}
\alias{FCA.data.table-package}
\alias{FCA.data.table}
\docType{package}
\title{Formal Concept Analysis with data.table}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab FCA.data.table\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-08-18\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Tobias Schlemmer

Maintainer: Tobias Schlemmer <keinstein@users.sourceforge.net>
}
\references{
~~ Literature or other references for background information ~~
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
library("data.table")
library("FCA.data.table")
hostelmatrix <- as.logical(c(
#   hostelworld  hestels      hostelbookers
#   c y l s f c  c y l s f c  c y l s f c
    0,0,1,0,0,0, 0,0,1,1,0,0, 0,0,1,1,0,0, # Nuevo Suizo
    0,1,1,1,0,1, 0,1,1,1,0,1, 1,1,1,1,0,1, # Samay
    1,1,1,1,0,1, 1,1,1,1,1,1, 0,1,1,1,1,1, # Oasis Backpacker
    1,1,0,1,0,1, 1,1,1,1,1,1, 1,1,1,1,1,1, # One
    1,1,0,1,0,1, 1,1,1,1,1,1, 1,1,1,1,1,1, # Ole Backpacker
    0,0,1,1,0,1, 1,1,1,1,1,1, 1,1,1,1,0,1  # Garden Backpacker
))

hm1 <- expand.grid(object = c("Nuevo Suizo","Samay","Oasis Backpacker",
                       "One","Ole Backpacker","Garden Backpacker"),
                   condition = c("hostelworld","hostels","hostelbookers"),
                   attribute = c("character","safety","location",
                       "staff","fun","cleanliness"),
                   stringsAsFactors=FALSE)

hostels <- tricontext.data.table(hm1[hostelmatrix,],
                                 objects = unique(hm1[,"object"]),
                                 attributes = unique(hm1[,"attribute"]),
                                 conditions = unique(hm1[,"condition"])
                                 )

he <- extent(hostels,c("Nuevo Suizo",
                       "Samay",
                       "Oasis Backpacker",
                       "Garden Backpacker"))
hm <- modus(hostels,c("hostelbookers"))
class(hm)
class(he)
debug(intent)
intent(he,hm)
}
