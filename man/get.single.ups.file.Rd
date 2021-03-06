% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ups-data.R
\name{get.single.ups.file}
\alias{get.single.ups.file}
\title{Get  data from a single UPS from Netmon logfiles}
\usage{
get.single.ups.file(from, to, ups = "forumA", cache = TRUE)
}
\arguments{
\item{from}{Date from which to collect data}

\item{to}{Date to which to collect data}

\item{ups}{UPS from which to get data}

\item{cache}{If \code{prefer}, use cached data if available, but
otherwise use source data. If \code{use}, only used cached data,
and don't try source data. If \code{none}, don't use.}
}
\value{
Table with columns \code{Time} and \code{kWh}
}
\description{
Get  data from a single UPS from Netmon logfiles
}
\author{
David Sterratt
}
