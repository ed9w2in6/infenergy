% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uni-data.R
\name{get.uni.half.hourly.data}
\alias{get.uni.half.hourly.data}
\title{Get half-hourly data from the University Energy Office's data files}
\usage{
get.uni.half.hourly.data(from, to)
}
\arguments{
\item{from}{Date from which to collect data}

\item{to}{Date to which to collect data}
}
\value{
Data frame with columns \code{Time} (the centre of the
time period) and \code{kWh} (energy use in kWh in the half hour
centred on \code{Time})
}
\description{
Get half-hourly data from the University Energy Office's data files
}
\author{
David Sterratt
}
