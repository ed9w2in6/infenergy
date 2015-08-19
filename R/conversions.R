##' @title Hourly representation of data
##' @param x \code{cumulative}, \code{halfhourly} \&c object
##' @author David Sterratt
##' @export
hourly <- function(x) {
  UseMethod("hourly")
}

##' @export
hourly.default <- function(x) {
}

##' @export
##' @method hourly cumulative
hourly.cumulative <- function(x) {
  ## Find the first and last time we can interpolate from
  from <- trunc(min(x$time), units="hours") + 3600
  to <- trunc(max(x$time), units="hours")
  ## Bin into hourly chunks
  times <- seq.POSIXt(from, to, by="1 hour")
  dat <- approx(x$time, x$cumkwh, times)
  out <- data.frame(Time=times[-1]-1800, kWh=diff(dat$y))
  class(out) <- c("hourly", class(out))
  attr(out, "from") <- attr(x, "from")
  attr(out, "to") <- attr(x, "to")
  return(out)
}

##' @title Daily representation of data
##' @param x \code{cumulative}, \code{hourly} \&c object
##' @author David Sterratt
##' @export
daily <- function(x) {
  UseMethod("daily")
}

##' @export
daily.default <- function(x) {
}

##' @export
##' @method daily hourly
daily.hourly <- function(x) {
  ## Find the first and last time we can interpolate from
  from <- as.POSIXlt(attr(x, "from"))
  to <- as.POSIXlt(attr(x, "to"))

  ## Bin into daily chunks, with the date boundary always being
  ## midnight in any timezone, e.g. BST or GMT
  dates <- round(seq(from, to, by="1 day"), units="day")
  bins <- cut(x$Time, dates, labels=seq.Date(as.Date(from), as.Date(to - 12*3600), by=1))

  ad <- aggregate(kWh ~ bins, data=x, FUN=sum)
  d <- with(ad, data.frame(Time=as.POSIXct(bins), kWh=kWh))
  attr(d, "from") <- from
  attr(d, "to") <- to
  class(d) <-  c("daily", class(d))
  return(d)
}

##' @export
##' @method daily cumulative
daily.cumulative <- function(x) {
  return(daily(hourly(x)))
}
