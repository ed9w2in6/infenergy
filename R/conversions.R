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


