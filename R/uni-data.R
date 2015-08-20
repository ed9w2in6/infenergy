##' @title Get half-hourly data from the University Energy Office's data files
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @return Data frame with columns \code{Time} (the centre of the
##' time period) and \code{kWh} (energy use in kWh in the half hour
##' centred on \code{Time})
##' @author David Sterratt
##' @export
get.uni.half.hourly.data <- function(from, to) {
  from <- as.POSIXlt(from)
  to <- as.POSIXlt(to)

  files <- list.files("/home/sterratt/admin/inf-energy/data/uni-hh/", "\\.csv$",
                      full.names=TRUE)
  dat <- NULL
  for (file in files) {
    fdat <- read.csv(file, header=FALSE)
    while (ncol(fdat) > 0) {
      d <- fdat[,1:2]
      d <- d[d[,1]!="Maximum",]
      d <- d[d[,1]!="Aggregate",]
      colnames(d) <- c("Time", "kWh")
      if (ncol(fdat) > 2) {
        fdat <- fdat[,-(1:3)]
      } else {
        fdat <- fdat[,-(1:2)]
      }
      dat <- rbind(dat, d)
    }
  }

  Time <- strptime(dat[,1], "%d/%m/%Y %H:%M", tz="GMT")
  nas <- is.na(Time)
  Time[nas] <- strptime(dat[nas,1], "%d/%m/%Y", tz="GMT")
  dat$Time <- Time-15*60
  dat <- dat[order(dat$Time),]
  dat <- unique(dat)
  dat <- subset(dat, (Time >= from) & (Time <= to))

  ## Quality check - should be a test?
  if (any(diff(dat$Time) > 30)) {
    print(dat[which(diff(dat$Time) > 30),])
    stop("Some time differences are greater than 30 minutes")
  }
  if (any(diff(dat$Time) < 0)) {
    print(dat[which(diff(dat$Time) > 0),])
    stop("Some time differences are negative")
  }
  attr(dat, "from") <- from
  attr(dat, "to") <- to
  class(dat) <- c("halfhourly", "data.frame")
  return(dat)
}
