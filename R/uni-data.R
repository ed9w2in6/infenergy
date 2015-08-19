##' @title Get half-hourly data from the University Energy Office's data files
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @return Data frame with columns \code{Time} (the centre of the
##' time period) and \code{kWh} (energy use in kWh in the half hour
##' centred on \code{Time})
##' @author David Sterratt
##' @export
get.uni.half.hourly.data <- function(from, to) {
  files <- list.files("/home/sterratt/admin/inf-energy/data/uni-hh/", "\\.csv$",
                      full.names=TRUE)
  dat <- NULL
  for (file in files) {
    fdat <- read.csv(file)
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
  dat <- subset(dat, (Time>=as.POSIXct(from, tz="GMT")) &
                (Time <= as.POSIXct(to, tz="GMT") + 24*3600))
  
  return(dat)
}

##' @title Get hourly data from the University Energy Office's data files
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @return Data frame with columns \code{Time} (the centre of the
##' time period) and \code{kWh} (energy use in kWh in the hour
##' centred on \code{Time})
##' @author David Sterratt
##' @export
get.uni.data.hourly <- function(from, to) {
  dat <- get.uni.half.hourly.data(from, to)
  fdat <- dat[seq(2,nrow(dat),2),]
  fdat[,2] <- fdat[,2] + dat[seq(1,nrow(dat),2),2]
  fdat[,1] <- dat[seq(1,nrow(dat),2),1] + 15*60
  attr(fdat, "from") <- from
  attr(fdat, "to") <- to
  class(fdat) <- c("hourly", "data.frame")
  return(fdat)
}

