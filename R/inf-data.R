cachedir <- "cache"

##' Get one day's data from an Informatics server
##'
##' @title Get one day's data from a UPS
##' @param date Day on which to get data
##' @param ups UPS from which to get data
##' @param power.factor Power factor from which to compute real power
##' from apparent power. If this \code{NA}, use the real power
##' supplied by the UPS
##' @param cache If \code{prefer}, use cached data if available, but
##' otherwise use source data. If \code{use}, only used cached data,
##' and don't try source data. If \code{none}, don't use.
##' @return Table with columns \code{Time} and \code{kWh}
##' @author David Sterratt
##' @export
get.inf.single.ups.date <- function(date, ups="forumA", power.factor=1, cache=TRUE) {

  blank.data <- function(date) {
    warning(paste("Some data points may be missing from", ups, "data on", date))
      times <- seq.POSIXt(as.POSIXct(date)+1800,
                          as.POSIXct(as.Date(as.POSIXct(date) + 26*3600)) - 1800,
                          by="1 hour")
    return(NULL)
  }

  if (!file.exists(cachedir)) 
    dir.create(cachedir)
  
  ## Read from cache - if it exists
  cachefile <- file.path(cachedir,
                         paste(ups, "_raw_", strftime(date, "%F"), ".csv", sep=""))
  if (cache & file.exists(cachefile)) {
    dat <- read.csv(cachefile)
  } else {
    ## Otherwise, get data from system
    base.url <- "http://netmon.inf.ed.ac.uk/raw-UPS"
    year <- strftime(date, "%Y")
    if (year != strftime(Sys.time(), "%Y")) {
      base.url <- file.path(base.url, year)
    }
    file <- file.path(base.url,
                      paste(ups, "_power.raw.",
                            strftime(date, "%F"), sep=""))
    dat <- tryCatch(read.csv(file, header=FALSE),
                    error=function(e) {return(NULL)},
                    warning=function(w) {})
    if (is.null(dat)) {
      warning(paste("Could not read", ups, "data from", date))
      return(blank.data(date))
    }
    colnames(dat) <- c("UnixTime",
                       "L1V", "L2V", "L3V", # Voltage in V
                       "L1I", "L2I", "L3I", # Current in dA
                       "L1P", "L2P", "L3P", # Real power in W
                       "L1L", "L2L", "L3L") # Percentage load
    write.csv(dat, cachefile, row.names=FALSE)
  }
  #print(showConnections())
  #closeAllConnections()
  ## Convert Unix time into POSIX time
  times <- as.POSIXct(dat[,"UnixTime"], origin=as.POSIXct("1970-01-01", tz="GMT"), tz="GMT")
  dat$Time <-times
  dat$UPS <- ups

  ## Compute apparent power in VA - current is in dA; voltage is V
  dat <- dplyr::mutate(dat, L1S=L1V*L1I/10)
  dat <- dplyr::mutate(dat, L2S=L2V*L2I/10)
  dat <- dplyr::mutate(dat, L3S=L3V*L3I/10)

  if (is.na(power.factor)) {
    ## Compute power factor
    dat <- dplyr::mutate(dat, L1PF=L1P/L1S)
    dat <- dplyr::mutate(dat, L2PF=L2P/L2S)
    dat <- dplyr::mutate(dat, L3PF=L3P/L3S)
  } else {
    ## Set power factor
    dat <- dplyr::mutate(dat, L1PF=power.factor)
    dat <- dplyr::mutate(dat, L2PF=power.factor)
    dat <- dplyr::mutate(dat, L3PF=power.factor)
    ## Compute real power from apparent power
    dat <- dplyr::mutate(dat, L1P=power.factor*L1S)
    dat <- dplyr::mutate(dat, L2P=power.factor*L2S)
    dat <- dplyr::mutate(dat, L3P=power.factor*L3S)
  }
    
  ## Compute power from the the voltage and current in each of the
  ## three phases - current is in dA, voltage is V
  dat <- dplyr::mutate(dat, P.kW = (L1P + L2P + L3P)/1000)
  return(dat)
}

##' @author David Sterratt
##' @export
get.inf.single.ups <- function(from, to, ups="forumA", cache=TRUE, ...) {
  from <- as.POSIXlt(from)
  ## Create list of dates from which to get data.
  dates <- as.list(seq.Date(as.Date(trunc(as.POSIXlt(from + 1, tz="GMT"), "day")),
                            to=as.Date(to), by=1))
  d <- do.call(rbind, lapply(dates, function(d) {
                               get.inf.single.ups.date(d, ups, ...)
                             }))
  d <- subset(d, Time >= from & Time < to)
  return(d)
}

##' @title Get data in hourly chunks from an informatics UPS log.
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param ups UPS from which to collect data
##' @return Data frame containing the columns \code{Time} of centre of
##' interval, \code{kWh} energy used in that interval in kWh.
##' @author David Sterratt
##' @export
get.inf.single.ups.data.hourly <- function(from, to,
                                           ups="forumA", cache=TRUE, ...) {
  ## Get the data
  d <- get.inf.single.ups(from, to, ups, ...)
    

  ## Create bins in which to aggregate the data
  if (nrow(d) > 0) {
    bins <- cut(d$Time, "hours") # , labels=times[-1]-30*60)
    if (any(!is.na(bins))) {
      ## Agregate the data
      ad <- aggregate(P.kW ~ bins, data=d, FUN=mean)
      d <- with(ad, data.frame(Time=as.POSIXct(bins, tz="GMT"), kWh=P.kW))
    } else {
      ## If all the bins are NA due to all the data being outwith times
      d <- data.frame(matrix(0, nrow=0, ncol=2))
      colnames(d) <- c("Time", "kWh")
    }
  }
  ## print(levels(bins))
  ## print(d$Time)
  ## Bin into hourly chunks
  times <- seq.POSIXt(as.POSIXlt(from),
                      to=as.POSIXlt(to)-1800, by="1 hour")

  missing.times <- as.POSIXct(setdiff(as.POSIXct(times, tz="GMT"), d$Time),
                              origin=as.POSIXct("1970-01-01", tz="GMT"), tz="GMT")
  if (length(missing.times > 0)) {
    d <- rbind(d, data.frame(Time=missing.times, kWh=NA))
    d <- d[order(d$Time),]
  }
  return(d)
}

##' @title Get hourly data from the Informatics UPS logs
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param upss Vector of UPSs from which to collect data
##' @return Data frame with columns \code{Time} (the centre of the
##' time period) and \code{kWh} (energy use in kWh in the hour
##' centred on \code{Time})
##' @author David Sterratt
##' @export
get.inf.ups.data.hourly <- function(from, to,
                                upss=c("forumA", "forumB"), ...) {

  dat <-  get.inf.single.ups.data.hourly(from, to, upss[1], ...)
  for (ups in upss[-1]) {
    d <- get.inf.single.ups.data.hourly(from, to, ups, ...)
    dat <- cbind(dat, d$kWh)
  }
  kWh <- dat[,-1,drop=FALSE]
  if (any(is.na(dat))) {
    warning("Values being inferred")
    if (any(na.omit(diff(apply(kWh , 1, range))/rowSums(kWh)) > 0.01)) {
      stop("Data are not consistent enough for inference")
    }
  }
  ad <- dat[,1]
  ad <- data.frame(Time=dat[,1], kWh=rowMeans(kWh, na.rm=TRUE)*length(upss))
  ## rowMeans(cbind(NA, NA)) == NaN !!
  ad$kWh[is.nan(ad$kWh)] <- NA
  ## ad <- aggregate(kWh ~ Time, data=dat, FUN=sum)
  attr(ad, "from") <- from
  attr(ad, "to") <- to
  class(ad) <- c("hourly", "data.frame")
  return(ad)
}


get.inf.data.daily <- function(from, to, ups="forumA") {

  d <- get.inf.ups.data.hourly(from, to, ups)

  ## Bin into daily chunks
  dates <- seq.POSIXt(as.POSIXct(from, tz="GMT"),
                      to=as.POSIXct(to, tz="GMT")+24*3600, by="1 day")

  bins <- cut(dat$Time, dates, labels=seq.Date(as.Date(from), as.Date(to), by=1))

  ad <- aggregate(kWh ~ bins, data=d, FUN=sum)
  d <- with(ad, data.frame(Date=as.POSIXct(bins), kWh=kWh))
  
  return(d)
}

get.inf.total.daily <- function(from, to,
                            upss=c("forumA", "forumB",
                              "serverL", "serverR")) {
  dat <- NULL
  for (ups in upss) {
    d <- get.inf.data.daily(from, to, ups)
    d$ups <- ups
    dat <- rbind(dat, d)
  }
  ad <- aggregate(kWh ~ Date, data=dat, FUN=sum)
  return(ad)
}


## dat <- get.inf.data("2011-07-07", "forumA", TRUE)
## dat <- get.inf.data.hourly("2011-07-20", to="2011-07-25", ups="forumA")
## png(file="forumA-sample.png", width=800, height=600)
## plot.energy(dat)
## dev.off()
## #dat <- get.inf.data.hourly("2011-07-20", to="2011-09-09")

