cachedir <- "cache"

##' Get one day's data from an Informatics server
##'
##' @title Get one day's data from a UPS
##' @param date Day on which to get data
##' @param ups UPS from which to get data
##' @param cache If \code{prefer}, use cached data if available, but
##' otherwise use source data. If \code{use}, only used cached data,
##' and don't try source data. If \code{none}, don't use.
##' @return Table with columns \code{Time} and \code{kWh}
##' @author David Sterratt
##' @export
get.inf.data <- function(date, ups="forumA", cache="prefer") {

  blank.data <- function(date) {
    warning(paste("Some data points may be missing from", ups, "data on", date))
      times <- seq.POSIXt(as.POSIXct(date)+1800,
                          as.POSIXct(as.Date(as.POSIXct(date) + 26*3600)) - 1800,
                          by="1 hour")
    return(NULL)
    ## return(data.frame(UnixTime=as.numeric(times),
    ##                  L1V=NA, L2V=NA, L3V=NA,
    ##                  L1I=NA, L2I=NA, L3I=NA,
    ##                  L1P=NA, L2P=NA, L3P=NA,
    ##                  L1L=NA, L2L=NA, L3L=NA,
    ##                  Time=times, UPS=ups, kWh=NA))
  }

  if (!file.exists(cachedir)) 
    dir.create(cachedir)
  
  ## Read from cache - if it exists
  cachefile <- file.path(cachedir,
                         paste(ups, "_raw_", strftime(date, "%F"), ".csv", sep=""))
  if (cache != "none" & file.exists(cachefile)) {
    dat <- read.csv(cachefile)
    times <- as.POSIXct(dat[,"UnixTime"], origin=as.POSIXct("1970-01-01", tz="GMT"), tz="GMT")
    dat$Time <-times
    return(dat)
  }

  if (cache == "use") {
    return(NULL)
  }
  
  ## Otherwise, get data from system
  base.url <- "http://netmon.inf.ed.ac.uk/raw-UPS"
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
                     "L1V", "L2V", "L3V",
                     "L1I", "L2I", "L3I",
                     "L1P", "L2P", "L3P",
                     "L1L", "L2L", "L3L")

  ## Convert Unix time into POSIX time
  times <- as.POSIXct(dat[,"UnixTime"], origin=as.POSIXct("1970-01-01", tz="GMT"), tz="GMT")
  dat$Time <-times
  dat$UPS <- ups

  ## Compute power from the the voltage and current in each of the
  ## three phases
  dat$kWh <- with(dat, (L1V*L1I + L2V*L2I + L3V*L3I)/10/1000)
  write.csv(dat, cachefile, row.names=FALSE)
  return(dat)
}

##' Get data in hourly chunks from an Informatics energy log.
##'
##' @title Get data in hourly chunks from an informatics server
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param upss Vector of upss from which to collect data
##' @return Data frame containing the columns \code{Time} of centre of
##' interval, \code{kWh} energy used in that interval in kWh and
##' \code{server} the name of the server to which the row relates
##' @author David Sterratt
##' @export
get.inf.ups.data.hourly <- function(from, to,
                                    ups="forumA", ...) {
  ## Create list of dates from which to get data. We need to get the
  ## extra day for the sake of BST, as we'd like to have all day
  ## boundaries on GMT.
  dates <- as.list(seq.Date(as.Date(from), to=as.Date(to)+1, by=1))

  ## Initialise output data frame
  d <- data.frame(matrix(0, nrow=0, ncol=2))
  colnames(d) <- c("Time", "kWh")
  for (date in dates) {
    ## Get the data for that date and server
    gd <- get.inf.data(date, ups, ...)
    if (!is.null(gd)) {
      ## d <- rbind(d, gd)
      d <- rbind(d, gd[,c("Time","kWh")])
    } else {
      warning(paste("Some data points may be missing from", ups, "data on", date))
      ## d <- rbind(d, data.frame(Time=seq.POSIXt(as.POSIXct(from, tz="GMT"),
      ##                             to=as.POSIXct(from, tz="GMT")+23*3600,
      ##                             by="1 hour"),
      ##                          kWh=NA))
    }
  }
    
  ## Bin into hourly chunks
  times <- seq.POSIXt(as.POSIXct(from, tz="GMT"),
                      to=as.POSIXct(to, tz="GMT")+24*3600, by="1 hour")
  ## Create bins in which to aggregate the data
  if (nrow(d) > 0) {
    bins <- cut(d$Time, times, labels=times[-1]-30*60)
    if (any(!is.na(bins))) {
      ## Agregate the data
      ad <- aggregate(kWh ~ bins, data=d, FUN=mean)
      d <- with(ad, data.frame(Time=as.POSIXct(bins, tz="GMT"), kWh=kWh))
    } else {
      ## If all the bins are NA due to all the data being outwith times
      d <- data.frame(matrix(0, nrow=0, ncol=2))
      colnames(d) <- c("Time", "kWh")
    }
  }
  ## print(paste("After aggregation",nrow(d)))
  missing.times <- as.POSIXct(setdiff(times[-1]-1800, d$Time),
                              origin=as.POSIXct("1970-01-01", tz="GMT"), tz="GMT")
  if (length(missing.times > 0)) {
    d <- rbind(d, data.frame(Time=missing.times, kWh=NA))
    d <- d[order(d$Time),]
  }
  ## } else {
  ##    d <- data.frame(Time=as.POSIXct(bins, tz="GMT"), kWh=NA)
  ## }
  return(d)
}

##' @title Get hourly data from the Informatics server logs
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param upss Vector of upss from which to collect data
##' @return Data frame with columns \code{Time} (the centre of the
##' time period) and \code{kWh} (energy use in kWh in the hour
##' centred on \code{Time})
##' @author David Sterratt
##' @export
get.inf.data.hourly <- function(from, to,
                                upss=c("forumA", "forumB"), ...) {

  dat <-  get.inf.ups.data.hourly(from, to, upss[1], ...)
  for (ups in upss[-1]) {
    d <- get.inf.ups.data.hourly(from, to, ups, ...)
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

