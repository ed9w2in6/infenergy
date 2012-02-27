cachedir <- "cache"

##' Get one day's data from an informatics server
##'
##' @title Get one day's data from a UPS
##' @param date Day on which to get data
##' @param ups UPS from which to get data
##' @param cache If \code{TRUE}, cache data
##' @return Table with columns \code{Time} and \code{kWh}
##' @author David Sterratt
##' @export
get.inf.data <- function(date, ups="forumA", cache=TRUE) {

  if (!file.exists(cachedir)) 
    dir.create(cachedir)
  
  ## Read from cache - if it exists
  cachefile <- file.path(cachedir,
                         paste(ups, "_raw_", strftime(date, "%F"), ".csv", sep=""))
  if (cache & file.exists(cachefile)) {
    dat <- read.csv(cachefile)
    dat$Time <- as.POSIXct(dat$Time)
    return(dat)
  }

  ## Otherwise, get data from system
  base.url <- "http://netmon.inf.ed.ac.uk/raw-UPS"
  file <- file.path(base.url,
                    paste(ups, "_power.raw.",
                          strftime(date, "%F"), sep=""))
  dat <- tryCatch(read.csv(file, header=FALSE), error=function(e) {return(NULL)})
  if (is.null(dat)) {
    return(NULL)
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
                            ups="forumA") {
  ## Create list of dates from which to get data. We need to get the
  ## extra day for the sake of BST, as we'd like to have all day
  ## boundaries on GMT.
  dates <- as.list(seq.Date(as.Date(from), to=as.Date(to)+1, by=1))

  ## Initialise output data frame
  d <- NULL

  for (date in dates) {
    ## Get the data for that date and server
    gd <- get.inf.data(date, ups)
    d <- rbind(d, gd)
  }

  ## Bin into hourly chunks
  times <- seq.POSIXt(as.POSIXct(from, tz="GMT"),
                      to=as.POSIXct(to, tz="GMT")+24*3600, by="1 hour")
  ## Create bins in which to aggregate the data
  bins <- cut(d$Time, times, labels=times[-1]-30*60)

  ## agregate the data
  ad <- aggregate(kWh ~ bins, data=d, FUN=mean)
  d <- with(ad, data.frame(Time=as.POSIXct(bins), kWh=kWh))
  
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
                                 upss=c("forumA", "forumB",
                                   "serverL", "serverR")) {
  dat <- NULL
  for (ups in upss) {
    d <- get.inf.ups.data.hourly(from, to, ups)
    d$ups <- ups
    dat <- rbind(dat, d)
  }
  ad <- aggregate(kWh ~ Time, data=dat, FUN=sum)
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

  bins <- cut(d$Time, dates, labels=seq.Date(as.Date(from), as.Date(to), by=1))
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

