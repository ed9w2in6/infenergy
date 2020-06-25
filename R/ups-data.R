cachedir <- "cache"

##' @title Get one day's data from a single UPS from Netmon logfiles
##' @param date Day on which to get data
##' @param ups UPS from which to get data
##' @param cache If \code{prefer}, use cached data if available, but
##' otherwise use source data. If \code{use}, only used cached data,
##' and don't try source data. If \code{none}, don't use.
##' @return Table with columns \code{Time} and \code{kWh}
##' @author David Sterratt
##' @export
get.single.ups.file.one.day <- function(date, ups="forumA", cache=TRUE) {
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
    con <- NULL
    if (year != strftime(Sys.time(), "%Y")) {
      file <- file.path(file.path(base.url, year),
                        paste(ups, "_power.raw.",
                              strftime(date, "%F"),
                              ".gz", sep=""))
      message(paste("Trying", file))
      tfile <- tempfile()
      ec <- tryCatch(download.file(file, tfile),
                     error=function(e) {
                       message(paste(file, "does not exist"))
                       return(1)
                     })
      if (ec == 0) {
        con <- gzfile(tfile)
      }
    }
    if (is.null(con)) {
      file <- file.path(base.url,
                        paste(ups, "_power.raw.",
                              strftime(date, "%F"),
                              sep=""))
      message(paste("Trying", file))
      con <- url(file)
    }
    dat <- NULL
    dat <- tryCatch(read.csv(con, header=FALSE),
                    error=function(e) {
                      close(con)
                      return(NULL)
                    },
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

    times <- as.POSIXct(dat[,"UnixTime"], origin=as.POSIXct("1970-01-01", tz="GMT"), tz="GMT")
    ## Convert Unix time into POSIX time
    dat$Time <-times
    dat$UPS <- ups
    write.csv(dat, cachefile, row.names=FALSE)
  }
  return(dat)
}

##' @title Get  data from a single UPS from Netmon logfiles
##' @author David Sterratt
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param ups UPS from which to get data
##' @param cache If \code{prefer}, use cached data if available, but
##' otherwise use source data. If \code{use}, only used cached data,
##' and don't try source data. If \code{none}, don't use.
##' @return Table with columns \code{Time} and \code{kWh}
##' @export
get.single.ups.file <- function(from, to, ups="forumA", cache=TRUE) {
  ## Create list of dates from which to get data.
  dates <- as.list(seq.Date(as.Date(trunc(as.POSIXlt(from + 1, tz="GMT"), "day")),
                            to=as.Date(to), by=1))

  dat <- do.call(rbind, lapply(dates, function(d) {
    get.single.ups.file.one.day(d, ups, cache=cache)
  }))
  dat <- subset(dat, Time >= from & Time < to)
  return(dat)
}

##' @title Dump UPS data to database
##' @author David Sterratt
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param ups UPS from which to get data
##' @param dry.run If \code{TRUE} do not dump data, but instead report
##'   on what owould be done
##' @export
dump.single.ups.to.db <- function(from, to, ups="forumA", dry.run=FALSE) {
  from <- as.POSIXlt(from)
  ## Create list of dates from which to get data.
  dates <- as.list(seq.Date(as.Date(trunc(as.POSIXlt(from + 1, tz="GMT"), "day")),
                            to=as.Date(to), by=1))
  drv <- DBI::dbDriver("PostgreSQL")
  con <-DBI::dbConnect(drv, user="postgres", dbname="UOE", host="localhost")
  tabname <- "forum_ups"
  for (d in dates) {
    ## Get data from file
    dat <- get.single.ups.file.one.day(d, ups, cache=FALSE)

    ## Make sure we don't add data already in the db
    dat.db <- get.single.ups.db(d - 1, d + 1, ups)
    dat <- subset(dat, !(UnixTime %in% dat.db$UnixTime))
    if (dry.run) {
      if (nrow(dat) > 0) {
        message(paste("Date:", d, "would add:"))
        print(summary(dat))
      } else {
        message(paste("All data already in db on", d))
      }
    } else {
      if (!is.null(dat)) {
        if (nrow(dat) > 0) {
          message(paste("Writing to db", d))
          DBI::dbWriteTable(con, tabname, dat,
                            append=DBI::dbExistsTable(con, tabname),
                            row.names=FALSE)
        } else {
          message(paste("All data already in db on", d))
        }
      }
    }
  }
  DBI::dbDisconnect(con)
}
##dump.single.ups.to.db("2011-07-06", "2015-08-20", ups="serverL")
##dump.single.ups.to.db("2011-07-06", "2015-08-20", ups="serverR")
##dump.single.ups.to.db("2011-07-06", "2013-01-03", ups="forumA")
##dump.single.ups.to.db("2013-01-03", "2013-03-30", ups="forumA")
##dump.single.ups.to.db("2011-07-06", "2015-08-20", ups="forumB")
## There is a gap in the data on 2013-01-17
##dump.single.ups.to.db("2013-03-18", "2015-08-20", ups="forumB")
## dump.single.ups.to.db("2011-07-06", "2015-08-20", ups="forumB")

##' @title Get data for a single UPS from database
##' @author David Sterratt
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param ups UPS from which to get data
##' @export
get.single.ups.db <- function(from, to, ups="forumA") {
  drv <- DBI::dbDriver("PostgreSQL")
  con <-DBI::dbConnect(drv, user="postgres", dbname="UOE", host="localhost")
  d <- DBI::dbGetQuery(con, paste0("SELECT * FROM forum_ups",
                                   " WHERE \"Time\" > '", format(from, usetz=TRUE), "'",
                                   " AND   \"Time\" < '", format(to  , usetz=TRUE), "'",
                                   " AND   \"UPS\" LIKE '", ups , "'",
                                   " ORDER BY \"Time\" ASC"))
  DBI::dbDisconnect(con)
  return(d)
}


##' @title Get data from a UPS
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param ups UPS from which to get data
##' @param method If \code{db}, collect from the database; otherwise
##'   from the source UPS files
##' @param power.factor Power factor from which to compute real power
##'   from apparent power. If this \code{NA}, use the real power
##'   supplied by the UPS
##' @param ... Arguments passed to \code{\link{get.single.ups.file}}
##' @return Table with columns \code{Time} and \code{kWh}
##' @author David Sterratt
##' @export
get.single.ups <- function(from, to, ups="forumA", method="db", power.factor=1, ...) {
  from <- as.POSIXlt(from, tz='GMT')
  to <- as.POSIXlt(to, tz='GMT')

  if (method=="db") {
    d <- get.single.ups.db(from, to, ups=ups)
  } else {
    d <- get.single.ups.file(from, to, ups=ups, ...)
  }
  if (nrow(d) == 0) {
    warning(paste("No data from", ups))
    return(d)
  }

  ## Compute apparent power in VA - current is in dA; voltage is V
  d <- plyr::mutate(d, L1S=L1V*L1I/10)
  d <- plyr::mutate(d, L2S=L2V*L2I/10)
  d <- plyr::mutate(d, L3S=L3V*L3I/10)

  if (is.na(power.factor)) {
    ## Compute power factor
    d <- plyr::mutate(d, L1PF=L1P/L1S)
    d <- plyr::mutate(d, L2PF=L2P/L2S)
    d <- plyr::mutate(d, L3PF=L3P/L3S)
  } else {
    ## Set power factor
    d <- plyr::mutate(d, L1PF=power.factor)
    d <- plyr::mutate(d, L2PF=power.factor)
    d <- plyr::mutate(d, L3PF=power.factor)
    ## Compute real power from apparent power
    d <- plyr::mutate(d, L1P=power.factor*L1S)
    d <- plyr::mutate(d, L2P=power.factor*L2S)
    d <- plyr::mutate(d, L3P=power.factor*L3S)
  }

  ## Compute power from the the voltage and current in each of the
  ## three phases - current is in dA, voltage is V
  d <- plyr::mutate(d, P.kW = (L1P + L2P + L3P)/1000)

  return(d)
}

##' @title Get data in hourly chunks from an informatics UPS log.
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @param ups UPS from which to collect data
##' @param ... Arguments passed to \code{\link{get.single.ups}} and
##'   \code{\link{get.single.ups.file}}.
##' @return Data frame containing the columns \code{Time} of centre of
##'   interval, \code{kWh} energy used in that interval in kWh.
##' @author David Sterratt
##' @export
get.single.ups.hourly <- function(from, to,
                                           ups="forumA", ...) {
  ## Get the data
  d <- get.single.ups(from, to, ups, ...)


  ## Create bins in which to aggregate the data
  if (nrow(d) > 0) {
    ## Have to do this in GMT for stable labels
    bins <- cut(as.POSIXlt(d$Time, tz="GMT"), "hours") # , labels=times[-1]-30*60)
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
##' @param infer.missing.data If one UPS in upss returns NA and the
##'   other doesn't try to infer the value of the missing UPS by
##'   assuming it is the same as the functioning UPS.
##' @param ... Arguments passed to \code{\link{get.single.ups.hourly}}
##' @return Data frame with columns \code{Time} (the centre of the
##'   time period) and \code{kWh} (energy use in kWh in the hour
##'   centred on \code{Time})
##' @author David Sterratt
##' @export
get.ups.hourly <- function(from, to,
                           upss=c("forumA", "forumB"),
                           infer.missing.data=TRUE,
                           ...) {

  dat <-  get.single.ups.hourly(from, to, upss[1], ...)
  for (ups in upss[-1]) {
    d <- get.single.ups.hourly(from, to, ups, ...)
    dat <- cbind(dat, d$kWh)
  }
  kWh <- dat[,-1,drop=FALSE]
  if (infer.missing.data) {
    if (any(is.na(dat))) {
      warning("Values being inferred")
      if (any(na.omit(diff(apply(kWh , 1, range))/rowSums(kWh)) > 0.01)) {
        stop("Data are not consistent enough for inference")
      }
    }
    ad <- data.frame(Time=dat[,1], kWh=rowMeans(kWh, na.rm=TRUE)*length(upss))
  } else {
    ad <- data.frame(Time=dat[,1], kWh=rowSums(kWh, na.rm=TRUE))
  }
  ## rowMeans(cbind(NA, NA)) == NaN !!
  ad$kWh[is.nan(ad$kWh)] <- NA
  ## ad <- aggregate(kWh ~ Time, data=dat, FUN=sum)
  attr(ad, "from") <- from
  attr(ad, "to") <- to
  class(ad) <- c("hourly", "data.frame")
  return(ad)
}
