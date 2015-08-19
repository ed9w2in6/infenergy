##' @title Get raw cumulative data from the Informatics Forum electricity meter
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @return Data frame with class \code{cumulative} containing the
##' columns \code{time} and \code{cumkwh} cumulative energy up to that
##' time in kWh. To allow conversion to hourly and daily formats there
##' will be one measurement just before the \code{from} date and one
##' just after the {to} date.
##' @author David Sterratt
##' @export
##' @examples
##' res <- get.inf.meter.data("2014-04-30", "2014-05-01")
##' with(res, plot(time[-1], diff(cumkwh), type="l"))
get.inf.meter.data <- function(from, to) {
  # Convert to POSIX times. This will give times that are a the date
  # boundaries in GMT or BST, depending on the season
  from <- as.POSIXlt(from)
  to <- as.POSIXlt(to)
  
  ## Set offset to get readings within an hour of either side
  offsets <- c(3600, 24*3600, 30*24*3600)
  ## Get data
  drv <- DBI::dbDriver("PostgreSQL")
  con <-DBI::dbConnect(drv, user="sterratt", password="PowerScript", dbname="sterratt", host="pgresearch")
  inds <- c(0, 0)
  i <- 0
  while((inds[1] == 0 || inds[2] == length(res$time))
        && (i <= length(offsets))) {
    i <- i + 1
    res <- DBI::dbGetQuery(con,
                           paste0("SELECT * FROM forum_electricity",
                                  " WHERE time > '", format(from - offsets[i], usetz=TRUE),
                                  " 'AND time <'", format(to + offsets[i], usetz=TRUE),
                                  "' ORDER BY time ASC"))
    ## Check for readings just before "from" and just after "to"
    ## dat <- approx(res$time, res$cumkwh, c(from, to))
    inds <- findInterval(c(from, to), res$time)
  }
  ## Trim records; start at end so indicies don't change
  if (inds[2] == nrow(res)) {
    warning("Data missing from the end of the record")
  } else {
    res <- res[1:(inds[2] + 1),]
  }
  if (inds[1] == 0) {
    warning("Data missing from the start of the record")
  } else {
    res <- res[inds[1]:nrow(res),]
  }
  DBI::dbDisconnect(con)
  attr(res, "from") <- from
  attr(res, "to") <- to
  class(res) <- c("cumulative", class(res))
  return(res)
}

##' @title Get data in hourly chunks from Informatics meter
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @return Data frame containing the columns \code{Time} of centre of
##' interval, \code{kWh} energy used in that interval in kWh.
##' @author David Sterratt
##' @export
get.inf.meter.data.hourly <- function(from, to) {
  
}





