##' @title Get raw cumulative data from Informatics meter
##' @param from Date from which to collect data
##' @param to Date to which to collect data
##' @return Data frame containing the columns \code{time} and
##' \code{cumkwh} cumulative energy up to that time in kWh.
##' @author David Sterratt
##' @export
##' @examples
##' res <- get.inf.meter.data("2014-04-30", "2014-05-01")
##' with(res, plot(time[-1], diff(cumkwh), type="l"))
get.inf.meter.data <- function(from, to) {
  drv <- dbDriver("PostgreSQL")
  con <-dbConnect(drv, user="sterratt", password="PowerScript", dbname="sterratt", host="pgresearch")
  res <- dbGetQuery(con, paste0("SELECT * FROM forum_electricity WHERE time > '", from, "' AND time <'", to, "'"))
  dbDisconnect(con)
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
# with(res, plot(time[-1], diff(cumkwh), type="l"))  
}





