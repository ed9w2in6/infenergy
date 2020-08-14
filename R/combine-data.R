##' @title Combine hourly data
##' @param comps list of components of hourly energy in
##'   \code{\link{hourly}} format.
##' @param tot Total energy generated in \code{\link{hourly}} format.
##' @return data frame with class \code{hourly} containing the
##'   components and the residual Other energy use
##' @author David Sterratt
##' @export
##' @examples
##' from <- "2016-09-01"
##' to <- "2016-09-08"
##' server <- get.ups.hourly(from, to, upss=c("serverL", "serverR"), power.factor=NA)
##' forum <-  get.ups.hourly(from, to, upss=c("forumB"), power.factor=0.9)
##' meter <- hourly(get.inf.meter.data(from, to))
##' combine <- combine.data.hourly(list(Server=server, Forum=forum), tot=meter)
##' plot(combine)
combine.data.hourly <- function(comps=list(), tot=NULL) {
  if (!inherits(tot, "hourly") && !is.null(tot))
    stop("tot is not an hourly class")

  ## Rename kWh headings of total and components in preparation for
  ## merging
  tot <- plyr::rename(tot, c(kWh="Total"))
  for (n in names(comps)) {
    comps[[n]] <- plyr::rename(comps[[n]], c(kWh=n))
  }

  ## Merge components and total into one data frame
  dat <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Time", all.x = TRUE),
                c(comps, list(tot)))

  ## Create "Other" column
  if (!is.null(tot)) {
    dat$Other <- dat[,"Total"] - rowSums(dat[,names(comps)], na.rm=TRUE)
  }
  dat$Total <- NULL

  ## Remove NAs
  dat[,names(comps)] <- apply(dat[,names(comps)], 2,
                              function(x) { replace(x, is.na(x), 0) })

  ## Set from and to attributes; conversions needed for compatability
  from <- as.POSIXlt(as.character(
    min(as.Date(sapply(c(comps, list(tot)),
                       function(x) {as.character(attr(x, "from"))})))), tz='GMT')
  to <- as.POSIXlt(as.character(
    max(as.Date(sapply(c(comps, list(tot)),
                       function(x) {as.character(attr(x, "to"))})))), tz='GMT')
  attr(dat, "from") <- from
  attr(dat, "to")   <- to
  
  class(dat) <- c("hourly", "data.frame")
  return(dat)
}

