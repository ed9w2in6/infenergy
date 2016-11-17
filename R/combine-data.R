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
  dat <- data.frame(Time=comps[[1]][,"Time"])
  if (inherits(tot, "hourly")) {
    dat <- data.frame(Time=tot[,"Time"])
    other <- tot$kWh
  } else {
    if (!is.null(tot)) {
      stop("tot is not an hourly class")
    }
  }
  for (n in names(comps)) {
    comp <- comps[[n]]
    if (!any(is.na(tot))) {
      if (attr(comp, "from") != attr(tot, "from")) {
        stop(paste("From date of ", n, "component doesn't match from date of total"))
      }
      if (attr(comp, "to") != attr(tot, "to")) {
        stop(paste("To date of", n, "component doesn't match to date of total"))
      }
      other <- other - comp$kWh      
    }
    col <- data.frame(comp$kWh)
    colnames(col) <- n
    dat <- data.frame(dat, col)
  }
  if (!is.null(tot)) {
    dat <- data.frame(dat, Other=other)
    attr(dat, "from") <- attr(tot, "from")
    attr(dat, "to")   <- attr(tot, "to")
  } else  {
    attr(dat, "from") <- attr(comps[[1]], "from")
    attr(dat, "to")   <- attr(comps[[1]], "to")
  }
  class(dat) <- c("hourly", "data.frame")
  return(dat)
}

