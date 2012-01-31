
setup.axes <- function(Time, ylim) {
  plot(NA, NA,
       xlim=range(as.numeric(Time)), ylim=ylim,
       ylab="Energy consumption per hour (kWh/hour)", xlab="Day",
       xaxt="n")
  ## Prettier labels
  t0 <- as.POSIXlt(as.Date(Time[1]))
  t1 <- Time[length(Time)]
  locs <- seq(t0, t1, by=24*60*60)
  rect(locs[c(1, 3, 5, 7)], 0, locs[c(2, 4, 6, 8)], max(ylim),
       col="#eeeeee", border=NA)
  axis(1, at=locs, labels=NA)
  locs <- seq(t0+12*60*60, t1, by=24*60*60)
  axis(1, at=locs, labels=weekdays(locs, TRUE), tcl=0)
}

plot.energy <- function(dat, new=TRUE, ...) {
  if (new) {
    with(dat, setup.axes(Time, c(0, max(kWh))))
  }
  
  with(dat, lines(Time, kWh, ...))

}

##' @title Step plot
##' @param x Centres of data
##' @param y Heights of data at centres
##' @param xlab 
##' @param ylab 
##' @param col 
##' @author David Sterratt
stepplot <- function(x, y, xlab="Time", ylab="kWh per hour",
                     col="white", ylim=NULL, ...) {
  ## Find breaks
  n <- length(x)
  ## Create extra centres
  xv <- as.vector(x)
  cents <- c(2*xv[1]-xv[2], xv, 2*xv[n]-xv[n-1])
  breaks <- (cents[-1]+cents[-length(cents)])/2

  ## Find totals
  yi <- apply(y, 2, sum)
  if (is.null(ylim))
    ylim=c(0, max(yi))
  plot(NA, NA, xlim=range(x), ylim=ylim,
       xlab=xlab, ylab=ylab, ...)

  for (i in nrow(y):1) {
    yi <- yi 
    xs <- as.vector(rbind(breaks, breaks))
    ys <- as.vector(rbind(c(0, yi), c(yi, 0)))
    print(length(xs))
    print(length(ys))
    polygon(xs, ys, col=col[i], border=NA)
    lines(xs[-c(1, length(xs))], ys[-c(1,length(ys))])
    yi <- yi - y[i,]
  }
}

##' @title Plot energy data hourly 
##' @param dat Data frame of hourly data with first column containing Time
##' @param col Colours to plot non-time columns
##' @param ylim Specify ylim
##' @author David Sterratt
##' @method plot hourly
##' @export
plot.hourly <- function(dat, col, ylim=NULL) {
  dat <- data.frame(dat)
  Time <- dat$Time
  dat <- t(subset(dat, select=-Time))

  if (is.null(ylim))
    ylim <- c(0, max(apply(dat, 2, sum)))
  stepplot(Time, dat, xaxt="n", col=col, ylim=ylim)
  ## Prettier labels
  t0 <- as.POSIXlt(Time[1] - 30*60) 
  t1 <- as.POSIXlt(Time[length(Time)] + 30*60)
  locs <- seq(t0, t1, by=24*60*60)
  lines(rbind(locs, locs, NA), rep(c(ylim, NA), length(locs)))

  ##rect(locs[c(1, 3, 5, 7)], 0, locs[c(2, 4, 6, 8)], max(ylim),
  ##       col="#eeeeee", border=NA)
  axis(1, at=locs, labels=NA)
  locs <- seq(t0+12*60*60, t1, by=24*60*60)
  axis(1, at=locs, labels=weekdays(locs, TRUE), tcl=0)
  lines(rbind(locs, locs, NA), rep(c(ylim, NA), length(locs)), col="gray")
}
