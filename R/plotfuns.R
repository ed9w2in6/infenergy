
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
##' @param y Matrix of heights of data at centres. One row per set of
##' data.
##' @param xlab X-axis label
##' @param ylab Y-axis label
##' @param col Vector of colours to plot each set of data.
##' @author David Sterratt
##' @export
stepplot <- function(x, y, xlab="Time", ylab="kW",
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
    xs <- as.vector(rbind(breaks, breaks))
    ys <- as.vector(rbind(c(0, yi), c(yi, 0)))
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
plot.hourly <- function(dat, col=NULL,
                        ylim=NULL, ylab="kW") {
  from <- attr(dat, "from")
  to <- attr(dat, "to")
  Time <- dat$Time
  dat <- t(subset(dat, select=-Time))
  if (is.null(col)) {
    col <- grey(seq(1, 0.5, len=nrow(dat)))
    col <- col[length(col):1]
  }
  
  if (is.null(ylim))
    ylim <- c(0, max(apply(dat, 2, sum)))
  stepplot(Time, dat, xaxt="n", col=col, ylim=ylim,
           main=paste(from, "to", to), ylab=ylab)
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

##' @title Plot energy data daily 
##' @param dat Data frame of daily data with first column containing Time
##' @param col Colours to plot non-time columns
##' @param ylim Specify ylim
##' @author David Sterratt
##' @method plot daily
##' @export
plot.daily <- function(dat, col=NULL, ylim=NULL, per.hour=FALSE, main=NA) {
  from <- attr(dat, "from")
  to <- attr(dat, "to")
  Time <- dat$Time
  dat <- t(subset(dat, select=-Time))
  if (per.hour) {
    dat <- dat/24
  }

  if (is.null(col)) {
    col <- grey(seq(1, 0.5, len=nrow(dat)))
    col <- col[length(col):1]
  }

  if (is.null(ylim))
    ylim <- c(0, max(apply(dat, 2, sum)))
  ylab <- ifelse(per.hour, "kW", "kWh per day")
  stepplot(Time, dat, xaxt="n", col=col, ylim=ylim,
       ylab=ylab, main=ifelse(is.na(main), paste(from, "to", to)), main)

  ## Lines indicating start of month
  t0 <- as.POSIXlt(Time[1]) 
  t1 <- as.POSIXlt(Time[length(Time)] + 24*60*60)
  locs <- as.POSIXct(unique(strftime(Time, "%Y-%m-01")))
  lines(rbind(locs, locs, NA), rep(c(ylim, NA), length(locs)))
  axis(1, at=locs, labels=NA)
  ## Positions for month labels
  locs <- locs + 15*24*60*60
  axis(1, at=locs, labels=strftime(locs, "%b"), tick=FALSE)
}

##' @title Plot energy data weekly 
##' @param dat Data frame of weekly data with first column containing Time
##' @param col Colours to plot non-time columns
##' @param ylim Specify ylim
##' @author David Sterratt
##' @method plot weekly
##' @export
plot.weekly <- function(dat, col=NULL, ylim=NULL, month.bars=TRUE, year.bars=FALSE, main=NULL, ylab=NULL,...) {
  from <- attr(dat, "from")
  to <- attr(dat, "to")
  Time <- dat$Time
  dat <- t(subset(dat, select=-Time))

  if (is.null(col)) {
    col <- grey(seq(1, 0.5, len=nrow(dat)))
    col <- col[length(col):1]
  }

  if (is.null(ylim))
    ylim <- c(0, max(apply(dat, 2, sum)))
  ylab <- ifelse(is.null(ylab), "kWh per week", ylab)
  stepplot(Time, dat, xaxt="n", col=col, ylim=ylim,
           ylab=ylab, bty="n", yaxt="n",
           main=ifelse(is.null(main), paste(from, "to", to), main),
           ...)

  if (month.bars) {
    ## Lines indicating start of month
    t0 <- as.POSIXlt(Time[1]) 
    t1 <- as.POSIXlt(Time[length(Time)] + 24*60*60)
    locs <- as.POSIXct(unique(strftime(Time, "%Y-%m-01")))
    print(locs)
    lines(rbind(locs, locs, NA), rep(c(ylim, NA), length(locs)))
    axis(1, at=locs, labels=NA)
    ## Positions for month labels
    locs <- locs + 15*24*60*60
    axis(1, at=locs, labels=strftime(locs, "%b"), tick=FALSE)
  }
  if (year.bars) {
    ## Lines indicating start of month
    t0 <- as.POSIXlt(Time[1])
    year <- as.numeric(strftime(t0, "%Y")) + 1
    locs <- as.POSIXct(seq(as.Date(paste0(year, "-01-01")), as.Date(Time[length(Time)]), by="year"))
    lines(rbind(locs, locs, NA), rep(c(ylim, NA), length(locs)))
    axis(1, at=locs, labels=strftime(locs, "%b %Y"), pos=0)
    axis(1, at=c(min(Time), max(Time)), labels=c(NA, NA), pos=0, tcl=0)
    axis(2, pos=min(Time), las=1)    
  }

}
