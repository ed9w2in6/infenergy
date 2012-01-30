get.uni.data <- function(file="111010 Informatics HH Data 10-11.csv") {
  dat <- read.csv(file)
  colnames(dat) <- c("Date", "kWh")
  dat$Date <- strptime(dat$Date, "%d/%m/%Y")

  return(dat)
}

##' @title Get half-hourly data from the University Energy Office's data files
##' @param file Input file
##' @return Data frame with columns \code{Time} (the centre of the
##' time period) and \code{kWh} (energy use in kWh in the half hour
##' centred on \code{Time})
##' @author David Sterratt
##' @export
get.uni.half.hourly.data <- function(file="111010 Informatics HH Data 10-11.csv") {
  dat <- read.csv(file)
  dat <- dat[dat[,1]!="Maximum",]
  dat <- dat[dat[,1]!="Aggregate",]
  colnames(dat) <- c("Time", "kWh")

  Time <- strptime(dat[,1], "%d/%m/%Y %H:%M")
  nas <- is.na(Time)
  Time[nas] <- strptime(dat[nas,1], "%d/%m/%Y")
  dat$Time <- Time-15*60
  
  return(dat)
}

##' @title Get hourly data from the University Energy Office's data files
##' @param file Input file
##' @return Data frame with columns \code{Time} (the centre of the
##' time period) and \code{kWh} (energy use in kWh in the hour
##' centred on \code{Time})
##' @author David Sterratt
##' @export
get.uni.data.hourly <- function(file="111010 Informatics HH Data 10-11.csv") {
  dat <- get.uni.half.hourly.data(file=file)
  fdat <- dat[seq(2,nrow(dat),2),]
  fdat[,2] <- fdat[,2] + dat[seq(1,nrow(dat),2),2]
  fdat[,1] <- dat[seq(1,nrow(dat),2),1] + 15*60
    
  return(fdat)
}

get.uni.data.daily <- function(from, to) {
  dat <- get.uni.data()
  return(subset(dat,
                Date >= as.POSIXct(from) &
                Date <= as.POSIXct(to)))
}
