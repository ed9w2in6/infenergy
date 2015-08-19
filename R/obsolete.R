get.uni.data <- function(file="111010 Informatics HH Data 10-11.csv") {
  dat <- read.csv(file)
  colnames(dat) <- c("Date", "kWh")
  dat$Date <- strptime(dat$Date, "%d/%m/%Y")
  return(dat)
}

get.uni.data.daily <- function(from, to) {
  dat <- get.uni.data()
  return(subset(dat,
                Date >= as.POSIXct(from) &
                Date <= as.POSIXct(to)))
}
