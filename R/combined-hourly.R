source("inf-servers.R")
source("hh-data.R")
source("plotfuns.R")

from <- "2011-10-17"
to <-   "2011-10-23"

## tot <- get.hh.data.hourly(from, to)
tot <- get.hh.data.hourly(file.path("inf-hh-data", paste(from, ".csv", sep="")))
forum <- get.total.hourly(from, to, c("forumA","forumB"))
server <- get.total.hourly(from, to, c("serverL","serverR"))
other <- data.frame(Time=tot$Time, kWh=tot$kWh - server$kWh - forum$kWh)
##tot <- list()
##tot$kWh <- server$kWh + forum$kWh 
dat <- rbind(Server=server$kWh,
             Forum=forum$kWh,
             Other=tot$kWh - server$kWh - forum$kWh)
# setup.axes(forum$Time, c(0, 150))
## png(file="combined-aug-2011.png", width=800, height=600)
names <- strftime(tot$Time, "%H")
times <- names
times[!(names=="00" | names=="12")] <- NA
## barplot(dat, names.arg=times, xlab="Time", ylab="kWh per hour",
##         col=c("red", "orange", "yellow"),
##         legend.text=c("ServerL+R", "ForumA+B", "Other"),
##         args.legend = list(inset = c(0, -0.12), x="topright"), add=FALSE)

plot.hourly(tot$Time, dat)

## pie(c(server=sum(server$kWh), forum=sum(forum$kWh), other=sum(tot$kWh - server$kWh - forum$kWh)))

## comb <- with(tot, data.frame(Date, t(dat)))
## write.csv(comb, "combined-aug-2011.csv", row.names=FALSE)


