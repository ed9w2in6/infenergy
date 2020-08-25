# infenergy
Informatics energy R package

> ~edit: This is a private fork (**_Please Make sure this is true!_**).~
> forked from: [davidcsterratt/infenergy](https://github.com/davidcsterratt/infenergy)

# modified 

- Modified to connect to a PostgreSQL server setup at `localhost` instead.
- Fixed timezone error when working internationally by explicitly specifying timezone parameter `tz` for all `as.POSIXlt`.
- This readme file.

# requirements

- Need R
- Need access to the PostgreSQL server at localhost.
Can set this up at localhost from a dump of the one at Edinburgh University to speed up things.

Most data are accessable, most functions are usable except `get.uni.half.hourly.data` because:

```R
## .
## .
## .
get.uni.half.hourly.data <- function(from, to) {
  from <- as.POSIXlt(from, tz="GMT")
  to <- as.POSIXlt(to, tz="GMT")

  files <- list.files("/home/sterratt/admin/inf-energy/data/uni-hh/", "\\.csv$",
                      full.names=TRUE)
## .
## .
## .            
```
As we can see right of access to the directories along the path `/home/sterratt/admin/inf-energy/data/uni-hh/` is required.
And thus this function can only be used at DICE

# Installation

> Requires the accessibility of this repository to be set to _**"public"**_
1. Start R
2. Type the following:

```R
install.packages("devtools")
library(devtools)
install_github("ed9w2in6/infenergy")
library(infenergy)
example("get.inf.meter.data")
```

# Un-installation

1. Start R
2. Type the following:

```R
library(devtools)
remove.packages("infenergy")
```
