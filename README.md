# infenergy
Informatics energy R package

> edit: This is a personal fork (i.e. still public!).  Modified to connect to a local PostgreSQL server instead.

# Installation

1. Start R
2. Type the following:
```
install.packages("devtools")
library(devtools)
install_github("davidcsterratt/infenergy")
library(infenergy)
example("get.inf.meter.data")
