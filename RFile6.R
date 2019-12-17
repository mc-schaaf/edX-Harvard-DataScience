library(tidyverse)


### Section 1
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url, col_names = FALSE)
nrow(dat)
ncol(dat)
url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
dat <- read_table(url, skip = 56)
nrow(dat)
ncol(dat)
