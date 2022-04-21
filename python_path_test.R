library(reticulate)
use_python("/home/ubuntu/anaconda3/bin/python3")
use_python("/usr/local/bin/python")

py_config()
Sys.setenv(RETICULATE_PYTHON = "/home/ubuntu/anaconda3/bin/python3")

library(reticulate)
library(reticulate)
use_python("/usr/local/bin/python3")

a <- 1

library(lubridate)
library(lsa)
install.packages('lsa')

?lsa
?isoweek
x <- ymd("2012-03-26")
week(x)
week(x) <- 1
x
week(x) <- 53
x
week(x) > 3