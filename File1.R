
# File:  File1.R.

# This file is for learning how to handle missing data in R.

# The first step will be running Little's Test for MCAR.

####  Load Packages, set up path                    ####

here::i_am(path = "inst/File1.Rmd")

library(xfun)         # for Rscript_call()
library(devtools)     # for session_info()
library(rstudioapi)   # for versionInfo()
library(here)         # for here()
library(rmarkdown)    # for render()
library(knitr)        # for kable()
library(lubridate)    # for today()
library(magrittr)     # for the pipe
library(ggplot2)      # for graphs


#### END OF Load Packages , set up path                          ####

#### Import and examine data                         ####

data_file <- read.csv(file = here("inst/extdata","Sample_Data.csv"))

head(data_file)
tail(data_file)


lapply(data_file, summary)



#### END of Import and examine data                         ####

