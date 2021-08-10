#
# sideeffects_load.R
#
# created on Tue Oct  1 10:09:18 2019
# Maria Neumeier, <maria dot neumeier at bli dot uzh dot ch>
#-----------------------------------------------------------------------
#
# load functions
source("sideeffects_func.R")


#wg <- read_csv("../data/sideeffects.csv")

# now the data is already in long format
wgl <- read_csv("../data/sideeffects_all.csv")

# excluded data (missing sds)
wgexcl <- read_csv("../data/sideeffects_excl.csv")
wgorig <- read_excel("../data/sideeffects_corrected.xlsx")
