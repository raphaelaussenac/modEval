################################################################################
# initialisation
################################################################################

# clean up environment
rm(list = ls())

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  setwd("/Users/raphaelaussenac/Documents/GitHub/modEval")
} else if (Sys.info()["sysname"] == "Windows"){
  setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval")
}

################################################################################
################################################################################
################################################################################
# choose evaluation site (profound / bauges)
evalSite <- 'profound'
################################################################################
################################################################################
################################################################################

# assemble data and simulations
source("./R/assembleDataAndSim.R")

# calculate index (N, Dg, BAI, GINI ...)
rm(list=setdiff(ls(), "evalSite"))
source("./R/indexCalc.R")

# evaluation and plots
rm(list=setdiff(ls(), "evalSite"))
source("./R/evalPlot.R")
