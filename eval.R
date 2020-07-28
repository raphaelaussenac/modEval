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

# source
source("R/assembleDataAndSim.R")
source("R/indexCalc.R")
source("R/evalPlot.R")
source('R/HetIndex.R')

# assemble data and simulations
WriteDataSim('bauges')

# calculate index (N, Dg, BAI, GINI ...)
WriteIndex('bauges')

# evaluation and plots
WritePlot('bauges')
