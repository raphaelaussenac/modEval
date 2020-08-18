library(drake)
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(viridis)

if (Sys.info()["sysname"] == "Darwin"){
  setwd("/Users/raphaelaussenac/Documents/GitHub/modEval")
} else if (Sys.info()["sysname"] == "Windows"){
  setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval")
}

# source
source("R/assembleDataAndSim.R")
source("R/standVarCalc.R")
source("R/evalMetricsCalc.R")
source('R/hetIndex.R')
source('R/msd.R')
source('R/plot.R')

################################################################################
# choose site
################################################################################

evalSite <- 'bauges'

################################################################################
################################################################################
################################################################################

plan <- drake_plan(
    IsDataFileProfound = dataAndSim(evalSite),
    IsDataIndexProfound = standVarCalc(evalSite),
    IsFiguresProfound = evalMetricsCalc(evalSite)
)
