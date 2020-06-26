library(drake)
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(plyr)
library(viridis)

if (Sys.info()["sysname"] == "Darwin"){
  setwd("/Users/raphaelaussenac/Documents/GitHub/modEval")
} else if (Sys.info()["sysname"] == "Windows"){
  setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval")
}

source("R/assembleDataAndSim.R")
source("R/indexCalc.R")
source("R/evalPlot.R")
source('R/HetIndex.R')
plan <- drake_plan(
    IsDataFileProfound = WriteDataSim('profound'),
    IsDataIndexProfound = WriteIndex('profound'),
    IsFiguresProfound = WritePlot('profound')

)

                                                                     
