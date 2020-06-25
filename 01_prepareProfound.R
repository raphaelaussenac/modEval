################################################################################
# initialisation
################################################################################

# clean up environment
rm(list = ls())

# library
library(plyr)
library(data.table)

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  setwd("/Users/raphaelaussenac/Documents/GitHub/modEval")
} else if (Sys.info()["sysname"] == "Windows"){
  setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval")
}

# retrieve list of file (simulations and observations)
fileNames <- Sys.glob('./data/obsAndSim/profound/*.csv')

# load data and assign model and site names to it
listsrc <- c('profound', 'landclim', '4c', 'Salem')
listsite <- c('kroof', 'solling-beech', 'solling-spruce')
alldf <- data.frame()
for (src in listsrc){
  for (i in grep(pattern = src, x = substr(fileNames, 27, nchar(fileNames)))){
    temp <- read.csv(fileNames[i])
    # detect site
    for (s in listsite){
      if(grepl(pattern = s, x = fileNames[i])){site <- s}
    }
    temp <- cbind(site, temp)
    temp$weight <- 1
    temp$src <- src
    alldf <- rbind(alldf, temp)
  }
}

write.csv(alldf, './data/allProfound.csv', row.names = FALSE)
