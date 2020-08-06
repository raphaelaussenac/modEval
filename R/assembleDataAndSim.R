# library
library(plyr)
library(data.table)

dataAndSim <- function(evalSite){
# retrieve list of file (simulations and observations)
  path <- paste0('./data/obsAndSim/', evalSite, '/')
  fileNames <- Sys.glob(paste0(path, '*.csv'))

# list of sources (data and models)
  listsrc <- c('data', 'landclim', '4c', 'salem')
# list of sites (only for profound)
  if(evalSite == 'profound'){
    listsite <- c('kroof', 'solling-beech', 'solling-spruce')
  }
# load data and assign model and site names to it
  alldf <- data.frame()
  for (src in listsrc){
    for (i in grep(pattern = src, x = substr(fileNames, nchar(path), nchar(fileNames)))){
      temp <- read.csv(fileNames[i])
      if(evalSite == 'profound'){
        # detect site
        for (s in listsite){
          if(grepl(pattern = s, x = fileNames[i])){site <- s}
        }
        temp <- cbind(site, temp)
        temp$weight <- 1
      }
      temp$src <- src
      alldf <- rbind(alldf, temp)
    }
  }

  out <- write.csv(alldf, paste0('./data/all_', evalSite, '.csv'), row.names = FALSE)
  return(file.exists(paste0('./data/all_', evalSite, '.csv')))
}
