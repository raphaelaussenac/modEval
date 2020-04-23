################################################################################
# initialisation
################################################################################

# clean up environment
rm(list = ls())

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  setwd("/Users/raphaelaussenac/Documents/GitHub/modEval/modelsOutput")
} else if (Sys.info()["sysname"] == "Windows"){
  setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval/modelsOutput")
}

# retrieve list of file (simulations and observations)
fileNames <- Sys.glob('*.csv')

################################################################################
# calculate yearly aggregated index (N, Dg, etc) for observed and predicted data
################################################################################

for (i in fileNames){

  # load file
  site <- read.csv(i)

  # down to the year level
  df <- data.frame()
  for (yr in unique(site$year)){
    yrdf <- site[site$year == yr, ]

    # Number of trees ----------------------------------------------------------
    yrdf$N <- nrow(yrdf)

    # mean quadratic diameter --------------------------------------------------
    yrdf$Dg <- sqrt(sum(yrdf$D^2)/nrow(yrdf))

    # save
    yrdf <- yrdf[1, c('year', 'N', 'Dg')]
    df <- rbind(df, yrdf)
  }

  # write site index
  if (Sys.info()["sysname"] == "Darwin"){
    write.csv(df, file = paste('/Users/raphaelaussenac/Documents/GitHub/modEval/indexForEval/',
                                    i, sep = ''), row.names = FALSE)
  } else if (Sys.info()["sysname"] == "Windows"){
    write.csv(df, file = paste('C:/Users/raphael.aussenac/Documents/GitHub/modEval/indexForEval/',
                                    i, sep = ''), row.names = FALSE)
  }

}
