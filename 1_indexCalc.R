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
fileNames <- Sys.glob('./modelsOutput/*.csv')

# load data and assign model and site names to it
listsrc <- c('profound', 'landclim', '4c', 'Salem')
listsite <- c('kroof', 'solling-beech', 'solling-spruce')
alldf <- data.frame()
for (src in listsrc){
  for (i in grep(pattern = src, x = fileNames)){
    temp <- read.csv(fileNames[i])
    # detect site
    for (s in listsite){
      if(grepl(pattern = s, x = fileNames[i])){site <- s}
    }
    temp$src <- src
    temp$site <- site
    alldf <- rbind(alldf, temp)
  }
}

################################################################################
# calculate yearly aggregated index (N, Dg, etc) for observed and predicted data
# at sp level
################################################################################

# sp N, Dg, H, BA
sp <- ddply(alldf, .(year, src, site, species), summarise, N = length(D_cm),
                                                 Dg = sqrt(sum(D_cm^2)/length(D_cm)),
                                                 H = sum(H_m) / length(H_m),
                                                 BA = sum(pi * D_cm^2 / 4) / 10000,
                                                 V = sum(V_m3))
#
# sp BAI
# first define lag function
lg <- function(x)c(NA, x[1:(length(x)-1)])
# convert sp into data.table format
sp <- data.table(sp)
# create lagged BA and year
sp[,BAlag := lg(BA), by = c('src', 'site', 'species')]
sp[,yearlag := lg(year), by = c('src', 'site', 'species')]


# calculate mean BAI/yr on same period than in PROFOUND dataset
# for all models
# convert back to data.frame
sp <- data.frame(sp)
modelDf <- data.frame()
for (mod in unique(alldf$src)[unique(alldf$src) != 'profound']){
  for (site in unique(alldf$site)){
    for (s in unique(alldf[alldf$site == site & alldf$src == mod, 'species'])){
      # retrieve start year of each growth period
      dfP <- sp[sp$src == 'profound' & sp$site == site & sp$species == s,]
      df <- sp[sp$src == mod & sp$site == site & sp$species == s,]
      df <- merge(df, dfP[, c('year', 'yearlag')], by = 'year', all.x = TRUE)
      df$BAI_yr <- NA
      #
      for (end in df[!is.na(df$yearlag.y), 'year']){
        # if start date exists in df
        start <- df[df$year == end, 'yearlag.y']
        if (start %in% df[!is.na(df$BA), 'year']){
          df[df$year == end, 'BAI_yr'] <- (df[df$year == end, 'BA'] - df[df$year == start, 'BA']) / (end - start)
        }
      }
      modelDf <- rbind(modelDf, df)
    }
  }
}
modelDf <- modelDf[, c('year', 'src', 'site', 'species', 'N', 'Dg', 'H', 'BA', 'V', 'BAI_yr')]

# calculate mean BAI/yr on PROFOUND dataset
profDf <- sp[sp$src == 'profound',]
profDf$BAI_yr <- (profDf$BA - profDf$BAlag) / (profDf$year - profDf$yearlag)
profDf <- profDf[, c('year', 'src', 'site', 'species', 'N', 'Dg', 'H', 'BA', 'V', 'BAI_yr')]

# merge profound + models
sp <- rbind(modelDf, profDf)


################################################################################
# calculate yearly aggregated index (N, Dg, etc) for observed and predicted data
# at stand level
################################################################################

# stand N, Dg, H, BA
stand <- ddply(alldf, .(year, src, site), summarise, N = length(D_cm),
                                                 Dg = sqrt(sum(D_cm^2)/length(D_cm)),
                                                 H = sum(H_m) / length(H_m),
                                                 BA = sum(pi * D_cm^2 / 4) / 10000,
                                                 V = sum(V_m3))
stand$species <- 'allsp'
stand$paste <- paste(stand$year, stand$src, stand$site, sep = '')

# stand BAI
standBAI <- ddply(sp, .(year, src, site), summarise, BAI_yr = sum(BAI_yr))
standBAI$paste <- paste(standBAI$year, standBAI$src, standBAI$site, sep = '')

# merge stand and standBAI
stand <- merge(stand, standBAI[, c('BAI_yr', 'paste')], by = 'paste')
stand <- stand[, c('year', 'src', 'site', 'species', 'N', 'Dg', 'H', 'BA', 'V', 'BAI_yr')]

# merge sp and stand
sp <- rbind(sp, stand)

################################################################################
# calculate heterogeneity index at stand level
################################################################################

# Import functions to calculate heterogeneity index
if (Sys.info()["sysname"] == "Darwin"){
  source("/Users/raphaelaussenac/Documents/GitHub/modEval/R/HetIndex.R")
} else if (Sys.info()["sysname"] == "Windows"){
  source("C:/Users/raphael.aussenac/Documents/GitHub/modEval/R/HetIndex.R")
}

Nvar <- "D_cm"
Inter <- 10
model <- c('profound', '4c', 'landclim', 'Salem')
site <- c('kroof','solling-spruce', 'solling-beech')
out <- ReturnHill(Nvar, model, site, Inter)
out <- data.frame(out)
out[, c('N', 'Inter', 'Nvar')] <- NULL

# add heterogeneity index to df
sp <- merge(sp, out, by.x= c('year', 'src', 'site'), by.y = c('year', 'model', 'site'))

# write site index
if (Sys.info()["sysname"] == "Darwin"){
  write.csv(sp, file = paste('/Users/raphaelaussenac/Documents/GitHub/modEval/indexForEval/evalVarSp.csv', sep = ''), row.names = FALSE)
} else if (Sys.info()["sysname"] == "Windows"){
  write.csv(sp, file = paste('C:/Users/raphael.aussenac/Documents/GitHub/modEval/indexForEval/evalVarSp.csv', sep = ''), row.names = FALSE)
}
