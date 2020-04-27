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
  setwd("/Users/raphaelaussenac/Documents/GitHub/modEval/modelsOutput")
} else if (Sys.info()["sysname"] == "Windows"){
  setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval/modelsOutput")
}

# retrieve list of file (simulations and observations)
fileNames <- Sys.glob('*.csv')

# load data and assign model and site names to it
listsrc <- c('profound', 'landclim', '4c')
listsite <- c('kroof', 'solling_beech', 'solling_spruce')
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
# at stand level
################################################################################

# stand N, Dg, H, BA
stand <- ddply(alldf, .(year, src, site), summarise, N = length(D),
                                                 Dg = sqrt(sum(D^2)/length(D)),
                                                 H = sum(H) / length(H),
                                                 BA = sum(pi * D^2 / 4) / 10000)
# stand BAI
# first define lag function
lg <- function(x)c(NA, x[1:(length(x)-1)])
# convert stand into data.table format
stand <- data.table(stand)
# create lagged variable
stand[,BAI := lg(BA), by = c('src', 'site')]
stand$BAI <- stand$BA - stand$BAI
stand$species <- 'allsp'
stand <- stand[, c('year', 'src', 'site', 'species', 'N', 'Dg', 'H', 'BA', 'BAI')]

# write site index
# if (Sys.info()["sysname"] == "Darwin"){
#   write.csv(stand, file = paste('/Users/raphaelaussenac/Documents/GitHub/modEval/indexForEval/evalVar.csv', sep = ''), row.names = FALSE)
# } else if (Sys.info()["sysname"] == "Windows"){
#   write.csv(stand, file = paste('C:/Users/raphael.aussenac/Documents/GitHub/modEval/indexForEval/evalVar.csv', sep = ''), row.names = FALSE)
# }

################################################################################
# calculate yearly aggregated index (N, Dg, etc) for observed and predicted data
# at sp level
################################################################################

# stand N, Dg, H, BA
sp <- ddply(alldf, .(year, src, site, species), summarise, N = length(D),
                                                 Dg = sqrt(sum(D^2)/length(D)),
                                                 H = sum(H) / length(H),
                                                 BA = sum(pi * D^2 / 4) / 10000)
# stand BAI
# convert stand into data.table format
sp <- data.table(sp)
# create lagged variable
sp[,BAI := lg(BA), by = c('src', 'site', 'species')]
sp$BAI <- sp$BA - sp$BAI
sp <- rbind(stand, sp)

# write site index
if (Sys.info()["sysname"] == "Darwin"){
  write.csv(sp, file = paste('/Users/raphaelaussenac/Documents/GitHub/modEval/indexForEval/evalVarSp.csv', sep = ''), row.names = FALSE)
} else if (Sys.info()["sysname"] == "Windows"){
  write.csv(sp, file = paste('C:/Users/raphael.aussenac/Documents/GitHub/modEval/indexForEval/evalVarSp.csv', sep = ''), row.names = FALSE)
}






#
# stand[stand$src == 'profound' & stand$site == "kroof",]
#
#
#
# # list of species
# specieslist <- unique(alldf$species)
#
# #
# for (src in listsrc){
#   # at each site
#   for (s in listsite){
#     site <- alldf[alldf$src == src & alldf$site == s, ]
#     # down to the year level
#     df <- data.frame()
#     for (yr in unique(site$year)){
#       yrdf <- site[site$year == yr, ]
#
#       # Number of trees --------------------------------------------------------
#       yrdf$N <- nrow(yrdf)
#       # per sepcies at kroof site
#       # if (s == 'kroof'){
#       #   sp <- ddply(yrdf, .(species), summarise, Nsp = length(D))
#       # }
#
#       # mean quadratic diameter ------------------------------------------------
#       yrdf$Dg <- sqrt(sum(yrdf$D^2)/nrow(yrdf))
#
#       # arithmetic mean height -------------------------------------------------
#       yrdf$H <- sum(yrdf$H) / nrow(yrdf)
#
#       # basal area -------------------------------------------------------------
#       yrdf$BA <- sum(pi * yrdf$D^2 / 4) / 10000 # convert in m2
#
#       # save
#       yrdf <- yrdf[1,]
#       yrdf[, c('species', 'D')] <- NULL
#
#       # yrdf <- yrdf[1, c('year', 'N', 'Dg', 'H', 'BA')]
#       df <- rbind(df, yrdf)
#     }
#
#     # basal area increment -------------------------------------------------------
#     df$BAI <- c(NA, df$BA[-length(df$BA)])
#     df$BAI <- df$BA - df$BAI
#
#     # write site index
#     if (Sys.info()["sysname"] == "Darwin"){
#       write.csv(df, file = paste('/Users/raphaelaussenac/Documents/GitHub/modEval/indexForEval/',
#                                       src, '_', s, '.csv', sep = ''), row.names = FALSE)
#     } else if (Sys.info()["sysname"] == "Windows"){
#       write.csv(df, file = paste('C:/Users/raphael.aussenac/Documents/GitHub/modEval/indexForEval/',
#                                       src, '_', s, '.csv', sep = ''), row.names = FALSE)
#     }
#
#   }
# }
