# load packages
library(dplyr)

# clean up environment
rm(list = ls())

# set work directory
setwd("/Users/raphael.aussenac/Documents/GitHub/modEval/data/obsAndSim/profound")

# load data
df <- read.csv("./samsara_raw/outputTreeList_Profound.txt", sep = '\t')

# change species name to comply with PROFOUND code
spcode <- data.frame('species' = c('Picea_abies', 'Pinus_sylvestris', 'Fagus_sylvatica', 'Quercus_robur', 'Acer_platanoides', 'Larix_decidua'),
                     'code' = c('piab', 'pisy', 'fasy', 'quro', 'acpl', 'lade'))
df <- merge(df, spcode, by.x = 'speciesName', by.y = 'species')

# retrieve site name
df[df$simulation == 1, "simulation"] <- 'solling-beech'
df[df$simulation == 2, "simulation"] <- 'solling-spruce-above'
df[df$simulation == 3, "simulation"] <- 'solling-spruce-below'
df[df$simulation == 4, "simulation"] <- 'kroof'

# keep only post-thinning values
# 1 - retrieve thinning dates
thin <- df[grep(pattern = 'Above|Bellow', x = df$eventName),]
thin$eventName <- as.character(thin$eventName)
thin$thinYear <- substr(thin$eventName, nchar(thin$eventName)-3, nchar(thin$eventName))
thin <- thin[, c('simulation', 'thinYear')]
thin <- thin[!duplicated(thin),]

# 2 - note thinning dates in df
df$thinYear <- 999
df[df$simulation == 'solling-beech' & df$year %in% thin[thin$simulation == 'solling-beech', 'thinYear'], 'thinYear'] <- 'Thin'
df[df$simulation == 'solling-spruce-above' & df$year %in% thin[thin$simulation == 'solling-spruce-above', 'thinYear'], 'thinYear'] <- 'Thin'
df[df$simulation == 'solling-spruce-below' & df$year %in% thin[thin$simulation == 'solling-spruce-below', 'thinYear'], 'thinYear'] <- 'Thin'
df[df$simulation == 'kroof' & df$year %in% thin[thin$simulation == 'kroof', 'thinYear'], 'thinYear'] <- 'Thin'

# 3 - remove pre-thinning values at thinning years
dfNoThin <- df[df$thinYear != 'Thin', ]
dfThin <- df[df$thinYear == 'Thin', ]
dfThin <- dfThin[dfThin$eventName != 'Evolution', ]
df <- rbind(dfNoThin, dfThin)

# keep only columns necessary for evaluation
df <- df[, c('year', 'code', 'dbh..cm.', 'height..m.', 'volume..m3.', 'x..m.', 'y..m.', 'simulation')]
colnames(df) <- c('year', 'species', 'D_cm', 'H_m', 'V_m3', 'X_utm', 'Y_utm', 'site')

# separate sites and remove site name
beech <- df[df$site == 'solling-beech', ]
beech$site <- NULL
spruceAbove <- df[df$site == 'solling-spruce-above', ]
spruceAbove$site <- NULL
spruceBelow <- df[df$site == 'solling-spruce-below', ]
spruceBelow$site <- NULL
kroof <- df[df$site == 'kroof', ]
kroof$site <- NULL

# save
write.csv(beech, paste0('samsara_solling-beech_', min(beech$year), '_', max(beech$year), '.csv'), row.names = FALSE)
write.csv(spruceAbove, paste0('samsaraAbove_solling-spruce_', min(spruceAbove$year), '_', max(spruceAbove$year), '.csv'), row.names = FALSE)
write.csv(spruceBelow, paste0('samsara_solling-spruce_', min(spruceBelow$year), '_', max(spruceBelow$year), '.csv'), row.names = FALSE)
write.csv(kroof, paste0('samsara_kroof_', min(kroof$year), '_', max(kroof$year), '.csv'), row.names = FALSE)
