# library
library(plyr)
library(data.table)

standVarCalc <- function(evalSite){
# load data
  alldf <- read.csv(paste0('./data/all_', evalSite, '.csv'))

################################################################################
# calculate yearly aggregated index (N, Dg, etc) for observed and predicted data
# at sp level
################################################################################

# sp N, Dg, H, BA
 sp <- ddply(alldf, .(year, src, site, species), summarise, N = sum(weight),
                                                 Dg = sqrt(sum(D_cm^2 * weight)/sum(weight)),
                                                 H = sum(H_m * weight) /sum(weight),
                                                 BA = sum((pi * D_cm^2 / 4) * weight) / 10000,
                                                 V = sum(V_m3 * weight))

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
  for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){
    for (site in unique(alldf$site)){
      for (s in unique(alldf[alldf$site == site & alldf$src == mod, 'species'])){
        # retrieve start year of each growth period
        dfP <- sp[sp$src == 'data' & sp$site == site & sp$species == s,]
        # when sp is not prensent in data (because of sp substitution)
        # use another species present in the data (only serves to retrieve
        # the starting year of each growth period)
        if (nrow(dfP) == 0 & evalSite == 'bauges'){
          otherSp <- sp[sp$src == 'data' & sp$site == site, 'species'][1]
          dfP <- sp[sp$src == 'data' & sp$site == site & sp$species == otherSp,]
        }
        df <- sp[sp$src == mod & sp$site == site & sp$species == s,]
        df <- merge(df, dfP[, c('year', 'yearlag')], by = 'year', all.x = TRUE)
        df$BAI_yr <- NA
        for (end in df[!is.na(df$yearlag.y), 'year']){
          # if start date exists in df
          start <- df[df$year == end, 'yearlag.y']
          if (start %in% df[!is.na(df$BA), 'year']){
            if (evalSite == 'profound'){
              df[df$year == end, 'BAI_yr'] <- (df[df$year == end, 'BA'] - df[df$year == start, 'BA']) / (end - start)
            } else if (evalSite == 'bauges'){ # growth period + 1 for the bauges data set
              df[df$year == end, 'BAI_yr'] <- (df[df$year == end, 'BA'] - df[df$year == start, 'BA']) / ((end - start) + 1)
            }
          }
        }
        modelDf <- rbind(modelDf, df)
      }
    }
  }
  modelDf <- modelDf[, c('year', 'src', 'site', 'species', 'N', 'Dg', 'H', 'BA', 'V', 'BAI_yr')]

# calculate mean BAI/yr on observation data set
  obsDf <- sp[sp$src == 'data',]
  if (evalSite == 'profound'){
    obsDf$BAI_yr <- (obsDf$BA - obsDf$BAlag) / (obsDf$year - obsDf$yearlag)
  } else if (evalSite == 'bauges'){ # growth period + 1 for the bauges data set
    obsDf$BAI_yr <- (obsDf$BA - obsDf$BAlag) / ((obsDf$year - obsDf$yearlag) + 1)
  }
  obsDf <- obsDf[, c('year', 'src', 'site', 'species', 'N', 'Dg', 'H', 'BA', 'V', 'BAI_yr')]

# merge profound + models
  sp <- rbind(modelDf, obsDf)

################################################################################
# calculate yearly aggregated index (N, Dg, etc) for observed and predicted data
# at stand level
################################################################################

# stand N, Dg, H, BA
  stand <- ddply(alldf, .(year, src, site), summarise, N = sum(weight),
                                                 Dg = sqrt(sum(D_cm^2 * weight)/sum(weight)),
                                                 H = sum(H_m * weight) /sum(weight),
                                                 BA = sum((pi * D_cm^2 / 4) * weight) / 10000,
                                                 V = sum(V_m3 * weight))
#
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

  # diameter class diversity
  out <- ReturnDivIndex(evalSite, "D_cm", 10)
  out <- data.frame(out)
  out[, c('N', 'Inter', 'Nvar')] <- NULL

  # species diversity
  divOut <- ReturnDivIndex(evalSite, "species")
  divOut <- data.frame(divOut)
  divOut[, c('N', 'GI')] <- NULL
  colnames(divOut)[!(colnames(divOut) %in% c('year', 'site', 'src'))] <- paste0(colnames(divOut)[!(colnames(divOut) %in% c('year', 'site', 'src'))], 'sp')

# add heterogeneity index to df
  sp <- merge(sp, out, by= c('year', 'src', 'site'))
  sp <- merge(sp, divOut, by = c('year', 'src', 'site'))

# write site index
  write.csv(sp, paste0('./data/eval_', evalSite, '.csv'), row.names = FALSE)
  return(file.exists(paste0('./data/eval_', evalSite, '.csv')))
}
