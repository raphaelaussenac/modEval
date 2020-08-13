# load packages
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(plyr)
library(viridis)
library(stringr)
library(broom)

evalMetricsCalc <- function(evalSite){

  ##############################################################################
  # format data
  ##############################################################################

  alldf <- read.csv(paste0('./data/eval_', evalSite, '.csv'))
  alldf <- alldf[order(alldf$src, alldf$site, alldf$species, alldf$year),]

  # verify whether mortality is switched off
  # diffTree <- ddply(alldf[alldf$species == "allsp" & alldf$src == 'data',], .(site), summarise, diffNbTree = min(N) - max(N))
  # table(diffTree$diffNbTree)

  # keep only index associated to last growth year at bauges sites
  if(evalSite == 'bauges'){
    lastYear <- ddply(alldf, .(site), summarise, lastYear = max(year))
    alldf <- merge(alldf, lastYear, by = 'site')
    alldf <- alldf[alldf$year == alldf$lastYear,]
    alldf[, c('lastYear', 'year')] <- NULL

    # long format
    df <- reshape2::melt(alldf, id.vars = c('site', 'src', 'species'))
    df <- reshape2::dcast(df, site + species + variable ~ src)

  } else if (evalSite == 'profound'){
    # long format
    df <- reshape2::melt(alldf, id.vars = c('site', 'src', 'species', 'year'))
    df <- reshape2::dcast(df, site + species + year + variable ~ src)

  }

  # sort factor
  df$variable <- factor(df$variable, levels = c('N', 'Dg', 'BA', 'H', 'BAI_yr', 'Sh', 'GS', 'Simp', 'GI', 'V', 'Shsp', 'GSsp', 'Simpsp'))

  ##############################################################################
  # calculate evaluation metrics
  ##############################################################################

  msd <- data.frame()
  for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){

    # at bauges site calculate absolute and relative difference between
    # observations and predictions
    if(evalSite == 'bauges'){
      df[, paste0(mod, '_absDiff')] <- df[, mod] - df$data
      df[, paste0(mod, '_relDiff')] <- (df[, mod] * 100 / df$data) - 100
    }

    # Calculate MSD and its 3 components
    # create evaluation data frame
    evaldf <- df
    colnames(evaldf)[colnames(evaldf) == 'data'] <- 'Y'
    colnames(evaldf)[colnames(evaldf) == mod] <- 'X'
    # calculation
    # set grouping factor
    if(evalSite == 'bauges'){
      groups <- c('species', 'variable')
    } else if (evalSite == 'profound'){
      groups <- c('site', 'species', 'variable')
    }
    msd3 <- calc_msd(evaldf, mod, groups)
    msd <- rbind(msd, msd3)

  }

  ##############################################################################
  # plot
  ##############################################################################

  # create directory to save plots
  if (!(dir.exists(paste0('plotEval/', evalSite)))){dir.create(paste0('plotEval/', evalSite), recursive = TRUE)}

  # plot MSD and its 3 components
  msdPlot(evalSite, msd, groups)

  # plot observations vs predictions
  obsPred(evalSite, df, alldf)

  # plot msd radarchart
  msdRadarPlot(evalSite, msd)

  # plot msd and times seris of variable for all species at specific 'profound' site
  if (evalSite == 'profound'){
    tsPlot(evalSite, df)
    tsSpPlot(evalSite, df, site = 'kroof')
    msdSpPlot(evalSite, msd, groups, site = 'kroof')
  }

  # plot variables errors (pred-obs) = f(environmental and stand features)
  # at bauges site
  if (evalSite == 'bauges'){
    diff <- diffPlot(evalSite, df, 'absDiff')
    diffPlot(evalSite, df, 'relDiff')
    models <- regDiffPlot(evalSite, diff, 'absDiff')
    regDiffPlot(evalSite, diff, 'relDiff')
    return(models)
  }


}
