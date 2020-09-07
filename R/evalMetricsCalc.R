# load packages
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(viridis)
library(stringr)
library(broom)

evalMetricsCalc <- function(evalSite){

  ##############################################################################
  # format data
  ##############################################################################

  alldf <- read.csv(paste0('./data/eval_', evalSite, '.csv'))
  alldf <- alldf[order(alldf$src, alldf$site, alldf$species, alldf$year),]

  # keep only index associated to last growth year at bauges sites
  if(evalSite == 'bauges'){

    alldf <- alldf %>% group_by(site) %>% filter(year >= max(year)) %>% ungroup() %>% select(-year)

    # long format
    df <- reshape2::melt(alldf, id.vars = c('site', 'src', 'species'))
    df <- reshape2::dcast(df, site + species + variable ~ src)

  } else if (evalSite == 'profound'){
    # long format
    df <- reshape2::melt(alldf, id.vars = c('site', 'src', 'species', 'year'))
    df <- reshape2::dcast(df, site + species + year + variable ~ src)

  }

  # sort factor
  df$variable <- factor(df$variable, levels = c('N', 'Dg', 'BA', 'H', 'V', 'BAI_yr', 'Sh', 'GS', 'Simp', 'GI', 'SkewD', 'Shsp', 'GSsp', 'Simpsp'))

  ##############################################################################
  # calculate evaluation metrics
  ##############################################################################

  msd <- data.frame()
  for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){

    # calculate absolute and relative difference between
    # observations and predictions
    df[, paste0(mod, '_absDiff')] <- df[, mod] - df$data
    df[, paste0(mod, '_relDiff')] <- ( (df[, mod] - df$data) * 100 ) / df$data
    df[df$data < 0 & !is.na(df$data), paste0(mod, '_relDiff')] <- df[df$data < 0 & !is.na(df$data), paste0(mod, '_relDiff')] * -1

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

  # plot absolute and relative difference between
  # observations and predictions
  diff <- diffPlot(evalSite, df, 'absDiff')
  temp <- diffPlot(evalSite, df, 'relDiff')

  # plot msd and times seris of variable for all species at specific 'profound' site
  if (evalSite == 'profound'){
    df[, c(str_which(colnames(df), "Diff"))] <- NULL
    tsPlot(evalSite, df)
    tsSpPlot(evalSite, df, site = 'kroof')
    msdSpPlot(evalSite, msd, groups, site = 'kroof')
  }

  # plot variables errors (pred-obs) = f(environmental and stand features)
  # at bauges site
  if (evalSite == 'bauges'){
    models <- regDiffPlot(evalSite, diff, 'absDiff')
    regDiffPlot(evalSite, diff, 'relDiff')
    return(models)
  }


}
