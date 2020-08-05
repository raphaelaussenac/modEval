# load packages
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(plyr)
library(viridis)
library(stringr)
source('R/msd.R')

WritePlotBauges <- function(evalSite){

  alldf <- read.csv(paste0('./data/eval_', evalSite, '.csv'))
  alldf <- alldf[order(alldf$src, alldf$site, alldf$species, alldf$year),]

  # verify whether mortality is switched off
  # diffTree <- ddply(alldf[alldf$species == "allsp" & alldf$src == 'data',], .(site), summarise, diffNbTree = min(N) - max(N))
  # table(diffTree$diffNbTree)

  # keep only index associated to last growth year
  lastYear <- ddply(alldf, .(site), summarise, lastYear = max(year))
  alldf <- merge(alldf, lastYear, by = 'site')
  alldf <- alldf[alldf$year == alldf$lastYear,]
  alldf[, c('lastYear', 'year')] <- NULL

  # long format
  df <- melt(alldf, id.vars = c('site', 'src', 'species'))
  df <- dcast(df, site + species + variable ~ src)

  # round
  df$data <- round(df$data, 3)
  df$landclim <- round(df$landclim, 3)
  df$salem <- round(df$salem, 3)

  # order factor
  df$variable <- factor(df$variable, levels = c('N', 'Dg', 'BA', 'H', 'BAI_yr', 'Sh', 'GS', 'Simp', 'GI', 'V'))

  ##############################################################################
  # evaluation metrics
  ##############################################################################

  msd <- data.frame()
  for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){

    # calculate absolute and relative difference between observations and predictions
    df[, paste0(mod, '_absDiff')] <- df[, mod] - df$data
    df[, paste0(mod, '_relDiff')] <- (df[, mod] * 100 / df$data) - 100

    # Calculate MSD and its 3 components
    # create evaluation data frame
    evaldf <- df
    colnames(evaldf)[colnames(evaldf) == 'data'] <- 'Y'
    colnames(evaldf)[colnames(evaldf) == mod] <- 'X'

    # X and Y must have the same length (no missing value in X or Y)
    evaldf <- evaldf[!is.na(evaldf$X) & !is.na(evaldf$Y),]

    # Calculation
    evaldf <- evaldf %>% group_by(species, variable) %>% mutate(x = X - mean(X, na.rm = TRUE),
                                                   y = Y - mean(Y, na.rm = TRUE),
                                                   xy = x * y)
    MSD <- evaldf %>% group_by(species, variable) %>%  dplyr::summarise(MSD = sum( (X - Y) ^2, na.rm = TRUE) / length(X))
    SB <- evaldf %>% group_by(species, variable) %>%  dplyr::summarise(SB = ( mean(X, na.rm = TRUE) - mean(Y, na.rm = TRUE) )^2 )
    br2 <- evaldf %>% group_by(species, variable) %>% dplyr::summarise(b = sum(xy, na.rm = TRUE) / sum(x^2, na.rm = TRUE),
                                                                    r2 = ( sum(xy, na.rm = TRUE)^2 ) / ( sum(x^2, na.rm = TRUE)*sum(y^2, na.rm = TRUE) ))
    br2$NU1 <- (1 - br2$b)^2
    br2$LC1 <- 1 - br2$r2
    NU2 <- evaldf %>% group_by(species, variable) %>% dplyr::summarise(NU2 = sum(x^2, na.rm = TRUE) / length(x))
    LC2 <- evaldf %>% group_by(species, variable) %>% dplyr::summarise(LC2 = sum(y^2, na.rm = TRUE) / length(y))
    NU <- merge(br2, NU2, by = c('species', 'variable'))
    NU$NU <- NU$NU1 * NU$NU2
    LC <- merge(br2, LC2, by = c('species', 'variable'))
    LC$LC <- LC$LC1 * LC$LC2

    MSD3 <- merge(MSD, SB, by = c('species', 'variable'))
    MSD3 <- merge(MSD3, NU[, c('species', 'variable', 'NU')])
    MSD3 <- merge(MSD3, LC[, c('species', 'variable', 'LC')])
    MSD3$mod <- mod
    msd <- rbind(msd,MSD3)

  }


  # ################################################################################
  # plots
  ################################################################################
  # create directory to save plots
  if (!(dir.exists(paste0('plotEval/', evalSite)))){dir.create(paste0('plotEval/', evalSite), recursive = TRUE)}

  # plot mean square deviation
  msd <- melt(msd, id.vars = c('species', 'variable', 'mod'))
  colnames(msd)[ncol(msd)-1] <- 'devMeasure'
  # order factor
  # msd$variable <- factor(msd$variable, levels = c('N', 'Dg', 'BA', 'H', 'BAI_yr', 'Sh', 'GS', 'Simp', 'GI'))
  msd$devMeasure <- factor(msd$devMeasure, levels = c('MSD', 'LC', 'NU', 'SB'))

  ggplot(data = msd[msd$devMeasure != 'MSD' & msd$species == 'allsp', ], aes(x = mod, y = value, fill = devMeasure)) +
    geom_bar(stat = "identity") +
    facet_wrap(. ~ variable, scale = "free") +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.spacing = unit(20, 'pt'))

  # plot absolute and relative difference between observations and predictions
  diff <- melt(df, id.vars = c('site', 'species', 'variable'))
  colnames(diff)[ncol(diff)-1] <- 'temp'
  # diff$src <- strsplit(as.character(diff$temp), '_')[[1]][1]
  diff$mod <- str_split_fixed(diff$temp, "_", 2)[,1]
  diff$diff <- str_split_fixed(diff$temp, "_", 2)[,2]
  diff[diff$diff == '', 'diff'] <- 'obsPred'
  diff$temp <- NULL

  # abs difference
  ggplot(data = diff[diff$sp == 'allsp' & diff$diff == 'absDiff',], aes(x =  mod, y = value)) +
    # geom_bar(stat = "identity") +
    geom_boxplot() +
    facet_wrap(. ~ variable, scale = "free") +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.spacing = unit(20, 'pt'))

  # abs difference
  ggplot(data = diff[diff$sp == 'allsp' & diff$diff == 'relDiff',], aes(x =  mod, y = value)) +
    # geom_bar(stat = "identity") +
    geom_boxplot() +
    facet_wrap(. ~ variable, scale = "free") +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.spacing = unit(20, 'pt'))

  # regression
  reg <- dcast(diff, site + species + variable + mod ~ diff)
  reg <- reg[reg$mod != 'data',]
  colnames(reg)[colnames(reg) == 'obsPred'] <- 'pred'
  reg[, c('relDiff')] <- NULL
  reg <- merge(reg, diff[diff$mod == 'data',c('site', 'species', 'variable', 'value')], by = c('site', 'species', 'variable'))
  colnames(reg)[length(colnames(reg))] <- 'obs'

  ggplot(data = reg[reg$species == 'allsp',]) +
  geom_point(aes(x = obs, y = absDiff, col = mod), alpha = 0.5) +
  facet_wrap(. ~ variable, scale = "free") +
  ylab('predicted - observed') +
  xlab('observed') +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.spacing = unit(20, 'pt'))


  return(1)

}
