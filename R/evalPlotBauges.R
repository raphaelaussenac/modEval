# load packages
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(plyr)
library(viridis)
library(stringr)
library(broom)
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
    # calculation
    msd3 <- calc_msd(evaldf, mod, groups = c('species', 'variable'))
    msd <- rbind(msd, msd3)

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
  msd$devMeasure <- factor(msd$devMeasure, levels = c('MSD', 'LC', 'NU', 'SB'))

  pl1 <- ggplot(data = msd[msd$devMeasure != 'MSD' & msd$species == 'allsp', ], aes(x = mod, y = value, fill = devMeasure)) +
    geom_bar(stat = "identity") +
    facet_wrap(. ~ variable, scale = "free") +
    theme_light() +
    xlab('models') +
    ylab('mean square deviation') +
    theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.spacing = unit(20, 'pt'))
  ggsave(file = paste0('./plotEval/', evalSite, '/msd.pdf'), plot = pl1, width = 10, height = 10)

  # plot absolute and relative difference between observations and predictions
  diff <- melt(df, id.vars = c('site', 'species', 'variable'))
  colnames(diff)[ncol(diff)-1] <- 'temp'
  # diff$src <- strsplit(as.character(diff$temp), '_')[[1]][1]
  diff$mod <- str_split_fixed(diff$temp, "_", 2)[,1]
  diff$diff <- str_split_fixed(diff$temp, "_", 2)[,2]
  diff[diff$diff == '', 'diff'] <- 'obsPred'
  diff$temp <- NULL

  # abs difference
  pl2 <- ggplot(data = diff[diff$sp == 'allsp' & diff$diff == 'absDiff',], aes(x =  mod, y = value)) +
    # geom_bar(stat = "identity") +
    geom_boxplot() +
    facet_wrap(. ~ variable, scale = "free") +
    theme_light() +
    xlab('models') +
    ylab('predictions - observations') +
    theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.spacing = unit(20, 'pt'))
  ggsave(file = paste0('./plotEval/', evalSite, '/absDiff.pdf'), plot = pl2, width = 10, height = 10)

  # abs difference
  pl3 <- ggplot(data = diff[diff$sp == 'allsp' & diff$diff == 'relDiff',], aes(x =  mod, y = value)) +
    # geom_bar(stat = "identity") +
    geom_boxplot() +
    facet_wrap(. ~ variable, scale = "free") +
    theme_light() +
    xlab('models') +
    ylab('(predictions * 100 / observations) - 100') +
    theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.spacing = unit(20, 'pt'))
    ggsave(file = paste0('./plotEval/', evalSite, '/relDiff.pdf'), plot = pl3, width = 10, height = 10)

  # regression
  reg <- dcast(diff, site + species + variable + mod ~ diff)
  reg <- reg[reg$mod != 'data',]
  colnames(reg)[colnames(reg) == 'obsPred'] <- 'pred'
  reg[, c('relDiff')] <- NULL
  reg <- merge(reg, diff[diff$mod == 'data',c('site', 'species', 'variable', 'value')], by = c('site', 'species', 'variable'))
  colnames(reg)[length(colnames(reg))] <- 'obs'

  pl4 <- ggplot(data = reg[reg$species == 'allsp',], aes(x = obs, y = absDiff, col = mod)) +
  geom_point(alpha = 0.5) +
  facet_wrap(. ~ variable, scale = "free") +
  geom_smooth(method = 'lm', formula = y ~ x) +
  ylab('predictions - observations') +
  xlab('observations') +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.spacing = unit(20, 'pt'))
  ggsave(file = paste0('./plotEval/', evalSite, '/regDiff.pdf'), plot = pl4, width = 10, height = 10)

  # plot BAI_yr absolute differences (pred - obs) against all other observed variables
  # TODO: add environmental varibales
  BAI <- reg[reg$variable == 'BAI_yr', c('site', 'species', 'mod', 'absDiff'),]
  colnames(BAI)[ncol(BAI)] <- 'absDiffBAI_yr'
  temp <- reg[, c('site', 'species', 'variable', 'obs'),]
  temp <- temp[!duplicated(temp),]
  BAIdiff <- merge(BAI, temp, by  = c('site', 'species'))

  # lm for BAI_absolute_diffrence ~ all variables
  modeldf <- BAIdiff[BAIdiff$species == 'allsp' & !is.na(BAIdiff$obs),] %>% group_by(variable, mod)
  models <- do(modeldf,
      glance( # replace glance by tidy to get models parameter estimates
        lm(absDiffBAI_yr ~ obs, data = .)))
  models <- data.frame(models)
  # add to BAIdiff
  BAIdiff <- merge(BAIdiff, models[, c('variable', 'mod', 'r.squared')], by = c('variable', 'mod'), all.x = TRUE)

  pl5 <- ggplot(data = BAIdiff[BAIdiff$species == 'allsp',], aes(x = obs, y = absDiffBAI_yr, col = mod)) +
  geom_point(alpha = 0.5) +
  geom_text(data = models[models$mod == 'landclim',], aes(x = -Inf, y = Inf, label = paste('R²=',round(r.squared, 3))), hjust = 0, vjust = 1) +
  geom_text(data = models[models$mod == 'salem',], aes(x = Inf, y = Inf, label = paste('R²=',round(r.squared, 3))), hjust = 1, vjust = 1) +
  facet_wrap(. ~ variable, scale = "free", strip.position = "bottom") +
  geom_smooth(method = 'lm', formula = y ~ x) +
  ylab('BAI_yr predictions - BAI_yr observations') +
  xlab('observations') +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.spacing = unit(20, 'pt'))
  ggsave(file = paste0('./plotEval/', evalSite, '/BAIdiff.pdf'), plot = pl5, width = 10, height = 10)

  return(1)

}
