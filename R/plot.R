# plot mean square deviation for each variable at each site
msdPlot <- function(evalSite, msd, groups){

  msdDf <- msd[!is.na(msd$SB) & !is.na(msd$NU) & !is.na(msd$LC),]
  msdDf <- reshape2::melt(msdDf, id.vars = c(groups, 'mod'))
  colnames(msdDf)[ncol(msdDf)-1] <- 'devMeasure'
  # order factor
  msdDf$devMeasure <- factor(msdDf$devMeasure, levels = c('MSD', 'LC', 'NU', 'SB'))

  pl1 <- ggplot(data = msdDf[msdDf$devMeasure != 'MSD' & msdDf$species == 'allsp', ], aes(x = mod, y = value, fill = devMeasure)) +
    geom_bar(stat = "identity") +
    # facet_wrap(. ~ variable, scale = "free") +
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
  if(evalSite == 'bauges'){
    pl1 <- pl1 + facet_wrap(. ~ variable, scale = "free")
    ggsave(file = paste0('./plotEval/', evalSite, '/msd.pdf'), plot = pl1, width = 10, height = 10)
  } else if (evalSite == 'profound'){
    pl1 <- pl1 + facet_grid(variable ~ site, scale = 'free')
    ggsave(file = paste0('./plotEval/', evalSite, '/msd.pdf'), plot = pl1, width = 8, height = 12)
  }

}


# plot mean square deviation for each variable at each site
msdSpPlot <- function(evalSite, msd, groups, site){

  msdDf <- msd[!is.na(msd$SB) & !is.na(msd$NU) & !is.na(msd$LC) & msd$site == site & msd$species != 'allsp',]
  msdDf <- reshape2::melt(msdDf, id.vars = c(groups, 'mod'))
  colnames(msdDf)[ncol(msdDf)-1] <- 'devMeasure'
  spLevelVariable <- c('N', 'Dg', 'BA', 'BAI_yr')
  msdDf <- msdDf[msdDf$variable %in% spLevelVariable, ]
  # order factor
  msdDf$devMeasure <- factor(msdDf$devMeasure, levels = c('MSD', 'LC', 'NU', 'SB'))

  pl1 <- ggplot(data = msdDf[msdDf$devMeasure != 'MSD', ], aes(x = mod, y = value, fill = devMeasure)) +
    geom_bar(stat = "identity") +
    facet_grid(variable ~ species, scale = "free") +
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
  ggsave(file = paste0('./plotEval/', evalSite, '/msdSp', site, '.pdf'), plot = pl1, width = 8, height = 12)

}


# plot time series for each variable at each site
tsPlot <- function(evalSite, df){

  ts <- df[df$species == 'allsp', ]
  ts <- reshape2::melt(ts, id.vars = c('site', 'species', 'year', 'variable'))
  colnames(ts)[ncol(ts)-1] <- 'src'
  ts <- ts[ts$variable != 'V', ]
  # sort factor
  ts$src <- factor(ts$src, levels = c('data', '4c', 'landclim', 'salem'))

  pl1 <- ggplot() +
    geom_path(data = ts[!is.na(ts$value),], aes(x = year, y = value, col = src), linetype = 'dashed') +
    geom_line(data = ts[ts$src == 'data' & !is.na(ts$value),], aes(x = year, y = value, col = src)) +
    facet_grid(variable ~ site, scale = "free") +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
          # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
  ggsave(file = paste0('./plotEval/', evalSite, '/ts.pdf'), width = 8, height = 12)

}


# plot time series for ecah species at specific site
tsSpPlot <- function(evalSite, df, site){

  ts <- df[df$species != 'allsp' & df$site == site, ]
  ts <- reshape2::melt(ts, id.vars = c('site', 'species', 'year', 'variable'))
  colnames(ts)[ncol(ts)-1] <- 'src'
  spLevelVariable <- c('N', 'Dg', 'BA', 'BAI_yr')
  ts <- ts[ts$variable %in% spLevelVariable, ]
  # sort factor
  ts$src <- factor(ts$src, levels = c('data', '4c', 'landclim', 'salem'))

  pl1 <- ggplot() +
    geom_path(data = ts[!is.na(ts$value),], aes(x = year, y = value, col = src), linetype = 'dashed') +
    geom_line(data = ts[ts$src == 'data' & !is.na(ts$value),], aes(x = year, y = value, col = src)) +
    facet_grid(variable ~ species, scale = "free") +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
          # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
  ggsave(file = paste0('./plotEval/', evalSite, '/tsSp', site, '.pdf'), width = 8, height = 12)

}


# plot absolute and relative difference between observations and predictions
diffPlot <- function(evalSite, df, relabsdiff){

  diff <- reshape2::melt(df, id.vars = c('site', 'species', 'variable'))
  colnames(diff)[ncol(diff)-1] <- 'temp'
  # diff$src <- strsplit(as.character(diff$temp), '_')[[1]][1]
  diff$mod <- str_split_fixed(diff$temp, "_", 2)[,1]
  diff$diff <- str_split_fixed(diff$temp, "_", 2)[,2]
  diff[diff$diff == '', 'diff'] <- 'obsPred'
  diff$temp <- NULL

  pl1 <- ggplot(data = diff[diff$sp == 'allsp' & diff$diff == relabsdiff,], aes(x =  mod, y = value)) +
    # geom_bar(stat = "identity") +
    geom_boxplot() +
    facet_wrap(. ~ variable, scale = "free") +
    theme_light() +
    xlab('models') +
    # ylab('predictions - observations') +
    theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.spacing = unit(20, 'pt'))
  if(relabsdiff == 'absDiff'){
    pl1 <- pl1 + ylab('predictions - observations')
  } else if(relabsdiff == 'relDiff'){
    pl1 <- pl1 + ylab('(predictions * 100 / observations) - 100')
  }
  ggsave(file = paste0('./plotEval/', evalSite, '/', relabsdiff, '.pdf'), plot = pl1, width = 10, height = 10)

  return(diff)

}

# plot regression diff = f(environmental and stand features)
# and return models specifications
regDiffPlot <- function(evalSite, diff){

  # requires 'diff' dataframe from 'diffPlot' function
  reg <- reshape2::dcast(diff, site + species + variable + mod ~ diff)
  reg <- reg[reg$mod != 'data',]
  colnames(reg)[colnames(reg) == 'obsPred'] <- 'pred'
  reg[, c('relDiff')] <- NULL
  reg <- merge(reg, diff[diff$mod == 'data',c('site', 'species', 'variable', 'value')], by = c('site', 'species', 'variable'))
  colnames(reg)[length(colnames(reg))] <- 'obs'

  pl1 <- ggplot(data = reg[reg$species == 'allsp',], aes(x = obs, y = absDiff, col = mod)) +
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
  ggsave(file = paste0('./plotEval/', evalSite, '/regDiff.pdf'), plot = pl1, width = 10, height = 10)

  # plot BAI_yr absolute differences (pred - obs) against all other observed variables
  # TODO: add environmental varibales
  BAI <- reg[reg$variable == 'BAI_yr', c('site', 'species', 'mod', 'absDiff'),]
  colnames(BAI)[ncol(BAI)] <- 'absDiffBAI_yr'
  temp <- reg[, c('site', 'species', 'variable', 'obs'),]
  temp <- temp[!duplicated(temp),]
  BAIdiff <- merge(BAI, temp, by  = c('site', 'species'))

  # lm for BAI_absolute_diffrence ~ all variables
  modeldf <- BAIdiff[BAIdiff$species == 'allsp' & !is.na(BAIdiff$obs) & !is.na(BAIdiff$absDiffBAI_yr),] %>% group_by(variable, mod)
  models <- do(modeldf,
      glance( # replace glance by tidy to get models parameter estimates
        lm(absDiffBAI_yr ~ obs, data = .)))
  models <- data.frame(models)
  # add models output to BAIdiff
  BAIdiff <- merge(BAIdiff, models[, c('variable', 'mod', 'r.squared')], by = c('variable', 'mod'), all.x = TRUE)

  pl2 <- ggplot(data = BAIdiff[BAIdiff$species == 'allsp',], aes(x = obs, y = absDiffBAI_yr, col = mod)) +
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
  ggsave(file = paste0('./plotEval/', evalSite, '/BAIdiff.pdf'), plot = pl2, width = 10, height = 10)

  return(models)

}

msdRadarPlot <- function(evalSite, msd){

  msdDf <- msd[!is.na(msd$SB) & !is.na(msd$NU) & !is.na(msd$LC) & msd$species == 'allsp', ]
  # calculate relative MSD for each variable at each site
  if(evalSite == 'profound'){
    group1 <- c('site', 'variable')
  } else if (evalSite == 'bauges'){
    group1 <- c('variable')
  }

  msdDf <- msdDf %>% group_by_at(group1) %>% mutate(maxMSD = max(MSD),
                                                         relMSD = MSD/max(MSD)) %>% ungroup()
  # long to wide
  if (evalSite == 'bauges'){
    msdDf$site <- 'bauges'
  }
  relativeMSD <- data.frame(spread(msdDf[, c('site', 'mod', 'variable', 'relMSD')], variable, relMSD))

  if(evalSite == 'profound'){
    pdf(file = paste0('./plotEval/', evalSite, '/radar.pdf'), width = 13, height = 5)
    par(mfrow = c(1,3))
  } else if (evalSite == 'bauges'){
    pdf(file = paste0('./plotEval/', evalSite, '/radar.pdf'), width = 10, height = 10)
  }
  # color vector
  # colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  # colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  colors_border = viridis_pal(alpha = 1, option = 'viridis')(100)[c(1, 50, 100)]
  colors_in = viridis_pal(alpha = 0.5, option = 'viridis')(100)[c(1, 50, 100)]

  for (i in unique(relativeMSD$site)){
    df <- relativeMSD[relativeMSD$site == i, ]
    rownames(df) <- df$mod
    df[, c('site', 'mod')] <- NULL
    df <- rbind(rep(1,ncol(df)) , rep(0,ncol(df)) , df)
    df <- round(df, 2)
    # remove columns with NA (ex: H at kroof site)
    df <- df[colSums(!is.na(df)) == nrow(df)]

    # plot with default options:
    radarchart(df , axistype=1,
        # custom polygon
        pcol = colors_border, pfcol = colors_in, plwd = 1, plty = 1,
        # title
        title = i,
        # custom labels
        vlcex = 1.6,
        # custom the grid
        cglcol = "grey", cglty = 1, axislabcol = "black", cglwd = 1, seg = 5, caxislabels = seq(0,1,0.2), calcex = 1.5,
        # center
        centerzero = FALSE
        )
  }

# Add a legend
  legend(x=0.55, y=-1, legend = rownames(df[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)

# dev.copy2pdf(file = paste0('./plotEval/radar.pdf'), width = 13, height = 5)
  dev.off()


}
