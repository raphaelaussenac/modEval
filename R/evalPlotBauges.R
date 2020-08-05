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



# TODO: change species codes and names of models for latinNames
  # head(df[df$site == 4740,])
  # head(alldf[alldf$site == 4740,])

  ################################################################################
  # evaluation
  ################################################################################

  # list of evaluation variables
  # alldf$V <- NULL
  # evalVar <- c('N', 'Dg', 'H', 'BA','BAI_yr', 'Sh', 'GS', 'Simp', 'GI')

  # # create evaluation data set
  # evaluation <- data.frame()
  # # for each model
  # for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){
  # # at each site
  #   for (site in unique(alldf$site)){
  #     # for each sp
  #     for (sp in unique(alldf$species)){
  #     # create df with index values
  #       obs <- alldf[alldf$src == 'data' & alldf$site == site & alldf$species == sp, ]
  #       colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site', 'species'))] <- paste('obs', colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site', 'species'))], sep = '')
  #       obs[, c('src', 'site', 'species')] <- NULL
  #       pred <- alldf[alldf$src == mod & alldf$site == site & alldf$species == sp, ]
  #       colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site', 'species'))] <- paste('pred', colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site', 'species'))], sep = '')
  #       # merge obs and pred
  #       df <- merge(obs, pred, by = 'year')
  #       evaluation <- rbind(evaluation, df)
  #     }
  #   }
  # }

  # calculate absolute and relative difference between observations and predictions
  for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){
    df[, paste0(mod, '_absDiff')] <- df[, mod] - df$data
    df[, paste0(mod, '_relDiff')] <- (df[, mod] * 100 / df$data) - 100
  }


  msd <- data.frame()
  for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){

    # create evaluation data frame
    evaldf <- df
    colnames(evaldf)[colnames(evaldf) == 'data'] <- 'Y'
    colnames(evaldf)[colnames(evaldf) == mod] <- 'X'

    # X and Y must have the same length (no missing value in X or Y)
    evaldf <- evaldf[!is.na(evaldf$X) & !is.na(evaldf$Y),]

    # Calculate MSD and its 3 components
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




  #
  #
  #
  #
  # # evaluation
  # deviancedf <- data.frame('variable'= NA, 'devMeasure'= NA, 'value' = NA, 'mod' = NA, 'sp' = NA)
  # # for each model
  # for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){
  #   # for each sp
  #   for (sp in unique(evaluation$species)){
  #     for (i in evalVar){
  #       # create evaluation df
  #       evaldf <- evaluation[evaluation$species == sp & evaluation$src == mod, c('year', colnames(evaluation)[colnames(evaluation) == paste('obs', i, sep = '') | colnames(evaluation) == paste('pred', i, sep = '')])]
  #       colnames(evaldf)[substr(colnames(evaldf), 1, 3) == 'obs'] <- 'Y'
  #       colnames(evaldf)[substr(colnames(evaldf), 1, 4) == 'pred'] <- 'X'
  #       # calculate deviance
  #       if (nrow(evaldf) > 0){
  #         # calculate MSD and its 3 components
  #         msd <- MSD(evaldf)
  #         # save
  #         deviancedf <- rbind(deviancedf, setNames(data.frame(rep(i, length(msd)),
  #                                                             c('MSD', 'SB', 'NU', 'LC'),
  #                                                             msd,
  #                                                             rep(as.character(mod), length(msd)),
  #                                                             rep(as.character(sp), length(msd))),
  #                                                  names(deviancedf)))
  #
  #         # calculate relative difference
  #         relDiff <- (evaldf$X * 100 / evaldf$Y) - 100
  #         # save
  #         deviancedf <- rbind(deviancedf, setNames(data.frame(rep(i, length(relDiff)),
  #                                                             rep('relDiff', length(relDiff)),
  #                                                             relDiff,
  #                                                             rep(as.character(mod), length(relDiff)),
  #                                                             rep(as.character(sp), length(relDiff))),
  #                                                  names(deviancedf)))
  #
  #         # calculate absolute difference
  #         absDiff <- evaldf$X - evaldf$Y
  #         # save
  #         deviancedf <- rbind(deviancedf, setNames(data.frame(rep(i, length(absDiff)),
  #                                                             rep('absDiff', length(absDiff)),
  #                                                             absDiff,
  #                                                             rep(as.character(mod), length(absDiff)),
  #                                                             rep(as.character(sp), length(absDiff))),
  #                                                  names(deviancedf)))
  #
  #         # save observed values
  #         Y <- evaldf$Y
  #         deviancedf <- rbind(deviancedf, setNames(data.frame(rep(i, length(Y)),
  #                                                             rep('Y', length(Y)),
  #                                                             Y,
  #                                                             rep(as.character(mod), length(Y)),
  #                                                             rep(as.character(sp), length(Y))),
  #                                                  names(deviancedf)))
  #
  #       } else {
  #         deviancedf <- rbind(deviancedf, c(i, 'MSD', NA, as.character(mod), as.character(sp)))
  #         deviancedf <- rbind(deviancedf, c(i, 'SB', NA, as.character(mod), as.character(sp)))
  #         deviancedf <- rbind(deviancedf, c(i, 'NU', NA, as.character(mod), as.character(sp)))
  #         deviancedf <- rbind(deviancedf, c(i, 'LC', NA, as.character(mod), as.character(sp)))
  #         deviancedf <- rbind(deviancedf, c(i, 'relDiff', NA, as.character(mod), as.character(sp)))
  #         deviancedf <- rbind(deviancedf, c(i, 'absDiff', NA, as.character(mod), as.character(sp)))
  #         deviancedf <- rbind(deviancedf, c(i, 'Y', NA, as.character(mod), as.character(sp)))
  #       }
  #     }
  #   }
  # }
  # deviancedf <- deviancedf[-1,]
  # deviancedf$value <- as.numeric(deviancedf$value)
  # deviancedf$variable <- as.factor(deviancedf$variable)
  # deviancedf$devMeasure <- as.factor(deviancedf$devMeasure)
  # deviancedf$mod <- as.factor(deviancedf$mod)
  # deviancedf$sp <- as.factor(deviancedf$sp)
  #
  # order factor to get stand variables first (N, Dg...) and heterogeneity index afterwards
  # msd$variable <- factor(msd$variable, levels = c('N', 'Dg', 'BA', 'H', 'BAI_yr', 'Sh', 'GS', 'Simp', 'GI'))
  #
  # deviancedfMSD <- deviancedf[deviancedf$devMeasure == "MSD",]
  # relDiffdf <- deviancedf[deviancedf$devMeasure == 'relDiff',]
  # absDiffdf <- deviancedf[deviancedf$devMeasure == 'absDiff',]
  # Ydf <- deviancedf[deviancedf$devMeasure == 'Y',]
  # deviancedf <- deviancedf[!(deviancedf$devMeasure %in% c("MSD", 'relDiff', 'absDiff', 'Y')),]
  #
  # ################################################################################
  # evaluation and time series at stand level
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














  # pl1 <- ggplot(data = deviancedf[deviancedf$sp == 'allsp',], aes(x = mod, y = value, fill = devMeasure)) +
  #   geom_bar(stat = "identity") +
  #   facet_wrap(. ~ variable, scale = "free") +
  #   theme_light() +
  #   theme(panel.grid.minor = element_blank(),
  #       # panel.grid.major = element_blank(),
  #       strip.background = element_blank(),
  #       strip.text = element_text(colour = 'black'),
  #       legend.position = "bottom",
  #       legend.title = element_blank(),
  #       panel.spacing = unit(20, 'pt'))
  # ggsave(file = paste0('plotEval/', evalSite, '/eval.pdf'), plot = pl1, width = 8, height = 10)
  #
  # # plot relative difference between observations and predictions
  # pl2 <- ggplot(data = relDiffdf[relDiffdf$sp == 'allsp',], aes(x = mod, y = value)) +
  #   # geom_bar(stat = "identity") +
  #   geom_boxplot() +
  #   facet_wrap(. ~ variable, scale = "free") +
  #   theme_light() +
  #   theme(panel.grid.minor = element_blank(),
  #       # panel.grid.major = element_blank(),
  #       strip.background = element_blank(),
  #       strip.text = element_text(colour = 'black'),
  #       legend.position = "bottom",
  #       legend.title = element_blank(),
  #       panel.spacing = unit(20, 'pt'))
  # ggsave(file = paste0('plotEval/', evalSite, '/relDiff.pdf'), plot = pl2, width = 8, height = 10)
  #
  # # plot absolute difference between observations and predictions
  # pl3 <- ggplot(data = absDiffdf[absDiffdf$sp == 'allsp',], aes(x = mod, y = value)) +
  #   # geom_bar(stat = "identity") +
  #   geom_boxplot() +
  #   facet_wrap(. ~ variable, scale = "free") +
  #   theme_light() +
  #   theme(panel.grid.minor = element_blank(),
  #       # panel.grid.major = element_blank(),
  #       strip.background = element_blank(),
  #       strip.text = element_text(colour = 'black'),
  #       legend.position = "bottom",
  #       legend.title = element_blank(),
  #       panel.spacing = unit(20, 'pt'))
  # ggsave(file = paste0('plotEval/', evalSite, '/absDiff.pdf'), plot = pl3, width = 8, height = 10)

#
#   regdf <- cbind(absDiffdf, Ydf[,'value'])
#   colnames(regdf)[length(colnames(regdf))] <- 'Y'
#
#   ggplot(data = regdf[regdf$sp == 'allsp',]) +
#   geom_point(aes(x = Y, y = value, col = mod), alpha = 0.5) +
#   facet_wrap(. ~ variable, scale = "free") +
#   ylab('predicted - observed') +
#   xlab('observed') +
#   theme_light() +
#   theme(panel.grid.minor = element_blank(),
#       # panel.grid.major = element_blank(),
#       strip.background = element_blank(),
#       strip.text = element_text(colour = 'black'),
#       legend.position = "bottom",
#       legend.title = element_blank(),
#       panel.spacing = unit(20, 'pt'))
#
# #
# testdf <- regdf[regdf$sp == 'allsp' & regdf$mod == 'landclim',]
# mod1 <- lm(testdf[testdf$variable == 'Dg', 'value'] ~ testdf[testdf$variable == 'Sh', 'Y'])
# summary(mod1)$r.squared




  # ################################################################################
  # # radarchart relative MSD at stand level
  # ################################################################################
  #
  # # calculate max MSD for each variable at each site
  # evaluationMSD <- evaluationMSD[evaluationMSD$species == 'allsp',]
  # maxMSD <- ddply(evaluationMSD, .(site, variable), summarise, maxMSD = max(value))
  #
  # # calculate relative MSD
  # relativeMSD <- merge(evaluationMSD, maxMSD, by = c('site', 'variable'))
  # relativeMSD$relMSD <- relativeMSD$value / relativeMSD$maxMSD
  #
  # # long to wide for radarchart function
  # relativeMSD[, c('devMeasure', 'value' , 'species', 'maxMSD')] <- NULL
  # relativeMSD <- spread(relativeMSD, variable, relMSD)
  #
  # # radarchart
  # pdf(file = "./plotEval/radar.pdf", width = 13, height = 5)
  # par(mfrow = c(1,3))
  # # color vector
  # # colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  # # colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  # colors_border = viridis_pal(alpha = 1, option = 'viridis')(100)[c(1, 50, 100)]
  # colors_in = viridis_pal(alpha = 0.5, option = 'viridis')(100)[c(1, 50, 100)]
  #
  # for (i in unique(relativeMSD$site)){
  #   df <- relativeMSD[relativeMSD$site == i, ]
  #   rownames(df) <- df$mod
  #   df[, c('site', 'mod')] <- NULL
  #   df <- rbind(rep(1,ncol(df)) , rep(0,ncol(df)) , df)
  #   df <- round(df, 2)
  #   # remove columns with NA (ex: H at kroof site)
  #   df <- df[colSums(!is.na(df)) == nrow(df)]
  #
  #   # plot with default options:
  #   radarchart(df , axistype=1,
  #       # custom polygon
  #       pcol = colors_border, pfcol = colors_in, plwd = 1, plty = 1,
  #       # title
  #       title = i,
  #       # custom labels
  #       vlcex = 1.6,
  #       # custom the grid
  #       cglcol = "grey", cglty = 1, axislabcol = "black", cglwd = 1, seg = 5, caxislabels = seq(0,1,0.2), calcex = 1.5,
  #       # center
  #       centerzero = FALSE
  #       )
  # }
  #
  # # Add a legend
  # legend(x=0.55, y=-1, legend = rownames(df[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)
  #
  # # dev.copy2pdf(file = paste0('./plotEval/radar.pdf'), width = 13, height = 5)
  # dev.off()
  return(1)

}
