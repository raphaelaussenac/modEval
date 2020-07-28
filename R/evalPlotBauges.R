# clean up environment
rm(list = ls())

setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval")

evalSite <- 'bauges'


################################################################################

# load packages
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(plyr)
library(viridis)
source('R/msd.R')

alldf <- read.csv(paste0('./data/eval_', evalSite, '.csv'))
alldf <- alldf[order(alldf$src, alldf$site, alldf$species, alldf$year),]

# verify whether mortality is switched off
# diffTree <- ddply(alldf[alldf$species == "allsp" & alldf$src == 'data',], .(site), summarise, diffNbTree = min(N) - max(N))
# table(diffTree$diffNbTree)

# keep only index associated to last growth year
lastYear <- ddply(alldf, .(site), summarise, lastYear = max(year))
alldf <- merge(alldf, lastYear, by = 'site')
alldf <- alldf[alldf$year == alldf$lastYear,]
alldf$lastYear <- NULL

################################################################################
# evaluation
################################################################################

# list of evaluation variables
alldf$V <- NULL
evalVar <- c('N', 'Dg', 'H', 'BA','BAI_yr', 'Sh', 'GS', 'Simp', 'GI')

# create evaluation data set
evaluation <- data.frame()
# for each model
for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){
# at each site
  for (site in unique(alldf$site)){
    # for each sp
    for (sp in unique(alldf$species)){
    # create df with index values
      obs <- alldf[alldf$src == 'data' & alldf$site == site & alldf$species == sp, ]
      colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site', 'species'))] <- paste('obs', colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site', 'species'))], sep = '')
      obs[, c('src', 'site', 'species')] <- NULL
      pred <- alldf[alldf$src == mod & alldf$site == site & alldf$species == sp, ]
      colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site', 'species'))] <- paste('pred', colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site', 'species'))], sep = '')
      # merge obs and pred
      df <- merge(obs, pred, by = 'year')
      evaluation <- rbind(evaluation, df)
    }
  }
}

# evaluation
deviancedf <- data.frame('variable'= NA, 'devMeasure'= NA, 'value' = NA, 'mod' = NA, 'sp' = NA)
# for each model
for (mod in unique(alldf$src)[unique(alldf$src) != 'data']){
  # for each sp
  for (sp in unique(evaluation$species)){
    for (i in evalVar){
      # create evaluation df
      evaldf <- evaluation[evaluation$species == sp & evaluation$src == mod, c('year', colnames(evaluation)[colnames(evaluation) == paste('obs', i, sep = '') | colnames(evaluation) == paste('pred', i, sep = '')])]
      colnames(evaldf)[substr(colnames(evaldf), 1, 3) == 'obs'] <- 'Y'
      colnames(evaldf)[substr(colnames(evaldf), 1, 4) == 'pred'] <- 'X'
      # calculate deviance
      if (nrow(evaldf) > 0){
        # calculate MSD and its 3 components
        msd <- MSD(evaldf)
        # save
        deviancedf <- rbind(deviancedf, c(i, 'MSD', msd[1], as.character(mod), as.character(sp)))
        deviancedf <- rbind(deviancedf, c(i, 'SB', msd[2], as.character(mod), as.character(sp)))
        deviancedf <- rbind(deviancedf, c(i, 'NU', msd[3], as.character(mod), as.character(sp)))
        deviancedf <- rbind(deviancedf, c(i, 'LC', msd[4], as.character(mod), as.character(sp)))
      } else {
        deviancedf <- rbind(deviancedf, c(i, 'MSD', NA, as.character(mod), as.character(sp)))
        deviancedf <- rbind(deviancedf, c(i, 'SB', NA, as.character(mod), as.character(sp)))
        deviancedf <- rbind(deviancedf, c(i, 'NU', NA, as.character(mod), as.character(sp)))
        deviancedf <- rbind(deviancedf, c(i, 'LC', NA, as.character(mod), as.character(sp)))
      }
    }
  }
}
deviancedf <- deviancedf[-1,]
deviancedf$value <- as.numeric(deviancedf$value)
deviancedf$variable <- as.factor(deviancedf$variable)
deviancedf$devMeasure <- as.factor(deviancedf$devMeasure)
deviancedf$mod <- as.factor(deviancedf$mod)
deviancedf$sp <- as.factor(deviancedf$sp)

deviancedfMSD <- deviancedf[deviancedf$devMeasure == "MSD",]
deviancedf <- deviancedf[deviancedf$devMeasure != "MSD",]

################################################################################
# evaluation and time series at stand level
################################################################################
# create directory to save plots
if (!(dir.exists(paste0('plotEval/', evalSite)))){dir.create(paste0('plotEval/', evalSite), recursive = TRUE)}

#
pl <- ggplot(data = deviancedf[deviancedf$sp == 'allsp',], aes(x = mod, y = value, fill = devMeasure)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ variable, scale = "free")+
  theme_light() +
  theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
ggsave(file = paste0('plotEval/', evalSite, '/eval.pdf'), plot=pl, width = 8, height = 5)

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
# return(1)
