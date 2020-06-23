################################################################################
# initialisation
################################################################################

# clean up environment
rm(list = ls())

# load packages
library(ggplot2)
library(reshape2)
library(tidyr)
library(fmsb)
library(plyr)
library(viridis)

# set work directory
# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  setwd("/Users/raphaelaussenac/Documents/GitHub/modEval/indexForEval")
} else if (Sys.info()["sysname"] == "Windows"){
  setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval/indexForEval")
}

alldf <- read.csv('evalVarSp.csv')
alldf <- alldf[order(alldf$src, alldf$site, alldf$species, alldf$year),]

################################################################################
# evaluation
################################################################################

# list of evaluation variables
alldf$V <- NULL
evalVar <- c('N', 'Dg', 'H', 'BA','BAI_yr', 'Sh', 'GS', 'Simp')

evaluation <- data.frame()
# for each model
for (mod in unique(alldf$src)[unique(alldf$src) != 'profound']){

  # at each site
  for (site in unique(alldf$site)){
    for (sp in unique(alldf$species)){
      # create df with index values
      obs <- alldf[alldf$src == 'profound' & alldf$site == site & alldf$species == sp, ]
      colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site', 'species'))] <- paste('obs', colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site', 'species'))], sep = '')
      obs[, c('src', 'site', 'species')] <- NULL
      pred <- alldf[alldf$src == mod & alldf$site == site & alldf$species == sp, ]
      colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site', 'species'))] <- paste('pred', colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site', 'species'))], sep = '')
      # merge obs and pred
      df <- merge(obs, pred, by = 'year')

      # if species present
      if (nrow(df) > 0){
        # for each index to compare
        deviancedf <- data.frame('variable'= NA, 'devMeasure'= NA, 'value' = NA)
        for (i in evalVar){
          # no evaluation of Volume
          if (i != 'V'){
            # no evaluation of H at kroof site
            if (i != 'H' | site != 'kroof'){
              # create evaluation df
              evaldf <- df[, c('year', colnames(df)[colnames(df) == paste('obs', i, sep = '') | colnames(df) == paste('pred', i, sep = '')])]
              colnames(evaldf)[substr(colnames(evaldf), 1, 3) == 'obs'] <- 'Y'
              colnames(evaldf)[substr(colnames(evaldf), 1, 4) == 'pred'] <- 'X'
              # remove NA values (for BAI_yr)
              if (i == 'BAI_yr'){
                evaldf <- evaldf[!is.na(evaldf$X), ]
                evaldf <- evaldf[!is.na(evaldf$Y), ]
              }
              # then if there is some data left
              if (nrow(evaldf) > 0){
                # calculate x, y and x*y, b and r²
                evaldf$x <- evaldf$X - mean(evaldf$X)
                evaldf$y <- evaldf$Y - mean(evaldf$Y)
                evaldf$xy <-evaldf$x * evaldf$y
                model <- lm(evaldf$Y ~ evaldf$X)
                b <- as.numeric(coef(model)[2]) # = sum(evaldf$xy) / sum(evaldf$x^2)
                r2 <- summary(model)$r.squared # = ( sum(evaldf$xy)^2 ) / ( sum(evaldf$x^2)*sum(evaldf$y^2) )
                # calculate MSD and its 3 components
                MSD <- sum( (evaldf$X - evaldf$Y) ^2) / nrow(evaldf)
                SB <- (mean(evaldf$X) - mean(evaldf$Y))^2
                NU <- ((1 - b)^2) * ( sum(evaldf$x^2) / nrow(evaldf) )
                LC <- (1 - r2) * ( sum(evaldf$y^2) / nrow(evaldf) )
                # save
                deviancedf <- rbind(deviancedf, c(i, 'MSD', MSD))
                deviancedf <- rbind(deviancedf, c(i, 'SB', SB))
                deviancedf <- rbind(deviancedf, c(i, 'NU', NU))
                deviancedf <- rbind(deviancedf, c(i, 'LC', LC))
              } else {
                deviancedf <- rbind(deviancedf, c(i, 'MSD', NA))
                deviancedf <- rbind(deviancedf, c(i, 'SB', NA))
                deviancedf <- rbind(deviancedf, c(i, 'NU', NA))
                deviancedf <- rbind(deviancedf, c(i, 'LC', NA))
              }
            }
          }
        }
        deviancedf$value <- as.numeric(deviancedf$value)
        deviancedf$mod <- mod
        deviancedf$site <- site
        deviancedf$species <- sp
        deviancedf <- deviancedf[-1,]
        # save
        evaluation <- rbind(evaluation, deviancedf)
      }
    }
  }
}

evaluationMSD <- evaluation[evaluation$devMeasure == "MSD",]
evaluation <- evaluation[evaluation$devMeasure != "MSD",]

################################################################################
# evaluation and time series at stand level
################################################################################

# evaluation
evaluation$site = factor(evaluation$site, levels = unique(alldf$site))
evaluation$mod = factor(evaluation$mod, levels = unique(alldf$src))
evaluation$variable = factor(evaluation$variable, levels = evalVar)

ggplot(data = evaluation[evaluation$species == 'allsp',], aes(x = mod, y = value, fill = devMeasure)) +
geom_bar(stat = "identity") +
facet_grid(variable ~ site, scale = "free") +
theme_light() +
theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
ggsave(paste('eval.jpg', sep = '_'), width = 8, height = 12)


# time series
stand <- alldf[alldf$species == 'allsp',]
stand$species <- NULL
ts <- melt(stand, id.vars = c("year", "src", "site"))
ts$site = factor(ts$site, levels = unique(alldf$site))
ts$src = factor(ts$src, levels = unique(alldf$src))
ts$variable = factor(ts$variable, levels = evalVar)
ts$year <- as.numeric(ts$year)

ggplot() +
geom_line(data = ts[ts$src == 'profound' & !is.na(ts$value),], aes(x = year, y = value, col = src)) +
geom_path(data = ts[ts$src != 'profound'& !is.na(ts$value),], aes(x = year, y = value, col = src), linetype = 'dashed') +
facet_grid(variable ~ site, scale = "free") +
theme_light() +
theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
ggsave(paste('ts.jpg', sep = '_'), width = 8, height = 12)


################################################################################
# evaluation and time series at sp level
################################################################################

# evaluation
spLevelVariable <- c('N', 'Dg', 'BA', 'BAI_yr')
sp <- evaluation[evaluation$site == 'kroof' & evaluation$sp != "allsp" & evaluation$variable %in% spLevelVariable,]
ggplot(data = sp, aes(x = mod, y = value, fill = devMeasure)) +
geom_bar(stat = "identity") +
facet_grid(variable ~ species, scale = "free") +
theme_light() +
theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
ggsave(paste('evalSpKroof.jpg', sep = '_'), width = 8, height = 8)


# time series
sp <- alldf[alldf$site == 'kroof' & alldf$species != 'allsp',]
ts <- melt(sp, id.vars = c("year", "src", 'site', "species"))
ts$site = factor(ts$site, levels = unique(alldf$site))
ts$src = factor(ts$src, levels = unique(alldf$src))
ts$variable = factor(ts$variable, levels = evalVar)
ts <- ts[ts$variable %in% spLevelVariable,]

ggplot() +
geom_line(data = ts[ts$src == 'profound' & !is.na(ts$value),], aes(x = year, y = value, col = src)) +
geom_line(data = ts[ts$src != 'profound' & !is.na(ts$value),], aes(x = year, y = value, col = src), linetype = 'dashed') +
facet_grid(variable ~ species, scale = "free") +
theme_light() +
theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
ggsave(paste('tsSpKroof.jpg', sep = '_'), width = 8, height = 8)


################################################################################
# radarchart relative MSD at stand level
################################################################################

# calculate max MSD for each variable at each site
evaluationMSD <- evaluationMSD[evaluationMSD$species == 'allsp',]
maxMSD <- ddply(evaluationMSD, .(site, variable), summarise, maxMSD = max(value))

# calculate relative MSD
relativeMSD <- merge(evaluationMSD, maxMSD, by = c('site', 'variable'))
relativeMSD$relMSD <- relativeMSD$value / relativeMSD$maxMSD

# long to wide for radarchart function
relativeMSD[, c('devMeasure', 'value' , 'species', 'maxMSD')] <- NULL
relativeMSD <- spread(relativeMSD, variable, relMSD)

# radarchart
par(mfrow = c(1,3))
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

dev.copy2pdf(file = paste0('radar.pdf'), width = 13, height = 5)
