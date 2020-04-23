################################################################################
# initialisation
################################################################################

# clean up environment
rm(list = ls())

# load packages
library(ggplot2)

# set work directory
setwd("C:/Users/raphael.aussenac/Documents/GitHub/modEval/indexForEval")

# load files
fileNames <- Sys.glob('*.csv')

# load data and assign model and site names to it
listsrc <- c('profound', 'landclim', '4c')
listsite <- c('kroof', 'solling_beech', 'solling_spruce')
alldf <- data.frame()
for (src in listsrc){
  for (i in grep(pattern = src, x = fileNames)){
    temp <- read.csv(fileNames[i])
    # detect site
    for (s in listsite){
      if(grepl(pattern = s, x = fileNames[i])){site <- s}
    }
    temp$src <- src
    temp$site <- site
    alldf <- rbind(alldf, temp)
  }
}

# manage different plot scales
# kroof is 0.5ha while other sites are 1ha
# N is therefore lower at kroof which could make MSD lower too
# to make it possible to compare deviance among sites
# Nobs and Npred must be multiplied by 2 ()
alldf[alldf$site == 'kroof', 'N'] <- alldf[alldf$site == 'kroof', 'N'] * 2


################################################################################
# evaluation
################################################################################

# list of evaluation variables
evalVar <- c('N', 'Dg')

evaluation <- data.frame()
# for each model
for (mod in listsrc[listsrc != 'profound']){

  # at each site
  for (site in listsite){
    # create df with index values
    obs <- alldf[alldf$src == 'profound' & alldf$site == site, ]
    colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site'))] <- paste('obs', colnames(obs)[!(colnames(obs) %in% c('year', 'src', 'site'))], sep = '')
    obs[, c('src', 'site')] <- NULL
    pred <- alldf[alldf$src == mod & alldf$site == site, ]
    colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site'))] <- paste('pred', colnames(pred)[!(colnames(pred) %in% c('year', 'src', 'site'))], sep = '')
    df <- merge(obs, pred, by = 'year')

    # for each index to compare
    deviancedf <- data.frame('variable'= NA, 'devMeasure'= NA, 'value' = NA)
    for (i in evalVar){
      # create evaluation df
      evaldf <- df[, c('year', colnames(df)[colnames(df) == paste('obs', i, sep = '') | colnames(df) == paste('pred', i, sep = '')])]
      colnames(evaldf)[substr(colnames(evaldf), 1, 3) == 'obs'] <- 'Y'
      colnames(evaldf)[substr(colnames(evaldf), 1, 4) == 'pred'] <- 'X'
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
      deviancedf <- rbind(deviancedf, c(i, 'SB', SB))
      deviancedf <- rbind(deviancedf, c(i, 'NU', NU))
      deviancedf <- rbind(deviancedf, c(i, 'LC', LC))
    }
    deviancedf <- deviancedf[-1,]
    deviancedf$value <- as.numeric(deviancedf$value)
    deviancedf$mod <- mod
    deviancedf$site <- site

    # save
    evaluation <- rbind(evaluation, deviancedf)
  }
}

# plot
ggplot(data = evaluation, aes(x = mod, y = value, fill = devMeasure)) +
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
ggsave(paste('eval.jpg', sep = '_'), width = 8, height = 8)
