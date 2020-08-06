# MSD <- function(evaldf){
# # provide 'evaldf' dataframe with three columns:
# # year = year
# # Y = observations
# # X = predictions
#
#   # calculate x, y and x*y, b and r²
#   evaldf$x <- evaldf$X - mean(evaldf$X)
#   evaldf$y <- evaldf$Y - mean(evaldf$Y)
#   evaldf$xy <-evaldf$x * evaldf$y
#   model <- lm(evaldf$Y ~ evaldf$X)
#   b <- as.numeric(coef(model)[2]) # = sum(evaldf$xy) / sum(evaldf$x^2)
#   r2 <- summary(model)$r.squared # = ( sum(evaldf$xy)^2 ) / ( sum(evaldf$x^2)*sum(evaldf$y^2) )
#   # calculate MSD and its 3 components
#   MSD <- sum( (evaldf$X - evaldf$Y) ^2) / nrow(evaldf)
#   SB <- (mean(evaldf$X) - mean(evaldf$Y))^2
#   NU <- ((1 - b)^2) * ( sum(evaldf$x^2) / nrow(evaldf) )
#   LC <- (1 - r2) * ( sum(evaldf$y^2) / nrow(evaldf) )
#
#   return(c(MSD, SB, NU, LC))
# }



calc_msd <- function(evaldf, mod, groups){

# input #
# evaldf:   a dataframe with at least X (predictions) and Y (observations) columns:
# mod:      the name of the model
# groups:   grouping factors
#
# output #
# mean square deviation and its 3 components
#

  # X and Y must have the same length (no missing value in X or Y)
  evaldf <- evaldf[!is.na(evaldf$X) & !is.na(evaldf$Y),]

  # Calculation
  evaldf <- evaldf %>% group_by_at(groups) %>% mutate(x = X - mean(X, na.rm = TRUE),
                                                 y = Y - mean(Y, na.rm = TRUE),
                                                 xy = x * y)
  MSD <- evaldf %>% group_by_at(groups) %>%  dplyr::summarise(MSD = sum( (X - Y) ^2, na.rm = TRUE) / length(X))
  SB <- evaldf %>% group_by_at(groups) %>%  dplyr::summarise(SB = ( mean(X, na.rm = TRUE) - mean(Y, na.rm = TRUE) )^2 )
  br2 <- evaldf %>% group_by_at(groups) %>% dplyr::summarise(b = sum(xy, na.rm = TRUE) / sum(x^2, na.rm = TRUE),
                                                                  r2 = ( sum(xy, na.rm = TRUE)^2 ) / ( sum(x^2, na.rm = TRUE)*sum(y^2, na.rm = TRUE) ))
  br2$NU1 <- (1 - br2$b)^2
  br2$LC1 <- 1 - br2$r2
  NU2 <- evaldf %>% group_by_at(groups) %>% dplyr::summarise(NU2 = sum(x^2, na.rm = TRUE) / length(x))
  LC2 <- evaldf %>% group_by_at(groups) %>% dplyr::summarise(LC2 = sum(y^2, na.rm = TRUE) / length(y))
  NU <- merge(br2, NU2, by = groups)
  NU$NU <- NU$NU1 * NU$NU2
  LC <- merge(br2, LC2, by = groups)
  LC$LC <- LC$LC1 * LC$LC2

  msd3 <- merge(MSD, SB, by = groups)
  msd3 <- merge(msd3, NU[, c(groups, 'NU')])
  msd3 <- merge(msd3, LC[, c(groups, 'LC')])
  msd3$mod <- mod

  return(msd3)

}



#
#
#
# df1 <- evaldf %>% group_by(species, variable)
#
# groups <- c('species', 'variable')
# df2 <- evaldf %>% group_by_at(groups)
#
#
#
#
#
#
#
# df1 <- data %>%
#   group_by_at(vars(one_of(columns))) %>%
#   summarize(Value = mean(value))
