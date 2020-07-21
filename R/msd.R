MSD <- function(evaldf){

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

  return(c(MSD, SB, NU, LC))
}
