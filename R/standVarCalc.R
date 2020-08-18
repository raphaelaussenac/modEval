standVarCalc <- function(evalSite){

  # load data
  alldf <- read.csv(paste0('./data/all_', evalSite, '.csv'))

  ##############################################################################
  # calculate yearly aggregated index (N, Dg, H, BA, V) for observed and
  # predicted data
  ##############################################################################

  # sp level
  YearlySP <- group_by(alldf, year, src, site, species) %>%
        dplyr::summarise(N = sum(weight),
                  Dg = sqrt(sum(D_cm^2 * weight)/sum(weight)),
                  H = sum(H_m * weight) /sum(weight),
                  BA = sum((pi * (D_cm/200)^2) * weight),
                  V = sum(V_m3 * weight)) %>% ungroup()
  #
  # stand level
  YearlySt <- group_by(alldf, year, src, site) %>%
        dplyr::summarise(N = sum(weight),
                  Dg = sqrt(sum(D_cm^2 * weight)/sum(weight)),
                  H = sum(H_m * weight) /sum(weight),
                  BA = sum((pi * (D_cm/200)^2) * weight),
                  V = sum(V_m3 * weight)) %>% ungroup() %>% mutate(species='allsp')
  #
  # calculate BAI
  Yearly <- rbind(YearlySP, YearlySt)
  # select years for which BA were observed --> calculate mean BAI/yr from pred BA
  # on same period than in obs dataset
  YearlyObs <- group_by(Yearly, site) %>% dplyr::filter(year %in% unique(year[src=='data'])) %>% ungroup()
  YearlyPred <- group_by(Yearly, site) %>% dplyr::filter(!(year %in% unique(year[src=='data']))) %>% ungroup()

  # function to retrieve previous year and BA
  yb <- function(variable){
        if (length(variable)>1){
                return(c(NA,variable[1:(length(variable)-1)]))
        }else{
                return(NA)
        }
  }

  DecYear <- 0
  if (evalSite=='bauges'){DecYear <- 1} # growth period + 1 for the bauges data set
  YearlyObs <- group_by(YearlyObs, site, src, species) %>%
        dplyr::mutate(yearbefore=yb(year), BAbefore=yb(BA), BAI_yr=(BA-BAbefore)/(year-yearbefore+DecYear)) %>%
        ungroup() %>% dplyr::select(-yearbefore, -BAbefore)

  df <- rbind(YearlyObs, mutate(YearlyPred, BAI_yr=NA))

  ################################################################################
  # calculate heterogeneity index at stand level
  ################################################################################

  # diameter class diversity
  out <- ReturnDivIndex(evalSite, "D_cm", 10)
  out <- data.frame(out)
  out <- out[, c('year', 'site', 'src', 'Sh', 'GS', 'GI', 'SkewD')]

  # species diversity
  divOut <- ReturnDivIndex(evalSite, "species")
  divOut <- data.frame(divOut)
  divOut <- divOut[, c('year', 'site', 'src', 'Sh', 'GS')]
  colnames(divOut)[!(colnames(divOut) %in% c('year', 'site', 'src'))] <- paste0(colnames(divOut)[!(colnames(divOut) %in% c('year', 'site', 'src'))], 'sp')

  # add heterogeneity index to df
  df <- merge(df, out, by= c('year', 'src', 'site'))
  df <- merge(df, divOut, by = c('year', 'src', 'site'))

  # write site index
  write.csv(df, paste0('./data/eval_', evalSite, '.csv'), row.names = FALSE)
  return(file.exists(paste0('./data/eval_', evalSite, '.csv')))
}
