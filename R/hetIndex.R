require(dplyr)

ChooseVar <- function(dataSet, Nvar = "D_cm", Inter = 10){
# dataset is model output, each line is tree.year data, Nvar the variable used
# Choose variable to analyse, if not species Inter defines the different classes
    dataSet <- mutate(dataSet, Var=dataSet[[Nvar]])
    if (Nvar %in% c('D_cm','H_m','V_m3')){
	    dataSet <- mutate(dataSet, Class = (1+floor((Var-Inter/2)/Inter))*Inter)
    }else if (Nvar=='species'){
	    dataSet <- mutate(dataSet, Class=as.character(Var))
    }else{
	    stop('Nvar specified not in output')
    }
    return(dataSet)
}

CalcDivIndex <- function(dataSet, type='BA'){
# dataSet after ChooseVar, return GS : Gini-Simpson, Sh : Shannon, N : Nb for each year
# type defines whether we use directly frequency or relative BA to compute metrics
    if (is.null(dataSet[["Var"]])){stop('Need to choose variable first')}
    dataSet <- mutate(dataSet, BA=pi*(D_cm/200)^2*weight)
    SkewNess <- group_by(dataSet, year, site, src) %>%
      dplyr::summarise(Dmean=sum(weight*D_cm)/sum(weight),
      sdD=sqrt(1/sum(weight) * sum((D_cm-Dmean)^2 * weight)),
      SkewD=1/sum(weight) * sum(weight * ((D_cm-Dmean)/sdD)^3)) %>% ungroup()
    ###
    SkewNess <- dplyr::select(SkewNess,-Dmean, -sdD)
    ###
    PClass <- group_by(dataSet, year, site, src) %>% mutate(N = sum(weight), BAt=sum(BA)) %>%
      ungroup() %>% group_by(year, Class, site, src) %>% dplyr::summarise(p=(sum(weight)/N[1]),
      pBA=sum(BA)/BAt[1], BAt=BAt[1]) %>% ungroup()
    if (type=='BA'){PClass$p <- PClass$pBA}
    HillNB <- group_by(PClass, year, site, src) %>% dplyr::summarise(Sh=-sum(p * log(p)),
	    NClass=n(), GS=1-sum(p^2), Simp=sum(p^2)) %>% ungroup()
    if (is.numeric(dataSet$Var)){
      GiniIndex <- group_by(dataSet, year, site, src) %>% dplyr::summarise(GI=Gini(Var,BA,weight)) %>% ungroup()
      DivIndex <- left_join(HillNB, GiniIndex, by=c('year','site','src'))
    }else{
      DivIndex <- mutate(HillNB, GI=NA)
    }
    DivIndex <- left_join(DivIndex, SkewNess, by=c('site','year','src'))
    return(DivIndex)
}

ReturnDivIndex <- function(evalSite, Nvar='D_cm', Inter=10, path='data'){
    if (!file.exists(paste0(path,'/','all_',evalSite,'.csv'))){stop('Need to build dataset first')}
    dataTemp <- read.csv(file=paste0(path,'/','all_',evalSite,'.csv'))
    dataTemp <- ChooseVar(dataTemp, Nvar=Nvar, Inter=Inter)
    DivIndex <- CalcDivIndex(dataTemp)
    return(as.data.frame(DivIndex))
}

Gini <- function (V, BA = rep(1, length = length(x)), weight = rep(1, length = length(x))){
    oV <- order(V)
    V <- V[oV]
    BA <- BA[oV]
    weight <- weight[oV]
    ## Lorenz Curve
    x <- cumsum(weight)/sum(weight) # CDF share of pop
    y <- cumsum(BA)/sum(BA) # CDF share of  BA
    if (length(x)==1){
      dx <- 0
    }else{
      dx <- diff(x)
    }
    A <- sum(c(x[1],dx)*(y+c(0,y[1:(length(y)-1)]))/2) # Area under the Lorenz Curve
    return(1-2*A)
}
