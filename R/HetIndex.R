require(dplyr)

ChooseVar <- function(dataSet, Nvar = "D_cm", Inter = 10){
# dataset is model output, each line is tree.year data, Nvar the variable used
# Choose variable to analyse, if not species Inter defines the different classes
    dataSet <- mutate(dataSet, Var=dataSet[[Nvar]])
    if (Nvar %in% c('D_cm','H_m','V_m3')){
	    dataSet <- mutate(dataSet, Class = (1+floor((Var-Inter/2)/Inter))*Inter)
    }else if (Nvar=='species'){
	    dataSet <- mutate(dataSet, Class=Var)
    }else{
	    stop('Nvar specified not in output')
    }
    return(dataSet)
}

CalcHill <- function(dataSet){
# dataSet after ChooseVar, return GS : Gini-Simpson, Sh : Shannon, N : Nb for each year
    if (is.null(dataSet[["Var"]])){stop('Need to choose variable first')}
    PClass <- group_by(dataSet, year) %>% mutate(N = n()) %>% ungroup() %>%
	    group_by(year, Class) %>% summarise(p=n()/N[1]) %>% ungroup()
    HillNB <- group_by(PClass, year) %>% summarise(Sh=-sum(p * log(p)),
	    N=n(), GS=1-sum(p^2), Simp=sum(p^2)) %>% ungroup()
    return(HillNB)
}

ReturnHill <- function(Nvar='D_cm', model='4c', site='kroof', Inter=10, path='.'){
	# model and site can be list, Nvar cannot
    listfiles <- intersect(list.files(path=path, pattern=paste(model,collapse='|')),
        list.files(path=path, pattern=paste(site,collapse='|')))
        if (length(listfiles) == 0){
          stop('no files found in directory')
        }
    Hill <- NULL
    for (ifile in listfiles){
         dataTemp <- read.csv(file=paste(path,ifile,sep=.Platform$file.sep))
         dataTemp <- ChooseVar(dataTemp, Nvar=Nvar, Inter=Inter)
	 Hill <- rbind(Hill,mutate(CalcHill(dataTemp), model=strsplit(ifile,'_')[[1]][1],
	   site=strsplit(ifile,'_')[[1]][2], Inter=Inter, Nvar=Nvar))
    }
    return(Hill)
}

Example <- function(){
	require(ggplot2)
    Nvar <- "D_cm"
    Inter <- 10
    model <- c('profound', '4c', 'landclim', 'Salem')
    site <- c('kroof','solling-spruce', 'solling-beech')
    OUT <- ReturnHill(Nvar, model, site, Inter)
    ggplot(OUT, aes(x=year, y=Sh, col=model)) + geom_line() +
        facet_wrap(~site) + ggtitle(paste(Nvar,"Shannon"))
}
