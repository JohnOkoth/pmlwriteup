dt.AdjNames<-function(LEGEND,NAMES, MODE){
  for(i in names(NAMES)){
    if(is.null(LEGEND[[i]])){
      LEGEND[[i]] = NAMES[[i]]
    }
  }
  if(MODE =="PvO"){
    LEGEND$CURVES = paste0(c(LEGEND$OBSERVED,LEGEND$MODELS),"-")
  }else if(MODE == "LR"){
    LEGEND$CURVES = paste0(LEGEND$MODELS,"-")
  }else{
    cat("MODE not supported, only 'LR' & 'PvO' are supported! \n")
    return(0)
  }
  return(LEGEND)
}


dt.LoadFromFeather <- function(DATA,NAMES){
  ## if a feather path is put in, load from feather
  if(is.character(DATA)){
    DT.paths <- DATA
    DATA <- data.frame()
    for(i in DT.paths){
      DATA <- rbind(DATA,
                    read.feather(i,
                                 KEEP = as.vector(unlist(NAMES))))
    }
  }
  return(DATA)
}

dt.CleanData <- function(DATA){
  for(i in names(DATA)){
    NA.ind <- is.na(DATA[,i])
    if(sum(NA.ind)>0){
      cat(paste0(sum(NA.ind)," (",percent(sum(NA.ind)/length(NA.ind)),") missing value(s) were removed from ",i,"\n" ))
      DATA = DATA[!is.na(DATA[,i]),]
    }
  }
  return(DATA)
}

dt.Rebase <- function(DATA, REBASE, NAMES){
  if(REBASE){
    for(i in NAMES$MODELS){
      DATA[,i] = DATA[,i] * sum(DATA[,NAMES$OBSERVED]) / sum(DATA[,i]*DATA[,NAMES$EXPOSURE])
    }
  }

  return(DATA)
}

dt.Earn <- function(DATA,NAMES){
  DATA[,c(NAMES$SPLIT.BY,
          NAMES$EXPOSURE,
          NAMES$VARIABLE,
          NAMES$MODELS,
          NAMES$OBSERVED)]

  ## Earn the fitted data
  DATA[,NAMES$MODELS] = DATA[,NAMES$MODELS]* DATA[,NAMES$EXPOSURE]

  return(DATA)
}


dt.Bucket <- function(DATA,NAMES,CUTS,N.BKTS){
  library(Hmisc)
  if (is.numeric( DATA[,NAMES$VARIABLE] )){
    if(is.null(CUTS)){
      CUTS <- Hmisc::wtd.quantile(DATA[,NAMES$VARIABLE],
                                  weights = DATA[,NAMES$EXPOSURE],
                                  probs = seq(1/N.BKTS, 1 - 1/N.BKTS, by = 1/N.BKTS))
      CUTS <- unique(CUTS)
    }
    DATA[,NAMES$VARIABLE] <- as.factor( cut2(DATA[,NAMES$VARIABLE],  cuts=CUTS))
  }else{
    DATA[,NAMES$VARIABLE] <- as.factor(DATA[,NAMES$VARIABLE])
  }

  return(DATA)
}


dt.Aggregate <-function(DATA, NAMES){
  if(is.null(NAMES$SPLIT.BY)){

    DATA.AGG <- data.frame(NB.OBS = rep(1,nrow(DATA)), DATA)

    DATA.AGG <-  DATA.AGG %>%
      group_by_(NAMES$VARIABLE) %>%
      summarise_each_(funs(sum), c(NAMES$MODELS,
                                   NAMES$OBSERVED,
                                   "NB.OBS",
                                   NAMES$EXPOSURE)) %>%
      mutate()

    DATA.AGG <- as.data.frame(DATA.AGG)

  }else{

    DATA.AGG <- DATA.AGG1 <- data.frame(NB.OBS = rep(1,nrow(DATA)), DATA)

    DATA.AGG1 <-  DATA.AGG1[,!(names(DATA.AGG1) %in% NAMES$SPLIT.BY)] %>%
      group_by_(NAMES$VARIABLE) %>%
      summarise_each_(funs(sum), c(NAMES$MODELS,
                                   NAMES$OBSERVED,
                                   "NB.OBS",
                                   NAMES$EXPOSURE)) %>%
      mutate()

    DATA.AGG1 <- data.frame( TEMP="Combined", as.data.frame(DATA.AGG1))
    names(DATA.AGG1)[which(names(DATA.AGG1)=="TEMP")] = NAMES$SPLIT.BY


    DATA.AGG <-  DATA.AGG %>%
      group_by_(NAMES$SPLIT.BY,NAMES$VARIABLE) %>%
      summarise_each_(funs(sum), c(NAMES$MODELS,
                                   NAMES$OBSERVED,
                                   "NB.OBS",
                                   NAMES$EXPOSURE)) %>%
      mutate()

    DATA.AGG<-  rbind(as.data.frame(DATA.AGG),
                      DATA.AGG1)
  }
  return(DATA.AGG)
}


dt.FixNames<-function(DATA,NAMES,LEGEND){

  for(i in names(NAMES)){
    names(DATA)[which(names(DATA) %in% NAMES[[i]])] = LEGEND[[i]]
  }

  return(DATA)
}

dt.Adjust<-function(DATA, LEGEND, MODE){

  if(MODE == "LR"){
    DATA = DATA %>%
      cbind(., DATA[,LEGEND$OBSERVED] / DATA[,c(LEGEND$MODELS)])

    names(DATA)[(length(DATA)-length(LEGEND$MODELS) + 1):length(DATA)] = LEGEND$CURVES

    if(length(LEGEND$CURVES) > 1){
      DATA[,LEGEND$CURVES] = apply(DATA[,LEGEND$CURVES],2,function(x){x[x>5] = 5; return(x)})
    }else{
      DATA[DATA[,LEGEND$CURVES]>5, LEGEND$CURVES] = 5
    }

  }else if(MODE == "PvO"){
    DATA = DATA %>%
      cbind(., DATA[,c(LEGEND$OBSERVED,LEGEND$MODELS)]/DATA[,LEGEND$EXPOSURE])

    names(DATA)[(length(DATA)-length(LEGEND$MODELS)):length(DATA)] = LEGEND$CURVES
  }else{
    cat("MODE not supported, only 'LR' & 'PvO' are supported! \n")
    return(0)
  }

  return(DATA)
}


dt.AdjustForPlot<-function(DATA,LEGEND){
  library(reshape2)

  if(is.null(LEGEND$SPLIT.BY)){
    DATA.PL <- melt(DATA , id=LEGEND$VARIABLE)
  }else{
    DATA.PL  <- melt(DATA , id=c(LEGEND$SPLIT.BY,LEGEND$VARIABLE))

    DATA.PL$variable2 = as.factor(paste0(DATA.PL[,LEGEND$SPLIT.BY], DATA.PL[,LEGEND$VARIABLE],sep = " - "))
  }

  TOT.XPO <- sum(DATA.PL[DATA.PL$variable == LEGEND$EXPOSURE, "value"])

  DATA.PL[DATA.PL$variable == LEGEND$EXPOSURE, "value"] <- DATA.PL[DATA.PL$variable == LEGEND$EXPOSURE, "value"] /
    max(DATA.PL[DATA.PL$variable == LEGEND$EXPOSURE, "value"]) *
    median(DATA.PL[DATA.PL$variable %in% LEGEND$CURVES, "value"])*.25

  return(DATA.PL)
}
