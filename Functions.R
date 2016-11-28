
#Percent converter
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}



dt.AdjNames<-function(LEGEND,NAMES, MODE){
  
  ## Replaces " " With "_" in names
  for(i in names(LEGEND)){
    if(!is.null(LEGEND[[i]])){
      LEGEND[[i]] = gsub(" ","_",LEGEND[[i]])
    }
  }
  
  ## if NULL then use NAMES
  for(i in names(NAMES)){
    if(is.null(LEGEND[[i]])){
      LEGEND[[i]] = NAMES[[i]]
    }
  }
  
  return(LEGEND)
}

dt.AdjNames2<-function(LEGEND,NAMES, MODE){
  
  if(MODE =="PvO"){
    LEGEND$PvO.CURVES = paste0(c(LEGEND$OBSERVED,LEGEND$MODELS),"_&mu;")
    LEGEND$PvO.TITLE = paste0("Predicted vs. Observed Chart with Respect to <br>", td.Frmt(LEGEND$VARIABLE))
  }else if(MODE == "LR"){
    LEGEND$LR.CURVES = paste0(LEGEND$MODELS,"_&Delta;")
    LEGEND$LR.TITLE = paste0("Double Lift Chart with Respect to <br>", td.Frmt(LEGEND$VARIABLE))
  }else if(MODE== "ALL"){
    LEGEND$PvO.CURVES = paste0(c(LEGEND$OBSERVED,LEGEND$MODELS),"_&mu;")
    LEGEND$PvO.TITLE = paste0("Predicted vs. Observed Chart with Respect to <br>", td.Frmt(LEGEND$VARIABLE))
    LEGEND$LR.CURVES = paste0(LEGEND$MODELS,"_&Delta;")
    LEGEND$LR.TITLE = paste0("Discrepency Chart with Respect to <br>", td.Frmt(LEGEND$VARIABLE))
  }else{
    cat("MODE not supported, only 'LR' & 'PvO' are supported! \n")
    return(0)
  }
  
  LEGEND$EXPOSURE.REL = paste0(LEGEND$EXPOSURE," - Rel")
  LEGEND$TITLE =
    
    return(LEGEND)
}


dt.LoadFromFeather <- function(DATA,NAMES, SAMPLE = NULL){
  ## if a feather path is put in, load from feather
  library(TDTools)
  if(is.character(DATA)){
    DT.paths <- DATA
    DATA <- data.frame()
    for(i in DT.paths){
      DATA <- rbind(DATA,
                    read.feather(i,
                                 KEEP = as.vector(unlist(NAMES))))
    }
  }
  
  if(!is.null(SAMPLE)){
    DATA  <- DATA[sample(nrow(DATA),nrow(DATA)*SAMPLE),]
  }
  
  return(DATA)
}

dt.CleanData <- function(DATA,NAMES){
  
  uNAMES = as.vector(unlist(NAMES))
  if(!any(NAMES$VARIABLE %in% NAMES$MODEL)){
    uNAMES = uNAMES[!(uNAMES %in% NAMES$VARIABLE)]
  }
  
  for(i in uNAMES){
    NA.ind <- is.na(DATA[,i])
    if(any(NA.ind)){
      cat(paste0(sum(NA.ind)," (",percent(sum(NA.ind)/length(NA.ind)),") missing value(s) were removed from ",i,"\n" ))
      DATA = DATA[!NA.ind,]
    }
  }
  
  for(i in NAMES$MODELS){
    Zero.ind <- DATA[,i]==0
    if(any(Zero.ind)){
      cat(paste0(sum(Zero.ind)," (",percent(sum(Zero.ind)/length(Zero.ind)),") zero(s) were removed from ",i,"\n" ))
      DATA = DATA[!Zero.ind,]
    }
  }
  
  
  return(DATA)
}

dt.Rebase <- function(DATA, REBASE, NAMES){
  if(REBASE){
    if(!is.null(NAMES$SPLIT.BY)){
      Lvls <- unique(DATA[,NAMES$SPLIT.BY])
      for(j in Lvls){
        for(i in NAMES$MODELS){
          DATA[DATA[,NAMES$SPLIT.BY]==j,i] = DATA[DATA[,NAMES$SPLIT.BY]==j,i] *
            sum(DATA[DATA[,NAMES$SPLIT.BY]==j,NAMES$OBSERVED]) /
            sum(DATA[DATA[,NAMES$SPLIT.BY]==j,i]*DATA[DATA[,NAMES$SPLIT.BY]==j,NAMES$EXPOSURE])
        }
      }
    }else{
      for(i in NAMES$MODELS){
        DATA[,i] = DATA[,i] *  sum(DATA[,NAMES$OBSERVED]) /  sum(DATA[,i]*DATA[,NAMES$EXPOSURE])
      }
    }
    
  }
  
  
  
  return(DATA)
}

dt.Earn <- function(DATA,NAMES){
  DATA[,as.vector(unlist(NAMES))]
  
  ## Earn the fitted data
  DATA[,NAMES$MODELS] = DATA[,NAMES$MODELS]* DATA[,NAMES$EXPOSURE]
  
  return(DATA)
}


dt.Bucket <- function(DATA,NAMES,CUTS,N.BKTS, RETURN.CUTS = F){
  library(Hmisc,quietly = T)
  #Adding the NA values...checking if it can fix the problem 
  DATA[,NAMES$VARIABLE] = addNA(DATA[,NAMES$VARIABLE],ifany=T)
  DATA[,NAMES$VARIABLE] = as.numeric(as.character(DATA[,NAMES$VARIABLE]))
  
  
  if (is.numeric( DATA[,NAMES$VARIABLE] ) && !is.null(N.BKTS)){
    if(is.null(CUTS)){
      CUTS <- Hmisc::wtd.quantile(DATA[,NAMES$VARIABLE],
                                  weights = DATA[,NAMES$EXPOSURE],
                                  probs = seq(1/N.BKTS, 1 - 1/N.BKTS, by = 1/N.BKTS))
      CUTS <- unique(CUTS)
    }
    DATA[,NAMES$VARIABLE] <- as.factor( cut2(DATA[,NAMES$VARIABLE],  cuts=CUTS))
    DATA[,NAMES$VARIABLE] = addNA(DATA[,NAMES$VARIABLE],ifany=T)
  }else{
    DATA[,NAMES$VARIABLE] <- as.factor(DATA[,NAMES$VARIABLE])
    DATA[,NAMES$VARIABLE] = addNA(DATA[,NAMES$VARIABLE],ifany=T)
  }
  
  if(RETURN.CUTS){
    DATA = list(DATA=DATA,CUTS=CUTS)
  }
  
  return(DATA)
}

dt.Variance<-function(DATA,NAMES,LEGEND,SPLIT.VARIANCE){
  library(Hmisc)
  
  
  
  if(SPLIT.VARIANCE && !is.null(NAMES$SPLIT.BY)){
    #Adding NA incase there are any..
    DATA[,NAMES$VARIABLE] = as.factor(addNA(DATA[,NAMES$VARIABLE],ifany=T))
    Lvls <- as.factor(c(unique(DATA[,NAMES$SPLIT.BY]),"Combined"))
    Bkts <- as.factor(unique(DATA[,NAMES$VARIABLE]))
    
    VARIANCE <- data.frame( rep(Lvls,each=length(Bkts)), rep(Bkts, length(Lvls))  )
    colnames(VARIANCE) = c(LEGEND$SPLIT.BY,LEGEND$VARIABLE)
    
    var<-c()
    for(j in Lvls){
      for(i in Bkts){
        if(j == "Combined"){
          #gets the variance for NA values only
          #if(is.na(i)){
            #TMP <- DATA[is.na(DATA[,NAMES$VARIABLE]),]
          #}else{
            TMP <- DATA[DATA[,NAMES$VARIABLE] == i,]
        
        }else{
          #gets the variance for na values
          #if(is.na(i)){
            #TMP <- DATA[is.na(DATA[,NAMES$VARIABLE]) & DATA[,NAMES$SPLIT.BY] == j,]
          #}else{
            TMP <- DATA[DATA[,NAMES$VARIABLE] == i & DATA[,NAMES$SPLIT.BY] == j,]
          }
        #}
    #}
        
        ind <- TMP[,NAMES$CLM.COUNT] >0
        sev.s2 = var(TMP[,NAMES$OBSERVED][ind]/TMP[,NAMES$CLM.COUNT][ind])
        sev.mu = mean(TMP[,NAMES$OBSERVED][ind]/TMP[,NAMES$CLM.COUNT][ind])
        freq.mu = sum(TMP[,NAMES$CLM.COUNT])/sum(TMP[,NAMES$EXPOSURE])
        freq.s2 = freq.mu
        
        var = c(var,sev.s2*freq.s2 + sev.s2*freq.mu^2 + freq.s2*sev.mu^2)
      }
    }
    
  }else{
    DATA[,NAMES$VARIABLE] = as.factor(addNA(DATA[,NAMES$VARIABLE],ifany=T))
    VARIANCE <- data.frame((as.factor(unique(DATA[,NAMES$VARIABLE]))))
    colnames(VARIANCE) = LEGEND$VARIABLE
    
    var<-tmp<-c()
    for(i in VARIANCE[,LEGEND$VARIABLE]){
      #gets the variane for na values only
      #if(is.na(i)){
        #TMP <- DATA[is.na(DATA[,NAMES$VARIABLE]),]
        #TMP2<-DATA[is.na(DATA[,NAMES$VARIABLE]),]
         
      #}else{
      DATA[,NAMES$VARIABLE] = as.factor(addNA(DATA[,NAMES$VARIABLE],ifany=T))
      #Can't read na values....
      TMP <- DATA[DATA[,NAMES$VARIABLE] == i,]
    #}
      
      ind <- TMP[,NAMES$CLM.COUNT] >0
      sev.s2 = var(TMP[,NAMES$OBSERVED][ind]/TMP[,NAMES$CLM.COUNT][ind])
      sev.mu = mean(TMP[,NAMES$OBSERVED][ind]/TMP[,NAMES$CLM.COUNT][ind])
      freq.mu = sum(TMP[,NAMES$CLM.COUNT])/sum(TMP[,NAMES$EXPOSURE])
      freq.s2 = freq.mu
      
      var = c(var,sev.s2*freq.s2 + sev.s2*freq.mu^2 + freq.s2*sev.mu^2)
    }
  }
  
  VARIANCE<-cbind(VARIANCE, var)
  colnames(VARIANCE)[ncol(VARIANCE)] <- c("Variance")
  
  return(VARIANCE)
}


dt.Aggregate <-function(DATA, NAMES){
  if(is.null(NAMES$SPLIT.BY)){
    
    #DATA[,NAMES$VARIABLE] = as.factor(addNA(DATA[,NAMES$VARIABLE],ifany=T))
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
    
    #DATA[,NAMES$VARIABLE] = as.factor(addNA(DATA[,NAMES$VARIABLE],ifany=T))
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

dt.Adjust<-function(DATA, LEGEND, MODE, RELATIVE.XPO){
  
  ## Put the exposure in proportion instead of absolute
  if(!is.null(LEGEND$SPLIT.BY)){
    Lvls <- unique(DATA[DATA[,LEGEND$SPLIT.BY] != "Combined", LEGEND$SPLIT.BY])
    DATA$TEMP = NA
    names(DATA)[ncol(DATA)] = LEGEND$EXPOSURE.REL
    for(i in Lvls){
      DATA[DATA[,LEGEND$SPLIT.BY]==i, LEGEND$EXPOSURE.REL] = DATA[DATA[,LEGEND$SPLIT.BY]==i, LEGEND$EXPOSURE] / DATA[DATA[,LEGEND$SPLIT.BY]=="Combined", LEGEND$EXPOSURE]
      
    }
  }
  
  if(MODE == "LR" || MODE == "ALL"){
    DATA = DATA %>%
      cbind(., DATA[,LEGEND$OBSERVED] / DATA[,c(LEGEND$MODELS)])
    
    names(DATA)[(length(DATA)-length(LEGEND$MODELS) + 1):length(DATA)] = LEGEND$LR.CURVES
    
    if(length(LEGEND$LR.CURVES) > 1){
      DATA[,LEGEND$LR.CURVES] = apply(DATA[,LEGEND$LR.CURVES],2,function(x){x[x>5] = 5; return(x)})
    }else{
      DATA[DATA[,LEGEND$LR.CURVES]>5, LEGEND$LR.CURVES] = 5
    }
    
  }
  
  if(MODE == "PvO" || MODE=="ALL"){
    
    ##the order here is important... changing it can give false results for deviation plot
    DATA = DATA %>%
      cbind(., DATA[,c(LEGEND$OBSERVED,LEGEND$MODELS)]/DATA[,LEGEND$EXPOSURE])
    
    
    names(DATA)[(length(DATA)-length(LEGEND$MODELS)):length(DATA)] = LEGEND$PvO.CURVES
  }
  
  if(!(MODE %in% c("LR","PvO","ALL"))){
    cat("MODE not supported, only 'LR' & 'PvO' are supported! \n")
    return(0)
  }
  
  if(!is.null(LEGEND$SPLIT.BY)){
    if(length(unique(DATA[,LEGEND$SPLIT.BY])) > 10){
      cat("Too Many Levels In", LEGEND$SPLIT.BY,".\n")
      return(0)
    }else{
      DATA[,LEGEND$SPLIT.BY] = as.factor(DATA[,LEGEND$SPLIT.BY])
    }
  }
  
  return(DATA)
}

td.Frmt<-function(x){
  return(gsub("_"," ",x))
}



