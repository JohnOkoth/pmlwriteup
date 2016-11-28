
#Reviewing the code for deviation chart
Deviation.Chart <- function(DATA,
                            NAMES = list(MODELS   = NULL,
                                         OBSERVED = NULL,
                                         CLM.COUNT = NULL,
                                         EXPOSURE = NULL,
                                         VARIABLE = NULL,
                                         SPLIT.BY = NULL),
                            LEGEND = list(MODELS  = NULL,
                                          OBSERVED = NULL,
                                          EXPOSURE = NULL,
                                          VARIABLE = NULL,
                                          SPLIT.BY = NULL),
                            PATH = NULL,
                            CUTS = NULL,
                            PALETTE = "Dark2",
                            DATA.ONLY = FALSE,
                            N.BKTS = 20){
  
  ############################
  ## 00 - Data Retrieval
  ############################
  
  ## Get the table from Plot.Chart function
  if(!is.list(DATA)){
    DATA<-Plot.Data(DATA = DATA,
                    NAMES = NAMES,
                    LEGEND = LEGEND,
                    CUTS = CUTS,
                    MODE = "LR",
                    REBASE = T,
                    EVAL.VARIANCE = TRUE,
                    SPLIT.VARIANCE = FALSE,
                    N.BKTS = N.BKTS)
  }else if(is.list(DATA) && is.null(DATA$PREPED)){
    cat("DATA is malformed! \n")
    return(0)
  }
  
  LEGEND = DATA$LEGEND
  #DATA.AGG is not added from the data to confirm where it originates from
  DATA.AGG = DATA$DATA.AGG
  
  ############################
  ## 05 - Data Preparation
  ############################
  
  nLvls<-1
  if(!is.null(LEGEND$SPLIT.BY)){
    Lvls<-unique(DATA.AGG[,LEGEND$SPLIT.BY])
    nLvls<-length(Lvls)
  }
  
  DATA.PL <- data.frame(N=1:(nrow(DATA.AGG)/nLvls))
  
  if(nLvls>1){
    for(j in Lvls){
      for(i in LEGEND$LR.CURVES){
        TMP <- DATA.AGG[DATA.AGG[,LEGEND$SPLIT.BY]==j,]
        #LR.CURVES come into play
        TMP$Error = abs(TMP[,i] - 1 )
        TMP<-TMP[rev(order(TMP$Error)),]
        TMP$Cumul_Xpo = cumsum(TMP[,LEGEND$EXPOSURE])/sum(TMP[,LEGEND$EXPOSURE])
        
        names(TMP)[(ncol(TMP)-1):ncol(TMP)] = c(paste0(j,"_Error_",i),paste0(j,"_Cumul_Xpo_",i))
        
        DATA.PL = data.frame(DATA.PL, TMP[,c(paste0(j,"_Error_",i),paste0(j,"_Cumul_Xpo_",i))],check.names = F)
        
      }
    }
  }else{
    for(i in LEGEND$LR.CURVES){
      TMP <- DATA.AGG
      TMP$Error = abs(TMP[,i] - 1 )
      TMP<-TMP[rev(order(TMP$Error)),]
      TMP$Cumul_Xpo = cumsum(TMP[,LEGEND$EXPOSURE])/sum(TMP[,LEGEND$EXPOSURE])
      
      names(TMP)[(ncol(TMP)-1):ncol(TMP)] = c(paste0("Error_",i),paste0("Cumul_Xpo_",i))
      
      DATA.PL = data.frame(DATA.PL, TMP[,c(paste0("Error_",i),paste0("Cumul_Xpo_",i))],row.names = NULL)
    }
  }
  
  if(!is.null(LEGEND$SPLIT.BY)){
    TMP <- DATA.AGG[DATA.AGG[,LEGEND$SPLIT.BY]=="Combined",]
  }else{
    TMP <- DATA.AGG
  }
  
  for(i in 2:3){
    TMP$Error = i * sqrt( (TMP$Variance - min(TMP$Variance,na.rm=T)) / TMP[,LEGEND$EXPOSURE] ) / TMP[,LEGEND$PvO.CURVES[1]]  ## divide the sd by the mean of the data in that bucket
    TMP<-TMP[rev(order(TMP$Error)),]
    TMP$Cumul_Xpo = cumsum(TMP[,LEGEND$EXPOSURE])/sum(TMP[,LEGEND$EXPOSURE])
    
    names(TMP)[(ncol(TMP)-1):ncol(TMP)] = c(paste0("Error_",i,"SD"),paste0("Cumul_Xpo_",i,"SD"))
    
    DATA.PL = data.frame(DATA.PL, TMP[,c(paste0("Error_",i,"SD"),paste0("Cumul_Xpo_",i,"SD"))],check.names = F)
    
  }
  
  if(DATA.ONLY){
    return(DATA.PL)
  }
  
  
  ############################
  ## 10 - Plotting
  ############################
  Cols <- brewer.pal(n = 8, name = PALETTE)
  
  DATA.THRESHOLD <- DATA.PL[!is.na(DATA.PL$Error_2SD),]
  
  Deviation.Chart <- plot_ly()
  Deviation.Chart <- add_lines(Deviation.Chart, x = c(0,1), y = c(1,1),
                               name = "Threshold", line = list(color = Cols[4], width = 0 ), showlegend=F ) %>%
    add_lines(x = DATA.THRESHOLD$Cumul_Xpo_3SD, y = DATA.THRESHOLD$Error_3SD, fill = 'tonexty',
              name = paste0("3&sigma; Threshold"), line = list(color = Cols[4], width = 0, shape = "spline" ) ) %>%
    add_lines(x = DATA.THRESHOLD$Cumul_Xpo_2SD, y = DATA.THRESHOLD$Error_2SD, fill = 'tonexty',
              name = paste0("2&sigma; Threshold"), line = list(color = Cols[6], width = 0, shape = "spline" ) )
  
  if(nLvls>1){
    
    Cols1<-c()
    for(i in 1:nLvls){
      Cols1<-rbind(Cols1, rev(colorRampPalette(c("white",brewer.pal(8,PALETTE)[i]))(length(LEGEND$LR.CURVES)+2))  )
    }
    
    l=1
    for(j in rev(Lvls)){
      k=1
      for(i in LEGEND$LR.CURVES){
        Deviation.Chart <- add_lines(Deviation.Chart, x=DATA.PL[,paste0(j,"_Cumul_Xpo_",i)] , y=DATA.PL[,paste0(j,"_Error_",i)],
                                     visible = ifelse(j=="Combined",TRUE,"legendonly"),
                                     name = paste0(j,"_",td.Frmt(i)), line = list(color = Cols1[l,k], width = 3, shape = "vh") )
        k=k+1
      }
      l=l+1
    }
    
    LEGEND$TITLE = paste0("Deviation Curves with Respect to ",td.Frmt(LEGEND$VARIABLE),
                          " for ",paste0(td.Frmt(LEGEND$MODELS),collapse = " & ")," Splitted by ", td.Frmt(LEGEND$SPLIT.BY))
    
  }else{
    
    k=1
    for(i in LEGEND$LR.CURVES){
      Deviation.Chart <- add_lines(Deviation.Chart, x=DATA.PL[,paste0("Cumul_Xpo_",i)] , y=DATA.PL[,paste0("Error_",i)],
                                   name = td.Frmt(i), line = list(color = Cols[k], width = 3, shape = "vh") )
      k=k+1
    }
    
    LEGEND$TITLE = paste0("Deviation Curves with Respect to <br>",td.Frmt(LEGEND$VARIABLE),
                          " for ",paste0(td.Frmt(LEGEND$MODELS),collapse = " & "))
  }
  
  Deviation.Chart <- layout(Deviation.Chart, margin = list(b=80),
                            xaxis = list(title = "Cumulative Exposure" ),
                            legend = list(x=1,y=1),
                            title = LEGEND$TITLE,
                            yaxis = list(title = "% Deviation",
                                         range = c(0,.5)))
  
  return(Deviation.Chart)
  
}
