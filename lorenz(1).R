Plot.Lorenz<- function(DATA,
                       NAMES = list(MODELS   = NULL,
                                    OBSERVED = NULL,
                                    EXPOSURE = NULL
                                    VARIABLE = NULL),
                       LEGEND = list(MODELS   = NULL,
                                     OBSERVED = NULL,
                                     EXPOSURE = NULL
                                     VARIABLE = NULL),
                       PATH = NULL,
                       SAMPLE = NULL,
                       DATA.ONLY = FALSE,
                       PALETTE = "Dark2",
                       N.BKTS = 50){
  library(dplyr)
  library(plotly)
  library(RColorBrewer)
  
  LEGEND <- LEGEND %>%
    dt.AdjNames(.,NAMES,MODE)
  
  DATA <- DATA %>%
    dt.LoadFromFeather(.,NAMES,SAMPLE) %>%
    dt.CleanData(.,NAMES) %>%
    dt.Earn(.,NAMES) %>%
    dt.FixNames(.,NAMES,LEGEND)
  
  #Lorenz per variable
  DATA<-DATA%>%
    dt.Bucket(.,NAMES,NAMES, CUTS, N.BKTS, RETURN.CUTS=T)
    dt.Aggregate(.,NAMES)
  
  
  # Calculate Lorenz curve for each model
  LORENZ.DT <- data.frame(N=1:N.BKTS)
  GINI <- data.frame(Models = c(LEGEND$MODELS, LEGEND$OBSERVED), Gini_Index = 0)
  for(i in c(LEGEND$MODELS, LEGEND$OBSERVED)){
    #Sort by the predicted i
    TMP<-data.table::as.data.table(DATA)
    data.table::setorderv(TMP, i, 1)
    TMP<-as.data.frame(TMP)
    
    # Accumulate exposure and bucket in order to reduce the number of points to plot
    TMP$Cumulative_Xpo <- cumsum(TMP[,LEGEND$EXPOSURE]) / sum(TMP[,LEGEND$EXPOSURE])
    TMP$Cumulative_Loss <- cumsum(TMP[,LEGEND$OBSERVED]) / sum(TMP[,LEGEND$OBSERVED])
    
    n<-nrow(TMP)
    GINI[GINI$Models == i, "Gini_Index"] <- sum(TMP$Cumulative_Loss[-1] * TMP$Cumulative_Xpo[-n]) - sum(TMP$Cumulative_Loss[-n] * TMP$Cumulative_Xpo[-1])
    
    CUTS <- quantile(TMP$Cumulative_Xpo, probs = seq(1/N.BKTS, 1 - 1/N.BKTS, by = 1/N.BKTS))
    TMP$Cumulative_Xpo <- Hmisc::cut2(TMP$Cumulative_Xpo, cuts = CUTS, levels.mean = T)
    
    TMP<-TMP %>%
      group_by(Cumulative_Xpo) %>%
      summarize_each(funs(sum))
    
    TMP<-as.data.frame(TMP)
    TMP$cXpo <- as.numeric(as.character(TMP$Cumulative_Xpo))
    TMP$cLoss <- cumsum(TMP[,LEGEND$OBSERVED])/sum(TMP[,LEGEND$OBSERVED])
    
    names(TMP)[(ncol(TMP)-1):ncol(TMP)]=c(paste0("Cumul_Xpo_",i), paste0("Cumul_Losses_",i))
    
    LORENZ.DT <- data.frame(LORENZ.DT, TMP[,c(paste0("Cumul_Xpo_",i), paste0("Cumul_Losses_",i)) ])
  }
  
  if(DATA.ONLY){
    return(list(LORENZ = LORENZ.DT, GINI = GINI))
  }
  
  Cols <- brewer.pal(n = 8, name = PALETTE)
  Lorenz.Chart <- plot_ly()
  
  k=1
  loc<-seq(0.15,.85,length=length(LEGEND$MODELS))
  for(i in LEGEND$MODELS){
    Lorenz.Chart <- add_lines(Lorenz.Chart, x=LORENZ.DT[,paste0("Cumul_Xpo_",i)] , y=LORENZ.DT[,paste0("Cumul_Losses_",i)],
                              name = td.Frmt(i), line = list(color = Cols[k], width = 3) ) %>%
      add_annotations(x = .125,
                      y = .95-.05*k,
                      text = round(GINI[GINI$Models==i,"Gini_Index"],4),
                      showarrow = F,
                      xref = "x",
                      yref = "y",
                      font = list(color = Cols[k],
                                  size = 14))
    k=k+1
  }
  Lorenz.Chart <- add_lines(Lorenz.Chart, x=LORENZ.DT[,paste0("Cumul_Xpo_",LEGEND$OBSERVED)] , y=LORENZ.DT[,paste0("Cumul_Losses_",LEGEND$OBSERVED)],
                            name = td.Frmt(LEGEND$OBSERVED), line = list(color = "black", width = 3, dash = "dashdot") ) %>%
    add_lines( x=c(0.001,1) , y=c(0.001,1),
               name = "45 Degree Line", line = list(color = "grey", width = 1, dash = "dash") ) %>%
    add_annotations(x = .125,
                    y = .95-.05*k,
                    text = round(GINI[GINI$Models==LEGEND$OBSERVED,"Gini_Index"],4),
                    showarrow = F,
                    xref = "x",
                    yref = "y",
                    font = list(color = "black",
                                size = 14)) %>%
    add_annotations(x = .125,
                    y = .96,
                    text = "Gini Index",
                    showarrow = F,
                    xref = "x",
                    yref = "y",
                    font = list(color = "grey",
                                size = 14)) %>%
    layout(margin = list(b=80),
           xaxis = list(title = "Cumulative Exposure" ),
           legend = list(x=1,y=1),
           title = paste0("Lorenz Curves for ",paste0(td.Frmt(LEGEND$MODELS),collapse = " & ")),
           yaxis = list(title = "Cumulative Losses"))
  
  ## Simply Return Plot or save plot to PNG
  if(is.null(PATH)){
    return(Lorenz.Chart)
  }else{
    htmlwidgets::saveWidget(Lorenz.Chart,file = paste0(PATH,"/Lorenz_",NAMES$OBSERVED,".html"),selfcontained = T )
    cat(paste0("Lorenz Plot for ",NAMES$OBSERVED," outputted to ",PATH," \n") )
  }
  
}

