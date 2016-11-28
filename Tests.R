


Plot.Data(DATA = c("/DATA/Raw_Data/Feather_Files/ONAU2015_ol_RSP.ftr"),
          VARIANCE.DATA = c("/DATA/Raw_Data/Feather_Files/ONAU2012_ol_RSP.ftr",
                            "/DATA/Raw_Data/Feather_Files/ONAU2013_ol_RSP.ftr",
                            "/DATA/Raw_Data/Feather_Files/ONAU2014_ol_RSP.ftr"),
          NAMES = list(MODELS =  "BI_PRED",
                       OBSERVED = "Clm_BI_Am",
                       CLM.COUNT = "Clm_BI_Nb",
                       EXPOSURE = "Xpo_Ern_BI_Nb",
                       VARIABLE = "Veh_Age_Nb",
                       SPLIT.BY = "Rat_DISML_In"),
          MODE = "ALL",
          EVAL.VARIANCE = T,
          SPLIT.VARIANCE = F,
          N.BKTS = 20)


Plot.Data(DATA = c("/DATA/Raw_Data/Feather_Files/ONAU2015_ol_RSP.ftr",
                   "/DATA/Raw_Data/Feather_Files/ONAU2012_ol_RSP.ftr",
                   "/DATA/Raw_Data/Feather_Files/ONAU2013_ol_RSP.ftr",
                   "/DATA/Raw_Data/Feather_Files/ONAU2014_ol_RSP.ftr"),
          VARIANCE.DATA = NULL,
          NAMES = list(MODELS =  "BI_PRED",
                       OBSERVED = "Clm_BI_Am",
                       CLM.COUNT = "Clm_BI_Nb",
                       EXPOSURE = "Xpo_Ern_BI_Nb",
                       VARIABLE = "Clt_Credit_Score_No",
                       SPLIT.BY = "Rat_DISML_In"),
          CUTS = NULL,
          MODE = "ALL",
          EVAL.VARIANCE = T,
          SPLIT.VARIANCE = F,
          N.BKTS = 20)















Plot.Data  <- function(DATA,
                       VARIANCE.DATA = NULL,
                       NAMES = list(MODELS   = NULL,
                                    OBSERVED = NULL,
                                    CLM.COUNT = NULL,
                                    VARIABLE = NULL,
                                    SPLIT.BY = NULL),
                       LEGEND = list(MODELS  = NULL,
                                     OBSERVED = NULL,
                                     EXPOSURE = NULL,
                                     VARIABLE = NULL,
                                     SPLIT.BY = NULL),
                       CUTS = NULL,
                       MODE = "ALL",
                       REBASE = T,
                       EVAL.VARIANCE = FALSE,
                       SPLIT.VARIANCE = FALSE,
                       N.BKTS = 20){
  
  
  library(dplyr,quietly = T)
  #Get the file source
  setwd("/TARIF/Pricing Innovation/2016/MCG-05-JPO/ONAU_RSP/Monitoring_Project/Source_Codes")
  source("Functions.R")
  
  DATA <- DATA %>%
    dt.LoadFromFeather(.,NAMES) %>%
    dt.CleanData(.,NAMES) %>%
    dt.Rebase(.,REBASE,NAMES)
  
  if(NAMES$VARIABLE %in% NAMES$MODELS){
    DATA$TEMP = DATA[,NAMES$VARIABLE]
    names(DATA)[ncol(DATA)] = paste0(NAMES$VARIABLE,"_Bkt")
    NAMES$VARIABLE = paste0(NAMES$VARIABLE,"_Bkt")
  }
  
  ##Fix names
  LEGEND <- LEGEND %>%
    dt.AdjNames(.,NAMES,MODE) %>%
    dt.AdjNames2(.,NAMES,MODE)
  
  DATA <- DATA %>%
    dt.Earn(.,NAMES) %>%
    dt.Bucket(., NAMES, CUTS, N.BKTS, RETURN.CUTS=T)
  
  CUTS<-DATA$CUTS
  DATA<-DATA$DATA
  
  if(EVAL.VARIANCE){
    if(!is.null(VARIANCE.DATA)){
      
      VARIANCE <- VARIANCE.DATA %>%
        dt.LoadFromFeather(., NAMES) %>%
        dt.CleanData(., NAMES) %>%
        dt.Rebase(., REBASE, NAMES) %>%
        dt.Earn(., NAMES) %>%
        dt.Bucket(., NAMES, CUTS, N.BKTS) %>%
        dt.Variance(., NAMES, LEGEND, SPLIT.VARIANCE)
      
    }else{
      
      VARIANCE <- DATA %>%
        dt.Variance(., NAMES, LEGEND, SPLIT.VARIANCE)
    }
    
  }
  
  DATA.AGG <- DATA %>%
    dt.Aggregate(., NAMES) %>%
    dt.FixNames(.,NAMES,LEGEND) %>%
    dt.Adjust(., LEGEND, MODE)
  
  if(EVAL.VARIANCE){
    DATA.AGG <- DATA.AGG %>%
      left_join(.,VARIANCE)
  }
  
  DATA.AGG[,LEGEND$VARIABLE] = addNA(DATA.AGG[,LEGEND$VARIABLE],ifany=T)
  if(!is.null(NAMES$SPLIT.BY)){
    DATA.AGG[,LEGEND$SPLIT.BY] = addNA(DATA.AGG[,LEGEND$SPLIT.BY],ifany=T)
  }
  
  
  return(list(DATA.AGG = DATA.AGG,
              LEGEND = LEGEND,
              PREPED = TRUE))
  
}



#Discrepency function 
Discrepency.Chart <- function(DATA,
                              NAMES = list(MODELS   = NULL,
                                           OBSERVED = NULL,
                                           VARIABLE = NULL,
                                           SPLIT.BY = NULL),
                              LEGEND = list(MODELS  = NULL,
                                            OBSERVED = NULL,
                                            EXPOSURE = NULL,
                                            VARIABLE = NULL,
                                            SPLIT.BY = NULL),
                              PATH = NULL,
                              CUTS = NULL,
                              REBASE = T,
                              PALETTE = "Dark2",
                              DATA.ONLY = FALSE,
                              N.BKTS = 20){
  
  library(RColorBrewer,quietly = T)
  library(plotly,quietly = T)
  #Functions source
  source("Functions.R")
  
  if(!is.list(DATA)){
    DATA<-Plot.Data(DATA = DATA,
                    NAMES = NAMES,
                    LEGEND = LEGEND,
                    CUTS = CUTS,
                    MODE = "LR",
                    REBASE = REBASE,
                    EVAL.VARIANCE = FALSE,
                    N.BKTS = N.BKTS)
  }else if(is.list(DATA) && is.null(DATA$PREPED)){
    cat("DATA is malformed! \n")
    return(0)
  }
  
  DATA.AGG = DATA$DATA.AGG
  LEGEND = DATA$LEGEND
  
  if(DATA.ONLY){
    return(DATA.AGG)
  }
  
  Cols <- brewer.pal(n = 8, name = PALETTE)
  Lvls <- unique(DATA.AGG[,LEGEND$SPLIT.BY])
  Cols1 <- rev(colorRampPalette(c("white",brewer.pal(8,PALETTE)[6]))(max(length(Lvls),length(LEGEND$CURVES))+2))
  for(i in 1:length(Lvls)){
    Cols1<-rbind(Cols1, c(rev(colorRampPalette(c("white",brewer.pal(8,PALETTE)[i]))(length(LEGEND$CURVES)+2)),
                          rep(0, max(length(Lvls),length(LEGEND$CURVES)) - length(LEGEND$CURVES)))  )
  }
  
  if(is.null(LEGEND$SPLIT.BY)){
    
    Max.Exp = max(DATA.AGG[,LEGEND$EXPOSURE])
    
    Final.Chart <- plot_ly() %>%
      add_bars(x = DATA.AGG[,LEGEND$VARIABLE] , y= DATA.AGG[,LEGEND$EXPOSURE], opacity = .6,name = td.Frmt(LEGEND$EXPOSURE), marker = list(color = Cols[6]) )
    k=1
    for(i in LEGEND$LR.CURVES){
      Final.Chart <- add_lines(Final.Chart, x=DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,i],  name = td.Frmt(i), yaxis="y2", line = list(color = Cols[k],
                                                                                                                                       width = 3) )
      
      k = k + 1
    }
  }else{
    
    Max.Exp = 0
    
    Final.Chart <- plot_ly()
    l=2
    for(j in Lvls){
      k=1
      
      DATA.AGG2 <- DATA.AGG[DATA.AGG[LEGEND$SPLIT.BY] == j,]
      
      if(j != "Combined"){
        Max.Exp = Max.Exp + max(DATA.AGG2[,LEGEND$EXPOSURE])
        Final.Chart <- add_bars(Final.Chart, x = DATA.AGG2[, LEGEND$VARIABLE] , y= DATA.AGG2[,LEGEND$EXPOSURE], opacity = .8,
                                name = paste0(j," - ",td.Frmt(LEGEND$EXPOSURE)), marker = list(color = Cols1[1,l]) )
        Final.Chart <- add_bars(Final.Chart, x = DATA.AGG2[, LEGEND$VARIABLE] , y= DATA.AGG2[,LEGEND$EXPOSURE.REL], opacity = .8,
                                name = paste0(j," - ",td.Frmt(LEGEND$EXPOSURE.REL)), marker = list(color = Cols1[1,l]),  visible = "legendonly" )
      }
      
      for(i in rev(LEGEND$LR.CURVES)){
        if(j != "Combined"){
          Final.Chart <- add_lines(Final.Chart, x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,i],yaxis="y2",
                                   name = paste0(j," - ",td.Frmt(i)), line = list(color = Cols1[l,k], width = 3), visible = "legendonly" )
        }else{
          Final.Chart <- add_lines(Final.Chart, x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,i],yaxis="y2",
                                   name = paste0(j," - ",td.Frmt(i)), line = list(color = Cols1[l,k], width = 3, dash="dashdot") )
        }
        
        k = k + 1
      }
      l = l + 1
    }
  }
  
  
  Final.Chart <-  layout(Final.Chart,margin = list(b=80),
                         barmode="stack", xaxis = list(title = gsub("_"," ",td.Frmt(LEGEND$VARIABLE)) ),
                         legend = list(x=1,y=1),
                         title = LEGEND$LR.TITLE,
                         yaxis = list(title = "Exposure",
                                      range = c(0,Max.Exp*2),
                                      side="right"),
                         yaxis2 = list(title = "Discrepency Ratio",
                                       side="left",
                                       overlaying = "y"))
  
  
  if(is.null(PATH)){
    return(Final.Chart)
  }else{
    htmlwidgets::saveWidget(Final.Chart,file = paste0(PATH,"/Discrepency_",NAMES$VARIABLE,".html"),selfcontained = T )
    cat(paste0("Discrepency Chart ",NAMES$VARIABLE," outputted to ",PATH," \n") )
  }
  
}