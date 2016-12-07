#' Lorenz Curve Plot
#'
#' Outputs the Lorenz Curve, it does some binning to make the code faster
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
#' @param NAMES \itemize{
#' \item{MODELS}{Vector of names of the columns with the model predictions}
#' \item{OBSERVED}{Column name of the observed variable}
#' \item{EXPOSURE}{Column name of the exposure variable}
#' }
#' @param LEGEND \itemize{
#' \item{MODELS}{ Vector of names in the legend for each model (Same order as in NAMES)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{OBSERVED}{ Name in the legend for the observed variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{EXPOSURE}{ Name in the legend for the exposure variable(Leave \code{NULL} to keep the names in NAMES)}
#' }
#' @param PATH Path to which the graph will be exported to. (Default \code{NULL} will display the graph instead of exporting)
#' @param SAMPLE If the data is too large you may set what proportion of the data you want it to use. (E.g. .5 will use half the data)
#' \code{NULL} will not use a sample.
#' @param DATA.ONLY TRUE will simply return a table instead of the plot
#' @param PALETTE ColorBrewer Palette, Default is \code{'Dark2'}
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#' @param AS.PDF Gives path to a temporary PDF. (Use this for PDF reports)
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
Plot.Lorenz<- function(DATA,
                       NAMES = list(MODELS   = NULL,
                                    OBSERVED = NULL,
                                    EXPOSURE = NULL),
                       LEGEND = list(MODELS   = NULL,
                                     OBSERVED = NULL,
                                     EXPOSURE = NULL),
                       PATH = NULL,
                       SAMPLE = NULL,
                       DATA.ONLY = FALSE,
                       PALETTE = "Dark2",
                       N.BKTS = 100,
                       AS.PDF = F){
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

    CUTS <- quantile(TMP$Cumulative_Xpo, probs = seq(1/N.BKTS, 1 - 1/N.BKTS, by = 1/N.BKTS)) %>%
      unique(.)
    TMP$Cumulative_Xpo <- Hmisc::cut2(TMP$Cumulative_Xpo, cuts = CUTS, levels.mean = T)

    TMP<-TMP %>%
      group_by(Cumulative_Xpo) %>%
      summarize_each(funs(sum))

    TMP<-as.data.frame(TMP)
    TMP$cXpo <- as.numeric(as.character(TMP$Cumulative_Xpo))
    TMP$cLoss <- cumsum(TMP[,LEGEND$OBSERVED])/sum(TMP[,LEGEND$OBSERVED])

    names(TMP)[(ncol(TMP)-1):ncol(TMP)]=c(paste0("Cumul_Xpo_",i), paste0("Cumul_Losses_",i))

    if(nrow(TMP)<nrow(LORENZ.DT)){

      x<-matrix(NA, ncol = ncol(TMP),nrow = (nrow(LORENZ.DT) - nrow(TMP)))
      x<-as.data.frame(x)
      colnames(x) <- colnames(TMP)

      TMP<-rbind(TMP,x)
    }

    LORENZ.DT <- data.frame(LORENZ.DT, TMP[,c(paste0("Cumul_Xpo_",i), paste0("Cumul_Losses_",i)) ])
  }

  if(DATA.ONLY){
    return(list(LORENZ = LORENZ.DT, GINI = GINI))
  }

  Cols <- RColorBrewer::brewer.pal(n = 8, name = PALETTE)
  Lorenz.Chart <- plot_ly(width = 800, height = 500)

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

  ## Simply Return Plot or save plot to PDF
  if(AS.PDF){
    library(webshot, quietly = T)
    library(png)
    tmpFile <- tempfile(fileext = ".pdf")
    export(Lorenz.Chart, file = tmpFile, vwidth = 800, vheight = 500)
    return(tmpFile)
  }else if(is.null(PATH)){
    return(Lorenz.Chart)
  }else{
    htmlwidgets::saveWidget(Lorenz.Chart,file = paste0(PATH,"/Lorenz_",NAMES$OBSERVED,".html"),selfcontained = T )
    cat(paste0("Lorenz Plot for ",NAMES$OBSERVED," outputted to ",PATH," \n") )
  }

}



#' Prepares Data For Charts
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
#' @param VARIANCE.DATA  Dataframe from which you the variance will be calculated from. (If NULL, same data as plotting data will be used)
#' @param NAMES \itemize{
#' \item{MODELS}{ Vector of names of the columns with the model predictions (non-earned)}
#' \item{OBSERVED}{ Column name of the observed variable}
#' \item{CLM.COUNT}{ Column name of the claim counts, used for theshold evaluation for Pure Premium}
#' \item{EXPOSURE}{ Column name of the exposure variable}
#' \item{VARIABLE}{ Column name of the variable with respect to which you want the LR proof (i.e. the x-axis)}
#' \item{SPLIT.BY}{ Column name of the factor variable by which you want to split the LR proofs by}
#' }
#' @param LEGEND \itemize{
#' \item{MODELS}{ Vector of names in the legend for each model (Same order as in NAMES)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{OBSERVED}{ Name in the legend for the observed variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{EXPOSURE}{ Name in the legend for the exposure variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{VARIABLE}{ Name in the legend for the variable with respect to which you want the LR proof (i.e. the x-axis)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{SPLIT.BY}{ Name in the legend for the factor variable by which you want to split the LR proofs by(Leave \code{NULL} to keep the names in NAMES)}
#' }
#' @param CUTS The cut points for the variable if the user wants to provide them. Leave \code{NULL} if you want auto bucket.
#' @param MODE Can choose between \code{"LR"} to produce discrepancy (LR Proof) or \code{"PvO"} to produce a predicted vs observed data or "ALL" to output both.
#' @param REBASE Set to TRUE if you want your predicted to be rebased to observed levels. (Defaults to TRUE)
#' @param EVAL.VARIANCE TRUE will evaluate the variance for the threshold evaluation.
#' @param SPLIT.VARIANCE TRUE will evaluate the split the Variance estimation by the SPLIT.BY variable. (Leave to FALSE unless you know what you are doing)
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#'
#' @return Aggregated data with requested columns
#'
#' @export
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

  cat("Preparing Data \n")
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

      cat("Preparing Variance Data \n")

      VARIANCE.DATA <- VARIANCE.DATA %>%
        dt.LoadFromFeather(., NAMES) %>%
        dt.CleanData(., NAMES) %>%
        dt.Rebase(., REBASE, NAMES)

      if(gsub("_Bkt","",NAMES$VARIABLE) %in% NAMES$MODELS){
        VARIANCE.DATA$TEMP = VARIANCE.DATA[,gsub("_Bkt","",NAMES$VARIABLE)]
        names(VARIANCE.DATA)[ncol(VARIANCE.DATA)] = NAMES$VARIABLE
      }

      VARIANCE <- VARIANCE.DATA %>%
        dt.Earn(., NAMES) %>%
        dt.Bucket(., NAMES, CUTS, N.BKTS) %>%
        dt.Variance(., NAMES, LEGEND, SPLIT.VARIANCE)

      VARIANCE <- VARIANCE[order(VARIANCE[,NAMES$VARIABLE]),]

    }else{

      VARIANCE <- DATA %>%
        dt.Variance(., NAMES, LEGEND, SPLIT.VARIANCE)

      VARIANCE <- VARIANCE[order(VARIANCE[,NAMES$VARIABLE]),]
    }

  }

  DATA.AGG <- DATA %>%
    dt.Aggregate(., NAMES) %>%
    dt.FixNames(.,NAMES,LEGEND) %>%
    dt.Adjust(., LEGEND, MODE)

  if(EVAL.VARIANCE){
    DATA.AGG <- DATA.AGG %>%
      data.frame(.,Variance = VARIANCE[,-1],check.names = F)
  }

  DATA.AGG[,LEGEND$VARIABLE] = addNA(DATA.AGG[,LEGEND$VARIABLE],ifany=T)
  if(!is.null(NAMES$SPLIT.BY)){
    DATA.AGG[,LEGEND$SPLIT.BY] = addNA(DATA.AGG[,LEGEND$SPLIT.BY],ifany=T)
  }


  return(list(DATA.AGG = DATA.AGG,
              LEGEND = LEGEND,
              PREPED = TRUE))

}



#' Discrepancy Chart
#'
#' Outputs the average deviation from the observed with respect to a variable grouping
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
#' Can also be a data prepared by Plot.Data function
#' @param NAMES \itemize{
#' \item{MODELS}{ Vector of names of the columns with the model predictions (non-earned)}
#' \item{OBSERVED}{ Column name of the observed variable}
#' \item{EXPOSURE}{ Column name of the exposure variable}
#' \item{VARIABLE}{ Column name of the variable with respect to which you want the LR proof (i.e. the x-axis)}
#' \item{SPLIT.BY}{ Column name of the factor variable by which you want to split the LR proofs by}
#' }
#' @param LEGEND \itemize{
#' \item{MODELS}{ Vector of names in the legend for each model (Same order as in NAMES)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{OBSERVED}{ Name in the legend for the observed variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{EXPOSURE}{ Name in the legend for the exposure variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{VARIABLE}{ Name in the legend for the variable with respect to which you want the LR proof (i.e. the x-axis)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{SPLIT.BY}{ Name in the legend for the factor variable by which you want to split the LR proofs by(Leave \code{NULL} to keep the names in NAMES)}
#' }
#' @param PATH Path to which the graph will be exported to. (Default \code{NULL} will display the graph instead of exporting)
#' @param CUTS The cut points for the variable if the user wants to provide them. Leave \code{NULL} if you want auto bucket.
#' @param MODE Can choose between \code{"LR"} to produce double lift chart (LR Proof) or \code{"PvO"} to produce a predicted vs observed plot.
#' @param REBASE Set to TRUE if you want your predicted to be rebased to observed levels. (Defaults to TRUE)
#' @param PALETTE ColorBrewer Palette, Default is \code{'Dark2'}
#' @param DATA.ONLY TRUE will simply return a table instead of the plot
#' @param EVAL.VARIANCE TRUE will evaluate the Threshold. (Leave to FALSE unless you know what you are doing)
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#' @param AS.PDF Gives path to a temporary PDF. (Use this for PDF reports)
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
Discrepancy.Chart <- function(DATA,
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
                              N.BKTS = 20,
                              AS.PDF = F){

  library(RColorBrewer,quietly = T)
  library(plotly,quietly = T)

  if(class(DATA) == "data.frame"){
    DATA<-Plot.Data(DATA = DATA,
                    NAMES = NAMES,
                    LEGEND = LEGEND,
                    CUTS = CUTS,
                    MODE = "LR",
                    REBASE = REBASE,
                    EVAL.VARIANCE = FALSE,
                    N.BKTS = N.BKTS)
  }else if(class(DATA) == "list" && is.null(DATA$PREPED)){
    cat("DATA is malformed! \n")
    return(0)
  }

  DATA.AGG = DATA$DATA.AGG
  LEGEND = DATA$LEGEND

  if(DATA.ONLY){
    return(DATA.AGG)
  }

  Cols <- RColorBrewer::brewer.pal(n = 8, name = PALETTE)
  Lvls <- unique(DATA.AGG[,LEGEND$SPLIT.BY])
  Cols1 <- rev(colorRampPalette(c("white",RColorBrewer::brewer.pal(8,PALETTE)[6]))(max(length(Lvls),length(LEGEND$CURVES))+2))
  for(i in 1:length(Lvls)){
    Cols1<-rbind(Cols1, c(rev(colorRampPalette(c("white",RColorBrewer::brewer.pal(8,PALETTE)[i]))(length(LEGEND$CURVES)+2)),
                          rep(0, max(length(Lvls),length(LEGEND$CURVES)) - length(LEGEND$CURVES)))  )
  }

  if(is.null(LEGEND$SPLIT.BY)){

    Max.Exp = max(DATA.AGG[,LEGEND$EXPOSURE])

    Final.Chart <- plot_ly(width = 800, height = 500) %>%
      add_bars(x = DATA.AGG[,LEGEND$VARIABLE] , y= DATA.AGG[,LEGEND$EXPOSURE], opacity = .6,name = td.Frmt(LEGEND$EXPOSURE), marker = list(color = Cols[6]) )
    k=1
    for(i in LEGEND$LR.CURVES){
      Final.Chart <- add_lines(Final.Chart, x=DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,i],  name = td.Frmt(i), yaxis="y2", line = list(color = Cols[k],
                                                                                                                                       width = 3) )

      k = k + 1
    }
  }else{

    Max.Exp = 0

    Final.Chart <- plot_ly(width = 800, height = 500)
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


  Final.Chart <-  layout(Final.Chart,margin = list(b=120),
                        barmode="stack", xaxis = list(title = gsub("_"," ",td.Frmt(LEGEND$VARIABLE)) ),
                        legend = list(x=1,y=1),
                        title = LEGEND$LR.TITLE,
                        yaxis = list(title = "Exposure",
                                     range = c(0,Max.Exp*2),
                                     side="right"),
                        yaxis2 = list(title = "Discrepancy Ratio",
                                      side="left",
                                      overlaying = "y"))

  if(AS.PDF){
    library(webshot, quietly = T)
    library(png)
    tmpFile <- tempfile(fileext = ".pdf")
    export(Final.Chart, file = tmpFile, vwidth = 800, vheight = 500)
    return(tmpFile)
  }else if(is.null(PATH)){
    return(Final.Chart)
  }else{
    htmlwidgets::saveWidget(Final.Chart,file = paste0(PATH,"/Discrepancy_",NAMES$VARIABLE,".html"),selfcontained = T )
    cat(paste0("Discrepancy Chart ",NAMES$VARIABLE," outputted to ",PATH," \n") )
  }

}


#' Predicted vs. Observed Chart
#'
#' Outputs average Predicted vs. average Observed with respect to a grouped variable
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
#' Can also be a data prepared by Plot.Data function
#' @param NAMES \itemize{
#' \item{MODELS}{ Vector of names of the columns with the model predictions (non-earned)}
#' \item{OBSERVED}{ Column name of the observed variable}
#' \item{EXPOSURE}{ Column name of the exposure variable}
#' \item{VARIABLE}{ Column name of the variable with respect to which you want the averages (i.e. the x-axis)}
#' \item{SPLIT.BY}{ Column name of the factor variable by which you want to split the averages by}
#' }
#' @param LEGEND \itemize{
#' \item{MODELS}{ Vector of names in the legend for each model (Same order as in NAMES)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{OBSERVED}{ Name in the legend for the observed variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{EXPOSURE}{ Name in the legend for the exposure variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{VARIABLE}{ Name in the legend for the variable with respect to which you want the LR proof (i.e. the x-axis)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{SPLIT.BY}{ Name in the legend for the factor variable by which you want to split the averages by(Leave \code{NULL} to keep the names in NAMES)}
#' }
#' @param PATH Path to which the graph will be exported to. (Default \code{NULL} will display the graph instead of exporting)
#' @param CUTS The cut points for the variable if the user wants to provide them. Leave \code{NULL} if you want auto bucket.
#' @param REBASE Set to TRUE if you want your predicted to be rebased to observed levels. (Defaults to TRUE)
#' @param PALETTE ColorBrewer Palette, Default is \code{'Dark2'}
#' @param DATA.ONLY TRUE will simply return a table instead of the plot
#' @param EVAL.VARIANCE TRUE will evaluate the Threshold. (Leave to FALSE unless you know what you are doing)
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#' @param AS.PDF Gives path to a temporary PDF. (Use this for PDF reports)
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
PvO.Chart <- function(DATA,
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
                      N.BKTS = 20,
                      AS.PDF = F){

  library(RColorBrewer,quietly = T)
  library(plotly,quietly = T)

  if(class(DATA) == "data.frame"){
    DATA<-Plot.Data(DATA = DATA,
                    NAMES = NAMES,
                    LEGEND = LEGEND,
                    CUTS = CUTS,
                    MODE = "PvO",
                    REBASE = REBASE,
                    EVAL.VARIANCE = FALSE,
                    N.BKTS = N.BKTS)
  }else if(class(DATA) == "list" && is.null(DATA$PREPED)){
    cat("DATA is malformed! \n")
    return(0)
  }

  DATA.AGG = DATA$DATA.AGG
  LEGEND = DATA$LEGEND

  if(DATA.ONLY){
    return(DATA.AGG)
  }

  Cols <- RColorBrewer::brewer.pal(n = 8, name = PALETTE)
  Lvls <- unique(DATA.AGG[,LEGEND$SPLIT.BY])
  Cols1 <- rev(colorRampPalette(c("white",RColorBrewer::brewer.pal(8,PALETTE)[6]))(max(length(Lvls),length(LEGEND$PvO.CURVES))+2))
  for(i in 1:length(Lvls)){
    Cols1<-rbind(Cols1, c(rev(colorRampPalette(c("white",RColorBrewer::brewer.pal(8,PALETTE)[i]))(length(LEGEND$PvO.CURVES)+2)),
                          rep(0, max(length(Lvls),length(LEGEND$PvO.CURVES)) - length(LEGEND$PvO.CURVES)))  )
  }



  Final.Chart <- plot_ly(width = 800, height = 500)

  if(is.null(LEGEND$SPLIT.BY)){

    Max.Exp = max(DATA.AGG[,LEGEND$EXPOSURE])

    Final.Chart <- add_bars(Final.Chart, x =DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,LEGEND$EXPOSURE], opacity = .6,
                            name = td.Frmt(LEGEND$EXPOSURE), marker = list(color = Cols[6]) )
    k=2
    for(i in LEGEND$PvO.CURVES[-1]){
      Final.Chart <- add_lines(Final.Chart, x=DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,i],yaxis="y2",
                               name = td.Frmt(i), line = list(color = Cols[k],
                                                              width = 3))
      k = k + 1
    }
    Final.Chart <- add_lines(Final.Chart,x=DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,LEGEND$PvO.CURVES[1]],yaxis="y2",
                             name = td.Frmt(LEGEND$PvO.CURVES[1]), line = list(color = Cols[1],
                                                                               width = 3,
                                                                               dash = "dashdot") )

  }else{


    Max.Exp = 0

    Final.Chart <- plot_ly(width = 800, height = 500)
    l=2
    for(j in Lvls){

      k=1
      DATA.AGG2 <- DATA.AGG[DATA.AGG[LEGEND$SPLIT.BY] == j,]
      if(j != "Combined"){
        Max.Exp = Max.Exp + max(DATA.AGG2[,LEGEND$EXPOSURE])
        Final.Chart <- add_bars(Final.Chart, x = DATA.AGG2[, LEGEND$VARIABLE] , y= DATA.AGG2[,LEGEND$EXPOSURE], opacity = .8,
                                name = paste0(j," - ",td.Frmt(LEGEND$EXPOSURE)), marker = list(color = Cols1[1,l]) )
        Final.Chart <- add_bars(Final.Chart, x = DATA.AGG2[, LEGEND$VARIABLE] , y= DATA.AGG2[,LEGEND$EXPOSURE.REL], opacity = .8,
                                name = paste0(j," - ",td.Frmt(LEGEND$EXPOSURE.REL)), marker = list(color = Cols1[1,l]), visible = "legendonly" )
      }

      for(i in LEGEND$PvO.CURVES[-1]){
        Final.Chart <- add_lines(Final.Chart, x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,i],yaxis="y2",
                                 visible = ifelse(j=="Combined",TRUE,"legendonly"),
                                 name = paste0(j," - ",td.Frmt(i)), line = list(color = Cols1[l,k],
                                                                                width = 3))
        k = k + 1
      }
      Final.Chart <- add_lines(Final.Chart,x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,LEGEND$PvO.CURVES[1]],yaxis="y2",
                               visible = ifelse(j=="Combined",TRUE,"legendonly"),
                               name = paste0(j," - ",td.Frmt(LEGEND$PvO.CURVES[1])), line = list(color = Cols1[l,k],
                                                                                                 width = 3,
                                                                                                 dash = "dashdot") )
      l = l + 1
    }

  }

  Final.Chart <-layout(Final.Chart, margin = list(b=120),
                       barmode="stack", xaxis = list(title = gsub("_"," ",td.Frmt(LEGEND$VARIABLE)) ),
                       legend = list(x=1,y=1),
                       title = LEGEND$PvO.TITLE,
                       autosize=F,
                       yaxis = list(title = "Exposure",
                                    range = c(0,Max.Exp*2),
                                    side="right"),
                       yaxis2 = list( title = "Mean",
                                      side="left",
                                      overlaying="y"))



  if(AS.PDF){
    library(webshot, quietly = T)
    library(png)
    tmpFile <- tempfile(fileext = ".pdf")
    export(Final.Chart, file = tmpFile, vwidth = 800, vheight = 500)
    return(tmpFile)
  }else if(is.null(PATH)){
    return(Final.Chart)
  }else{
    htmlwidgets::saveWidget(Final.Chart,file = paste0(PATH,"/PvO_",NAMES$VARIABLE,".html"),selfcontained = T )
    cat(paste0("PvO Chart ",NAMES$VARIABLE," outputted to ",PATH," \n") )
  }



}


#' Deviation Charts
#'
#' Outputs a chart of % errors distribution with respect to a variable. Each point on the line represents Pr(% Error >= y) = x.
#' Note that these graphs are heavily influenced by the bucketting of the variable. Not that splitted threshold is not supported at the moment.
#' The given threshold is only for the combined curve.
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
#' @param NAMES \itemize{
#' \item{MODELS}{ Vector of names of the columns with the model predictions (non-earned)}
#' \item{CLM.COUNT}{ Column name of the claim counts, used for theshold evaluation for Pure Premium}
#' \item{OBSERVED}{ Column name of the observed variable}
#' \item{EXPOSURE}{ Column name of the exposure variable}
#' \item{VARIABLE}{ Column name of the variable with respect to which you want the LR proof (i.e. the x-axis)}
#' \item{SPLIT.BY}{ Column name of the factor variable by which you want to split the LR proofs by}
#' }
#' @param LEGEND \itemize{
#' \item{MODELS}{ Vector of names in the legend for each model (Same order as in NAMES)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{OBSERVED}{ Name in the legend for the observed variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{EXPOSURE}{ Name in the legend for the exposure variable(Leave \code{NULL} to keep the names in NAMES)}
#' \item{VARIABLE}{ Name in the legend for the variable with respect to which you want the LR proof (i.e. the x-axis)(Leave \code{NULL} to keep the names in NAMES)}
#' \item{SPLIT.BY}{ Name in the legend for the factor variable by which you want to split the LR proofs by(Leave \code{NULL} to keep the names in NAMES)}
#' }
#' @param PATH Path to which the graph will be exported to. (Default \code{NULL} will display the graph instead of exporting)
#' @param CUTS The cut points for the variable if the user wants to provide them. Leave \code{NULL} if you want auto bucket.
#' @param PALETTE ColorBrewer Palette, Default is \code{'Dark2'}
#' @param DATA.ONLY TRUE will simply return a table instead of the plot
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#' @param AS.PDF Gives path to a temporary PDF. (Use this for PDF reports)
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
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
                            N.BKTS = 20,
                            AS.PDF = F){

  library(plotly,quietly = T)

  ############################
  ## 00 - Data Retrieval
  ############################

  ## Get the table from Plot.Chart function
  if(class(DATA) == "data.frame"){
    DATA<-Plot.Data(DATA = DATA,
                    NAMES = NAMES,
                    LEGEND = LEGEND,
                    CUTS = CUTS,
                    MODE = "LR",
                    REBASE = T,
                    EVAL.VARIANCE = TRUE,
                    SPLIT.VARIANCE = FALSE,
                    N.BKTS = N.BKTS)
  }else if(class(DATA) == "list" && is.null(DATA$PREPED)){
    cat("DATA is malformed! \n")
    return(0)
  }

  LEGEND = DATA$LEGEND
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

      DATA.PL = data.frame(DATA.PL, TMP[,c(paste0("Error_",i),paste0("Cumul_Xpo_",i))], row.names = NULL, check.names = F)
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
  Cols <- RColorBrewer::brewer.pal(n = 8, name = PALETTE)

  DATA.THRESHOLD <- DATA.PL[!is.na(DATA.PL$Error_2SD),]

  Deviation.Chart <- plot_ly(width = 800, height = 500)
  Deviation.Chart <- add_lines(Deviation.Chart, x = c(0,1), y = rep(max(1,DATA.THRESHOLD$Error_3SD),2),
                               name = "Threshold", line = list(color = Cols[4], width = 0 ), showlegend=F ) %>%
    add_lines(x = DATA.THRESHOLD$Cumul_Xpo_3SD, y = DATA.THRESHOLD$Error_3SD, fill = 'tonexty',
              name = paste0("3&sigma; Threshold"), line = list(color = Cols[4], width = 0, shape = "spline" ) ) %>%
    add_lines(x = DATA.THRESHOLD$Cumul_Xpo_2SD, y = DATA.THRESHOLD$Error_2SD, fill = 'tonexty',
              name = paste0("2&sigma; Threshold"), line = list(color = Cols[6], width = 0, shape = "spline" ) )

  if(nLvls>1){

    Cols1<-c()
    for(i in 1:nLvls){
      Cols1<-rbind(Cols1, rev(colorRampPalette(c("white",RColorBrewer::brewer.pal(8,PALETTE)[i]))(length(LEGEND$LR.CURVES)+2))  )
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

    LEGEND$TITLE = paste0("Deviation Curves with Respect to ",td.Frmt(LEGEND$VARIABLE)," Splitted by ", td.Frmt(LEGEND$SPLIT.BY))

  }else{

    k=1
    for(i in LEGEND$LR.CURVES){
      Deviation.Chart <- add_lines(Deviation.Chart, x=DATA.PL[,paste0("Cumul_Xpo_",i)] , y=DATA.PL[,paste0("Error_",i)],
                                   name = td.Frmt(i), line = list(color = Cols[k], width = 3, shape = "vh") )
      k=k+1
    }

    LEGEND$TITLE = paste0("Deviation Curves with Respect to <br>",td.Frmt(LEGEND$VARIABLE))
  }

  Deviation.Chart <- layout(Deviation.Chart, margin = list(b=80),
                            xaxis = list(title = "Cumulative Exposure" ),
                            legend = list(x=1,y=1),
                            autosize=F,
                            title = LEGEND$TITLE,
                            yaxis = list(title = "% Deviation (Absolute Error)",
                                         range = c(0,max(DATA.PL[,paste0("Error_",LEGEND$LR.CURVES)]))))

  if(AS.PDF){
    library(webshot, quietly = T)
    library(png)
    tmpFile <- tempfile(fileext = ".pdf")
    export(Deviation.Chart, file = tmpFile, vwidth = 800, vheight = 500)
    return(tmpFile)
  }else{
    return(Deviation.Chart)
  }


}


