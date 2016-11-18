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





#' Model Charts
#'
#' Outputs the double lift chart or predicted vs observed plots
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
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
#' @param EVAL.THRESHOLD TRUE will evaluate the Threshold. (Leave to FALSE unless you know what you are doing)
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
Plot.Chart <- function(DATA,
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
                     MODE = "LR",
                     REBASE = T,
                     PALETTE = "Dark2",
                     DATA.ONLY = FALSE,
                     EVAL.THRESHOLD = FALSE,
                     N.BKTS = 20){


  library(dplyr,quietly = T)
  library(RColorBrewer,quietly = T)
  library(plotly,quietly = T)

  ##Fix names
  LEGEND <- LEGEND %>%
    dt.AdjNames(.,NAMES,MODE) %>%
    dt.AdjNames2(.,NAMES,MODE)

  DATA <- DATA %>%
    dt.LoadFromFeather(.,NAMES) %>%
    dt.CleanData(.,NAMES) %>%
    dt.Rebase(.,REBASE,NAMES)

  if(NAMES$VARIABLE %in% NAMES$MODELS){
    DATA$TEMP = DATA[,NAMES$VARIABLE]
    names(DATA)[ncol(DATA)] = paste0(NAMES$VARIABLE,"_Bkt")
    NAMES$VARIABLE = paste0(NAMES$VARIABLE,"_Bkt")
  }


  DATA <- DATA %>%
    dt.Earn(.,NAMES) %>%
    dt.Bucket(.,NAMES,CUTS,N.BKTS)

  if(EVAL.THRESHOLD){
    THRESHOLD <- DATA %>%
      dt.Threshold(.,NAMES,LEGEND)
  }

  DATA.AGG <- DATA %>%
    dt.Aggregate(., NAMES) %>%
    dt.FixNames(.,NAMES,LEGEND) %>%
    dt.Adjust(., LEGEND, MODE)

  if(EVAL.THRESHOLD){
    DATA.AGG <- DATA.AGG %>%
      left_join(.,THRESHOLD)
  }



  if(DATA.ONLY){
    return(list(DATA=DATA.AGG, LEGEND = LEGEND))
  }

    Cols <- brewer.pal(n = 8, name = PALETTE)
    Lvls <- unique(DATA.AGG[,LEGEND$SPLIT.BY])
    Cols1 <- rev(colorRampPalette(c("white",brewer.pal(8,PALETTE)[6]))(max(length(Lvls),length(LEGEND$CURVES))+2))
    for(i in 1:length(Lvls)){
      Cols1<-rbind(Cols1, c(rev(colorRampPalette(c("white",brewer.pal(8,PALETTE)[i]))(length(LEGEND$CURVES)+2)),
                            rep(0, max(length(Lvls),length(LEGEND$CURVES)) - length(LEGEND$CURVES)))  )
    }


    if(MODE=="LR"){

      if(is.null(NAMES$SPLIT.BY)){

        Max.Exp = max(DATA.AGG[,LEGEND$EXPOSURE])

        Final.Chart <- plot_ly() %>%
          add_bars(x = DATA.AGG[,LEGEND$VARIABLE] , y= DATA.AGG[,LEGEND$EXPOSURE], opacity = .6,name = td.Frmt(LEGEND$EXPOSURE), marker = list(color = Cols[6]) )
        k=1
        for(i in LEGEND$CURVES){
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

          for(i in rev(LEGEND$CURVES)){
            if(j != "Combined"){
              Final.Chart <- add_lines(Final.Chart, x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,i],yaxis="y2",
                                       name = paste0(j," - ",td.Frmt(i)), line = list(color = Cols1[l,k], width = 3) )
            }else{
              Final.Chart <- add_lines(Final.Chart, x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,i],yaxis="y2",
                                       name = paste0(j," - ",td.Frmt(i)), line = list(color = Cols1[l,k], width = 3, dash="dashdot") )
            }

            k = k + 1
          }
          l = l + 1
        }
      }


    Final.Chart <- layout(Final.Chart,margin = list(b=80),
                          barmode="stack", xaxis = list(title = gsub("_"," ",td.Frmt(LEGEND$VARIABLE)) ),
                          legend = list(x=1,y=1),
                          title = LEGEND$TITLE,
                          yaxis = list(title = "Exposure",
                                       range = c(0,Max.Exp*2),
                                       side="right"),
                          yaxis2 = list(title = "Loss Ratio (capped at 500%)",
                                        side="left",
                                        overlaying = "y"))
    }else if(MODE=="PvO"){


      Final.Chart <- plot_ly()

      if(is.null(NAMES$SPLIT.BY)){

        Max.Exp = max(DATA.AGG[,LEGEND$EXPOSURE])

        Final.Chart <- add_bars(Final.Chart, x =DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,LEGEND$EXPOSURE], opacity = .6,
                 name = td.Frmt(LEGEND$EXPOSURE), marker = list(color = Cols[6]) )
        k=2
        for(i in LEGEND$CURVES[-1]){
          Final.Chart <- add_lines(Final.Chart, x=DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,i],yaxis="y2",
                                   name = td.Frmt(i), line = list(color = Cols[k],
                                                         width = 3))
          k = k + 1
        }
        Final.Chart <- add_lines(Final.Chart,x=DATA.AGG[,LEGEND$VARIABLE] , y=DATA.AGG[,LEGEND$CURVES[1]],yaxis="y2",
                                 name = td.Frmt(LEGEND$CURVES[1]), line = list(color = Cols[1],
                                                                      width = 3,
                                                                      dash = "dashdot") )
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
                                    name = paste0(j," - ",td.Frmt(LEGEND$EXPOSURE.REL)), marker = list(color = Cols1[1,l]), visible = "legendonly" )
          }

          for(i in LEGEND$CURVES[-1]){
            Final.Chart <- add_lines(Final.Chart, x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,i],yaxis="y2",
                                     name = paste0(j," - ",td.Frmt(i)), line = list(color = Cols1[l,k],
                                                           width = 3))
            k = k + 1
          }
          Final.Chart <- add_lines(Final.Chart,x=DATA.AGG2[,LEGEND$VARIABLE] , y=DATA.AGG2[,LEGEND$CURVES[1]],yaxis="y2",
                                   name = paste0(j," - ",td.Frmt(LEGEND$CURVES[1])), line = list(color = Cols1[l,k],
                                                                        width = 3,
                                                                        dash = "dashdot") )
          l = l + 1
        }

      }

      Final.Chart <-layout(Final.Chart, margin = list(b=80),
          barmode="stack", xaxis = list(title = gsub("_"," ",td.Frmt(LEGEND$VARIABLE)) ),
          legend = list(x=1,y=1),
          title = LEGEND$TITLE,
          yaxis = list(title = "Exposure",
                       range = c(0,Max.Exp*2),
                       side="right"),
          yaxis2 = list( title = "Loss Cost",
                        side="left",
                       overlaying="y"))

    }


  if(is.null(PATH)){
    return(Final.Chart)
  }else{
    htmlwidgets::saveWidget(Final.Chart,file = paste0(PATH,"/",NAMES$VARIABLE,".html"),selfcontained = T )
    cat(paste0("Plot ",NAMES$VARIABLE," outputted to ",PATH," \n") )
  }

}



#' Deviation Charts
#'
#' Outputs a chart of % errors distribution with respect to a variable. Each point on the line represents Pr(% Error >= y) = x.
#' Note that these graphs are heavily influenced by the bucketting of the variable.
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
#' @param THRESHOLD The Table of thresholds with the probability level and the deviation tolerence at that level.
#' @param PALETTE ColorBrewer Palette, Default is \code{'Dark2'}
#' @param DATA.ONLY TRUE will simply return a table instead of the plot
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
Deviation.Plot <- function(DATA,
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
                       THRESHOLD = NULL,
                       PALETTE = "Dark2",
                       DATA.ONLY = FALSE,
                       N.BKTS = 20){


  ## Get the table from Plot.Chart function
  TABLE <- Plot.Chart(DATA = DATA,
                      NAMES = NAMES,
                      LEGEND = LEGEND,
                      CUTS = CUTS,
                      MODE = "LR",
                      REBASE = T,
                      DATA.ONLY = T,
                      N.BKTS = N.BKTS,
                      EVAL.THRESHOLD = T)

  LEGEND = TABLE$LEGEND
  DATA = TABLE$DATA

  nLvls<-1
  if(!is.null(NAMES$SPLIT.BY)){
    Lvls<-unique(DATA[,LEGEND$SPLIT.BY])
    nLvls<-length(Lvls)
  }

  DATA.PL <- data.frame(N=1:(nrow(DATA)/nLvls))
  if(nLvls>1){
    for(j in Lvls){
      for(i in LEGEND$CURVES){
        TMP <- DATA[DATA[,LEGEND$SPLIT.BY]==j,]
        TMP$Error = abs(TMP[,i] - 1 )
        TMP<-TMP[rev(order(TMP$Error)),]
        TMP$Cumul_Xpo = cumsum(TMP[,LEGEND$EXPOSURE])/sum(TMP[,LEGEND$EXPOSURE])

        names(TMP)[(ncol(TMP)-1):ncol(TMP)] = c(paste0(j,"_Error_",i),paste0(j,"_Cumul_Xpo_",i))

        DATA.PL = data.frame(DATA.PL, TMP[,c(paste0(j,"_Error_",i),paste0(j,"_Cumul_Xpo_",i))])
      }
    }
  }else{
    for(i in LEGEND$CURVES){
      TMP <- DATA
      TMP$Error = abs(TMP[,i] - 1 )
      TMP<-TMP[rev(order(TMP$Error)),]
      TMP$Cumul_Xpo = cumsum(TMP[,LEGEND$EXPOSURE])/sum(TMP[,LEGEND$EXPOSURE])

      names(TMP)[(ncol(TMP)-1):ncol(TMP)] = c(paste0("Error_",i),paste0("Cumul_Xpo_",i))

      DATA.PL = data.frame(DATA.PL, TMP[,c(paste0("Error_",i),paste0("Cumul_Xpo_",i))])
    }
  }

  if(is.null(THRESHOLD)){
    for(i in c("THRESHOLD_2SD", "THRESHOLD_3SD")){
      TMP <- DATA
      TMP$Error = TMP[,i]
      TMP<-TMP[rev(order(TMP$Error)),]
      TMP$Cumul_Xpo = cumsum(TMP[,LEGEND$EXPOSURE])/sum(TMP[,LEGEND$EXPOSURE])

      names(TMP)[(ncol(TMP)-1):ncol(TMP)] = c(paste0("Error_",i),paste0("Cumul_Xpo_",i))

      DATA.PL = data.frame(DATA.PL, TMP[,c(paste0("Error_",i),paste0("Cumul_Xpo_",i))])
    }
  }


  if(DATA.ONLY){
    return(DATA.PL)
  }

  Cols <- brewer.pal(n = 8, name = PALETTE)

  Deviation.Chart <- plot_ly()
  if(!is.null(THRESHOLD)){
    Deviation.Chart <- add_lines(Deviation.Chart, x = c(0,1), y = c(1,1),
              name = "Threshold", line = list(color = Cols[4], width = 0 ), showlegend=F ) %>%
      add_lines(x = THRESHOLD$Prob, y = THRESHOLD$Deviation, fill = 'tonexty',
                name = "Threshold", line = list(color = Cols[4], width = 0, shape = "spline" ) )
  }else{
    Deviation.Chart <- add_lines(Deviation.Chart, x = c(0,1), y = c(1,1),
                                 name = "Threshold", line = list(color = Cols[4], width = 0 ), showlegend=F ) %>%
      add_lines(x = DATA.PL$Cumul_Xpo_THRESHOLD_3SD, y = DATA.PL$Error_THRESHOLD_3SD, fill = 'tonexty',
                name = "3SD Threshold", line = list(color = Cols[4], width = 0, shape = "spline" ) ) %>%
      add_lines(x = DATA.PL$Cumul_Xpo_THRESHOLD_2SD, y = DATA.PL$Error_THRESHOLD_2SD, fill = 'tonexty',
                name = "2SD Threshold", line = list(color = Cols[6], width = 0, shape = "spline" ) )
  }

  if(nLvls>1){

    Cols1<-c()
    for(i in 1:length(Lvls)){
      Cols1<-rbind(Cols1, c(rev(colorRampPalette(c("white",brewer.pal(8,PALETTE)[i]))(length(LEGEND$CURVES)+2)),
                            rep(0, max(length(Lvls),length(LEGEND$CURVES)) - length(LEGEND$CURVES)))  )
    }

    l=1
    for(j in rev(Lvls)){
      k=1
      for(i in LEGEND$CURVES){
        Deviation.Chart <- add_lines(Deviation.Chart, x=DATA.PL[,paste0(j,"_Cumul_Xpo_",i)] , y=DATA.PL[,paste0(j,"_Error_",i)],
                                     name = paste0(j,"_",td.Frmt(i)), line = list(color = Cols1[k,l], width = 3, shape = "vh") )
        k=k+1
      }
      l=l+1
    }

    LEGEND$TITLE = paste0("Deviation Curves with Respect to ",td.Frmt(LEGEND$VARIABLE),
                          " for ",paste0(td.Frmt(LEGEND$MODELS),collapse = " & ")," Splitted by ", td.Frmt(LEGEND$SPLIT.BY))

  }else{

    k=1
    for(i in LEGEND$CURVES){
      Deviation.Chart <- add_lines(Deviation.Chart, x=DATA.PL[,paste0("Cumul_Xpo_",i)] , y=DATA.PL[,paste0("Error_",i)],
                                   name = td.Frmt(i), line = list(color = Cols[k], width = 3, shape = "vh") )
      k=k+1
    }

    LEGEND$TITLE = paste0("Deviation Curves with Respect to ",td.Frmt(LEGEND$VARIABLE),
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


