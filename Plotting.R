#' Lorenz Curve Plot
#'
#' Outputs the Lorenz Curve, it does some binning to make the code faster
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure
#' @param NAMES \itemize{
#' \item{MODELS}{Vector of names of the columns with the model predictions}
#' \item{OBSERVED}{Column name of the observed variable}
#' \item{EXPOSURE}{Column name of the exposure variable}
#' }
#' @param PATH Path to which the graph will be exported to. (Default \code{NULL} will display the graph instead of exporting)
#' @param SAMPLE If the data is too large you may set what proportion of the data you want it to use. (E.g. .5 will use half the data)
#' \code{NULL} will not use a sample.
#' @param DATA.ONLY TRUE will simply return a table instead of the plot
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
Plot.Lorenz<- function(DATA,
                       NAMES = list(MODELS   = NULL,
                                    OBSERVED = NULL,
                                    EXPOSURE = NULL),
                       PATH = NULL,
                       SAMPLE = NULL,
                       DATA.ONLY = FALSE,
                       N.BKTS = 50){
  library(dplyr)
  library(ggplot2)

  # Sample the data if requested.
  # Makes it much faster for large datasets
  if(is.null(SAMPLE)){
    DATA  <- DATA[, c(NAMES$MODELS, NAMES$OBSERVED, NAMES$EXPOSURE)]
  }else{
    DATA  <- DATA[sample(nrow(DATA),nrow(DATA)*SAMPLE), c(NAMES$MODELS, NAMES$OBSERVED, NAMES$EXPOSURE)]
  }

  # Calculate Earned Loss Cost (E.g. Apply the Weights)
  # To be on same basis as the observed
  DATA[,NAMES$MODELS] <- DATA[,NAMES$MODELS] * DATA[,NAMES$EXPOSURE]

  # Calculate Lorenz curve for each model
  LORENZ.DT <- data.frame()
  for(i in c(NAMES$MODELS, NAMES$OBSERVED)){
    #Sort by the predicted i
    TMP<-DATA %>%
      arrange_(i)

    # Accumulate exposure and bucket in order to reduce the number of points to plot
    TMP$Cumulative_Xpo <- cumsum(TMP[,NAMES$EXPOSURE]) / sum(TMP[,NAMES$EXPOSURE])
    TMP$Cumulative_Xpo <- Hmisc::cut2(TMP$Cumulative_Xpo, g = N.BKTS,levels.mean = T)

    TMP<-TMP %>%
      group_by(Cumulative_Xpo) %>%
      summarize_each(funs(sum))

    TMP<-as.data.frame(TMP)
    TMP$Cumulative_Xpo <- as.numeric(as.character(TMP$Cumulative_Xpo))
    TMP$Cumulative_Losses = cumsum(TMP[,NAMES$OBSERVED])/sum(TMP[,NAMES$OBSERVED])

    LORENZ.DT <- rbind(LORENZ.DT, data.frame( Model = i, TMP[,c("Cumulative_Xpo", "Cumulative_Losses") ]))
  }

  colnames(LORENZ.DT) <- c("Model","Cumulative_Xpo", "Cumulative_Losses")

  if(DATA.ONLY){
    return(LORENZ.DT)
  }

  Lorenz <- ggplot(LORENZ.DT, aes(y = Cumulative_Losses, x = Cumulative_Xpo, group = Model, color = Model) ) +
    geom_line() +
    scale_color_brewer(palette="Set2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
          axis.text.y = element_text(size=12),
          legend.position="bottom",
          legend.title=element_blank(),
          legend.text = element_text(size=12))


  ## Simply Return Plot or save plot to PNG
  if(is.null(PATH)){
    return(Lorenz)
  }else{
    cat(paste0("Lorenz plot ",NAMES$OBSERVED," outputted to ",PATH," \n") )
    png(paste0(PATH,"/Lorenz_",NAMES$OBSERVED,".png"))
    print(Lorenz)
    dev.off()
  }

}



#' Model Charts
#'
#' Outputs the double lift chart or predicted vs observed plots
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure
#' @param NAMES \itemize{
#' \item{MODELS}{Vector of names of the columns with the model predictions (non-earned)}
#' \item{OBSERVED}{Column name of the observed variable}
#' \item{EXPOSURE}{Column name of the exposure variable}
#' \item{VARIABLE}{Column name of the variable with respect to which you want the LR proof (i.e. the x-axis)}
#' \item{SPLIT.BY}{Column name of the factor variable by which you want to split the LR proofs by}
#' }
#' @param PATH Path to which the graph will be exported to. (Default \code{NULL} will display the graph instead of exporting)
#' @param CUTS The cut points for the variable if the user wants to provide them. Leave \code{NULL} if you want auto bucket.
#' @param MODE Can choose between \code{"LR"} to produce double lift chart (LR Proof) or \code{"PvO"} to produce a predicted vs observed plot.
#' @param DATA.ONLY TRUE will simply return a table instead of the plot
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
Plot.Chart <- function(DATA,
                     NAMES = list(MODELS   = NULL,
                                  OBSERVED = NULL,
                                  EXPOSURE = NULL,
                                  VARIABLE = NULL,
                                  SPLIT.BY = NULL),
                     PATH = NULL,
                     CUTS = NULL,
                     MODE = "LR",
                     DATA.ONLY = FALSE,
                     N.BKTS = 20){
  library(Hmisc)
  library(dplyr)
  library(reshape2)
  library(RColorBrewer)

  DATA <- DATA[,c(NAMES$SPLIT.BY,
                  NAMES$EXPOSURE,
                  NAMES$VARIABLE,
                  NAMES$MODELS,
                  NAMES$OBSERVED)]

  ## Earn the fitted data
  DATA[,NAMES$MODELS] = DATA[,NAMES$MODELS]* DATA[,NAMES$EXPOSURE]

  #Model diff:
  if (is.numeric( DATA[,NAMES$VARIABLE] )){

    CUTS <- Hmisc::wtd.quantile(DATA[,NAMES$VARIABLE],
                                weights = DATA[,NAMES$EXPOSURE],
                                probs = seq(1/N.BKTS, 1 - 1/N.BKTS, by = 1/N.BKTS))

    DATA[,NAMES$VARIABLE] <- as.factor( cut2(DATA[,NAMES$VARIABLE],  cuts=CUTS))
  }else{
    DATA[,NAMES$VARIABLE] <- as.factor(DATA[,NAMES$VARIABLE])
  }

  #Aggregate dataset
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


  #LR
  if(MODE == "LR"){
    DATA.AGG = DATA.AGG %>%
      cbind(., DATA.AGG[,NAMES$OBSERVED] / DATA.AGG[,c(NAMES$MODELS)])

    NAMES$CURVES = paste0("C_", NAMES$MODELS)
    names(DATA.AGG)[(length(DATA.AGG)-length(NAMES$MODELS) + 1):length(DATA.AGG)] = NAMES$CURVES

    if(length(NAMES$CURVES) > 1){
      DATA.AGG[,NAMES$CURVES] = apply(DATA.AGG[,NAMES$CURVES],2,function(x){x[x>5] = 5; return(x)})
    }else{
      DATA.AGG[DATA.AGG[,NAMES$CURVES]>5, NAMES$CURVES] = 5
    }

  }else if(MODE == "PvO"){
    DATA.AGG = DATA.AGG %>%
      cbind(., DATA.AGG[,c(NAMES$OBSERVED,NAMES$MODELS)]/DATA.AGG[,NAMES$EXPOSURE])

    NAMES$CURVES = paste0("C_", c(NAMES$OBSERVED,NAMES$MODELS))
    names(DATA.AGG)[(length(DATA.AGG)-length(NAMES$MODELS)):length(DATA.AGG)] = NAMES$CURVES
  }else{
    cat("MODE not supported, only 'LR' & 'PvO' are supported! \n")
    return(0)
  }

  if(DATA.ONLY){
    return(DATA.AGG)
  }

  #LR PROOF
  if(is.null(NAMES$SPLIT.BY)){
    DATA.PL <- melt(DATA.AGG , id=NAMES$VARIABLE)
  }else{
    DATA.PL  <- melt(DATA.AGG , id=c(NAMES$SPLIT.BY,NAMES$VARIABLE))

    DATA.PL$variable2 = as.factor(paste0(DATA.PL[,NAMES$SPLIT.BY], DATA.PL[,NAMES$VARIABLE],sep = " - "))
  }

  TOT.XPO <- sum(DATA.PL[DATA.PL$variable == NAMES$EXPOSURE, "value"])
  DATA.PL[DATA.PL$variable == NAMES$EXPOSURE, "value"] <- DATA.PL[DATA.PL$variable == NAMES$EXPOSURE, "value"] /
    max(DATA.PL[DATA.PL$variable == NAMES$EXPOSURE, "value"]) *
    median(DATA.PL[DATA.PL$variable %in% NAMES$CURVES, "value"])*.25


  if(is.null(NAMES$SPLIT.BY)){

    Final.Chart <- ggplot(DATA.PL, aes_string(x = NAMES$VARIABLE, y = "value", group= "variable", color="variable", shape="variable")  ) +
      geom_bar(data= DATA.PL[DATA.PL$variable == NAMES$EXPOSURE,], stat="identity",alpha=.75,fill="gold2",colour=NA,show.legend=FALSE) +
      geom_line(data= DATA.PL[DATA.PL$variable %in% NAMES$CURVES,], size=1) +
      geom_point(data= DATA.PL[DATA.PL$variable %in% NAMES$CURVES,], aes(shape=variable,size=.5))  +
      scale_color_brewer(palette="Set2") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
            axis.text.y = element_text(size=12),
            legend.position="bottom",
            legend.title=element_blank(),
            legend.text = element_text(size=12))

    if(MODE=="LR"){
      Final.Chart = Final.Chart + labs(x = NAMES$VARIABLE,
           y = "Loss Ratio (capped at 500%)")
    }else if(MODE=="PvO"){
      Final.Chart = Final.Chart + labs(x = NAMES$VARIABLE,
           y = "Average Loss")
    }

  }else{
    Final.Chart <-  ggplot(DATA.PL, aes_string(x = NAMES$VARIABLE, y = "value", group="variable2", color=NAMES$SPLIT.BY, shape="variable", linetype="variable")) +
      geom_bar(data= DATA.PL[DATA.PL$variable == NAMES$EXPOSURE,], stat="identity",alpha=.75,fill="gold2",colour=NA,show.legend=FALSE) +
      geom_line(data= DATA.PL[DATA.PL$variable %in% NAMES$CURVES,], size=1) +
      geom_point(data= DATA.PL[DATA.PL$variable %in% NAMES$CURVES,], aes(shape=variable,size=.5))  +
      scale_color_brewer("YlGn") +

      theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
            axis.text.y = element_text(size=12),
            legend.position="bottom",
            legend.title=element_blank(),
            legend.text = element_text(size=12))

      if(MODE=="LR"){
        Final.Chart = Final.Chart + labs(x = NAMES$VARIABLE,
                                         y = "Loss Ratio (capped at 500%)")
      }else if(MODE=="PvO"){
        Final.Chart = Final.Chart + labs(x = NAMES$VARIABLE,
                                         y = "Average Loss")
      }
  }

  if(is.null(PATH)){
    return(Final.Chart)
  }else{
    cat(paste0("Plot ",NAMES$VARIABLE," outputted to ",PATH," \n") )
    png(paste0(PATH,"/",NAMES$VARIABLE,".png"))
    print(Final.Chart)
    dev.off()
  }

}


