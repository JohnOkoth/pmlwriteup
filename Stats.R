
#' Get Model stats
#'
#' Outputs the MSE or RMSE by variable or total
#'
#' @param DATA  Dataframe containing the predicted, observed and exposure
#' @param NAMES \itemize{
#' \item{MODELS}{Vector of names of the columns with the model predictions (non-earned)}
#' \item{OBSERVED}{Column name of the observed variable}
#' \item{EXPOSURE}{Column name of the exposure variable}
#' \item{VARIABLE}{Column name of the variable with respect to which you want the LR proof (i.e. the x-axis)}
#' }
#' @param CUTS The cut points for the variable if the user wants to provide them. Leave \code{NULL} if you want auto bucket.
#' @param MODE Can choose between \code{"TOTAL"} or \code{"BYVAR"} (\code{"total"} will ignore unnecessary parameters)
#' @param TYPE Can choose between \code{"MSE"}, \code{"RMSE"}, \code{"LOGMSE"}
#' @param N.BKTS Number of groupings to do for. Lower number of groupings offer faster performance but more approximate
#'
#' @return Either a .png file in the path or output a graph in R
#'
#' @export
Stat.Error <- function(DATA,
                       NAMES = list(MODELS   = NULL,
                                    OBSERVED = NULL,
                                    EXPOSURE = NULL,
                                    VARIABLE = NULL),
                       CUTS = NULL,
                       MODE = "TOTAL",
                       TYPE = "MSE",
                       N.BKTS = 20){

  library(Hmisc)

  MSE<-function(X){
    out<-c()
    for(i in NAMES$MODELS){
      out<-c(out,
             Hmisc::wtd.mean( (X[,i]*X[,NAMES$EXPOSURE] - X[,NAMES$OBSERVED])^2, X[,NAMES$EXPOSURE] )   )
    }
    return(out)
  }

  LOGMSE<-function(X){
    out<-c()
    X[X[,NAMES$OBSERVED] == 0,NAMES$OBSERVED] = 1
    for(i in NAMES$MODELS){
      X[X[,i]==0,i] = 1
      out<-c(out,
             Hmisc::wtd.mean( ( log(X[,i]*X[,NAMES$EXPOSURE]) - log(X[,NAMES$OBSERVED]))^2, X[,NAMES$EXPOSURE] )   )
    }
    return(out)
  }

  RMSE<-function(X){
    return(sqrt(MSE(X)))
  }

  if(MODE=="TOTAL"){

    if(TYPE == "MSE"){
      Stat = MSE(DATA)
    }else if(TYPE == "RMSE"){
      Stat = RMSE(DATA)
    }else if(TYPE == "LOGMSE"){
      Stat = LOGMSE(DATA)
    }else{
      cat("TYPE not supported! (Only 'MSE', 'RMSE' and 'LOGMSE')\n")
    }

    Stat.Tbl <- data.frame(Models = NAMES$MODELS, Stat = Stat)

  }else if(MODE == "BYVAR"){
    library(dplyr)
    library(reshape2)
    library(RColorBrewer)

    DATA <- DATA[,c(NAMES$EXPOSURE,
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
    DATA.AGG <- data.frame(NB.OBS = rep(1,nrow(DATA)), DATA)

    DATA.AGG <-  DATA.AGG %>%
      group_by_(NAMES$VARIABLE) %>%
      summarise_each_(funs(sum), c(NAMES$MODELS,
                                   NAMES$OBSERVED,
                                   "NB.OBS",
                                   NAMES$EXPOSURE)) %>%
      mutate()

    DATA.AGG <- as.data.frame(DATA.AGG)

    #Averages
    DATA.AGG[,c(NAMES$MODELS)] = DATA.AGG[,c(NAMES$MODELS)] / DATA.AGG[,NAMES$EXPOSURE]

    if(TYPE == "MSE"){
      Stat = MSE(DATA.AGG)
    }else if(TYPE == "RMSE"){
      Stat = RMSE(DATA.AGG)
    }else if(TYPE == "LOGMSE"){
      Stat = LOGMSE(DATA)
    }else{
      cat("TYPE not supported! (Only 'MSE' and 'RMSE')\n")
    }

    Stat.Tbl <- data.frame(Models = NAMES$MODELS, Stat = Stat)

  }else{
    cat("MODE not supported! (Only 'TOTAL' and 'BYVAR')\n")
  }

  return(Stat.Tbl)

}
