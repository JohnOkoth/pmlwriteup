#' Data Checking Function
#'
#' Returns encoded data with its decryption key or decoded data
#'
#' @param DATA Data directly imported from Radar
#' @param PATH Path to folder to save report to. (If \code{NULL} only output will be printed) Can be relative to working directory.
#'
#' @return Returns a list of variables that have missing values along with which lines have a missing value
#'
#' @export
Data.CHK<-function(DATA,PATH=NULL){

  ## First put missing variables in a list
  LST<-list()
  lns<-c()
  for(i in names(DATA)){

    cat(paste0(sum(is.na(DATA[,i]))," Missing Value(s) in ",i,"\n"))

    if(sum(is.na(DATA[,i]))>0){
      LST[[i]]<-which(is.na(DATA[,i]))
      lns<-c(lns,length(LST[[i]]))
    }
  }

  ## Get the max length of missing
  if(is.null(lns)){
    maxlen<-0
  }else{
    maxlen<-max(lns)
  }

  ## Put the Variables in a dataframe with padding
  OUT<-c()
  for(i in names(LST)){
    OUT<-cbind(OUT,c(LST[[i]],rep(NA,maxlen - length(LST[[i]])))  )
  }
  OUT<-as.data.frame(OUT)
  colnames(OUT)<-names(LST)

  cat(paste0(length(OUT), " Variable(s) have missing values. \n"))

  if(!is.null(PATH)){
    write.csv(OUT,file=paste0(PATH,"/REPORT_",Sys.info()["user"],"_",Sys.Date(),".csv"), na="", row.names = F)
  }
  return(OUT)
}
