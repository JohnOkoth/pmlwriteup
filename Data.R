#' CSV to Feather
#'
#' Reads a CSV and transforms it to Feather format. Uses the data.table package fread.
#'
#' @param PATH_IN  Path to CSV data
#' @param PATH_OUT Path to Feather output (Including desired filename)
#'
#' @export
csv.to.feather <- function(PATH_IN,PATH_OUT){
  library(data.table)
  library(feather)
  DATA <- fread(PATH_IN, data.table = F)

  write_feather(DATA, PATH_OUT)
  cat("Feather Data written to: ", PATH_OUT)
}


#' Get column names
#'
#' @param PATH_IN  Path to CSV data
#'
#' @return Vector of column names
#'
#' @export
get.cols.feather <- function(PATH_IN){
  library(feather)
  META <- feather_metadata(PATH_IN)

  return(names(META$types))
}


#' Read From Feather
#'
#' Reads desired columns from a Feather Data File
#'
#' @param PATH_IN  Path to CSV data
#' @param KEEP Vector of column names to keep
#' @param DROP Vector of column names to drop
#'
#' @return Dataframe containing desired columns
#'
#' @export
read.feather <- function(PATH_IN, KEEP=NULL, DROP=NULL){
  library(feather)
  META <- feather_metadata(PATH_IN)

  if(is.null(KEEP) && is.null(DROP)){
    DATA <- as.data.frame(read_feather(PATH_IN))
  }else if(!is.null(KEEP) && !is.null(DROP)){
    cat("You can only use KEEP or DROP but not both! \n")
    return(0)
  }else if(!is.null(KEEP)){
    DATA <- as.data.frame(read_feather(PATH_IN, columns = which(names(META$types) %in% KEEP)))
  }else if(!is.null(DROP)){
    DATA <- as.data.frame(read_feather(PATH_IN, columns = which(!(names(META$types) %in% DROP))))
  }
}
