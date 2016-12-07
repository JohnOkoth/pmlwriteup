#' Generate Report Pieces
#'
#' @param PATH The path to the folder in which you want the pieces to be exported
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
#' Can also be a data prepared by Plot.Data function
#' @param VARIANCE.DATA  Dataframe from which you the variance will be calculated from. (If NULL, same data as plotting data will be used)
#' @param VARIABLES Vector of column names of the variable with respect to which you want the report
#' @param EXPOSURE Column name of the exposure variable
#' @param MODELS Vector of names of the columns with the model predictions (non-earned)
#' @param OBSERVED Column name of the observed variable
#' @param CLM.COUNT Column name of the claim counts, used for theshold evaluation for Pure Premium
#'
#' @return Exports the PDF reports to the desired folder and returns a vector of paths to the files exported
#'
#' @export
Generate.Report.Pieces<-function(PATH = getwd(),
                         DATA,
                         VARIANCE.DATA,
                         VARIABLES,
                         EXPOSURE,
                         MODELS,
                         OBSERVED,
                         CLM.COUNT){
  Exported.Paths<-c()

  cntr = 1
  sec.cntr = 0
  page.cntr = 1
  for(i in VARIABLES){
    rmarkdown::render(paste0(system.file(package="TDTools"),"/Reporting/Var_Report_PDF.Rmd"),
                      output_dir = PATH,
                      output_file = paste0(i,".pdf"),
                      params = list(data          = DATA,
                                    variance.data = VARIANCE.DATA,
                                    variable      = i,
                                    exposure      = EXPOSURE,
                                    model         = MODELS,
                                    observed      = OBSERVED,
                                    clm.count     = CLM.COUNT,
                                    counter       = cntr,
                                    sec.counter   = sec.cntr,
                                    page.counter  = page.cntr))
    cntr = cntr + 3
    page.cntr = page.cntr + 3
    sec.cntr = sec.cntr + 1

    Exported.Paths <- c(Exported.Paths, paste0(PATH,"/",i,".pdf"))
  }

  return(Exported.Paths)
}


#' Collate Reports
#'
#' This function only works on linux based system. In windows the report pieces have to be collated manually using an external tool
#'
#' @param IN.PATHS Vector of paths of all the reports pieces
#' @param OUT.PATH Path to the final report to be exported including file name
#'
#' @return Exports the collated PDF reports to the desired destination
#'
#' @export
Collate.Reports<-function(IN.PATHS,OUT.PATH){
  system(paste0("pdfunite ", paste(paste0("'",IN.PATHS,"'"), collapse = " ")," '",
                paste0(system.file(package="TDTools"),"/Reporting/Appendix.pdf"),"' '",OUT.PATH,"'"))
}

#' Generate Complete Report
#'
#' This function only works on linux based system. In windows you must run \code{Generate.Report.Pieces} and manually collate the pieces.
#'
#' @param PATH The path to the folder in which you want the pieces to be exported
#' @param REPORT.NAME Name of the report
#' @param DATA  Dataframe containing the predicted, observed and exposure. (Can also be a path to a feather data or vector of feather data paths that will be row bound)
#' Can also be a data prepared by Plot.Data function
#' @param VARIANCE.DATA  Dataframe from which you the variance will be calculated from. (If NULL, same data as plotting data will be used)
#' @param VARIABLES Vector of column names of the variable with respect to which you want the report
#' @param EXPOSURE Column name of the exposure variable
#' @param MODELS Vector of names of the columns with the model predictions (non-earned)
#' @param OBSERVED Column name of the observed variable
#' @param CLM.COUNT Column name of the claim counts, used for theshold evaluation for Pure Premium
#'
#' @return Exports the PDF reports to the desired folder and returns a vector of paths to the files exported
#'
#' @export
Generate.Report<-function(PATH = getwd(),
                          REPORT.NAME = "Final_Report",
                          DATA,
                          VARIANCE.DATA,
                          VARIABLES,
                          EXPOSURE,
                          MODELS,
                          OBSERVED,
                          CLM.COUNT){

  IN.PT<-Generate.Report.Pieces(PATH = PATH,
                        DATA = DATA,
                        VARIANCE.DATA = VARIANCE.DATA,
                        VARIABLES = VARIABLES,
                        EXPOSURE = EXPOSURE,
                        MODELS =  MODELS,
                        OBSERVED = OBSERVED,
                        CLM.COUNT = CLM.COUNT)

  OUT.PATH<-paste0(PATH, "/",REPORT.NAME,".pdf")

  Collate.Reports(IN.PATHS = IN.PT,
                  OUT.PATH = OUT.PATH)

  file.remove(IN.PT)

  cat("Report Exported to",OUT.PATH,"\n")
}

