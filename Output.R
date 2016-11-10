#' Export a list of tables to Excel
#'
#' @param TABLES A List of a list of Tables to be exported to excel. The outer list defines the excel sheet and the inner list defines the tables.
#' The names in the the list define the named ranges and the sheets.
#' @param PATH Path to the excel file you want the tables exported to. If it does not exist, a new xlsx file will be created.
#'
#' @export
Tables.to.XL <-function(TABLES,PATH){

  library(JAVAPACK)
  cat("Setting JAVA heap to 8g \n")
  options( java.parameters = "-Xmx8g" )
  library(XLConnect)

  wb.out <- loadWorkbook(PATH, create = TRUE)

  for(kk in names(TABLES)){
    Sh.name = kk
    createSheet(wb.out, name = Sh.name)

    MultiVariate = TABLES[[kk]]

    pt0 = 1
    for(i in 1:length(MultiVariate)){

      createName(wb.out, name = names(MultiVariate)[i], formula = paste0(Sh.name,"!$A$",pt0), overwrite = TRUE)
      pt0<-pt0+nrow(MultiVariate[[i]])+2
      writeNamedRegion(wb.out, MultiVariate[[i]], names(MultiVariate)[i], TRUE)
    }

    saveWorkbook(wb.out )
    cat(paste0("Tables ",kk, " saved to: ",PATH,"\n"))
  }
}
