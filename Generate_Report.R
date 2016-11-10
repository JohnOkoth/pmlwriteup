GenerateReport<-function(Coverage,File,Variable,Report){
  library(knitr)
  library(rmarkdown)
  
  
  #Rmd file path
  directory<-("/TARIF/Pricing Innovation/2016/MCG-05-JPO/ONAU_RSP/Monitoring_Project/Saved_Data")
  setwd(directory)
  #setwd("/TARIF/Pricing Innovation/2016/MCG-05-JPO/ONAU_RSP/Monitoring_Project/Saved_Data/COLL/Age")
  
  

  if (Report=="PvO"){
  
    rmarkdown::render("PvO.Rmd",params = list(Coverage=Coverage, File=File, Variable=Variable))  
  
  }else if (Report=="Lift"){
    
    rmarkdown::render("Lift.Rmd",params = list(Coverage=Coverage, File=File, Variable=Variable))
  
    }else if (Report=="Model_Stats"){
    
    rmarkdown::render("Model_Stats.Rmd",params = list(Coverage=Coverage, File=File, Variable=Variable))
  
      }else{
    
    rmarkdown::render("test3.Rmd",params=list(Coverage=Coverage, File=File, Variable=Variable))
  }
   
}

