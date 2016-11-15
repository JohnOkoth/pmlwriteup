#####STEP 1#######
#Data Extraction for ol files
#Currently extracting rds files.

ONAUT_Experience<-function(i){
  setwd("/DATA/Raw_Data")
  #RAW DATA EXTRACTOR#######
  #Loading data file in rds format from the main file directory 
  #Enter the last two digits for the respective accident year  
  Experience1<-readRDS(paste("ONAU20",sprintf("%02d",i),"_ol.rds",sep=""))
  setwd("/TARIF/Pricing Innovation/2016/MCG-05-JPO/RCodes/RUOx_Modeling")
  return(Experience1)
  
}


#Converting rds file to feather
rds.to.feather <- function(PATH_IN,PATH_OUT){
  #library(data.table)
  library(feather)
  setwd("/DATA/Raw_Data")
  #DATA <- fread(PATH_IN, data.table = F)
  DATA<-readRDS(PATH_IN)
  
  setwd("/DATA/Feather_Files")
  write_feather(DATA, PATH_OUT)
  cat("Feather Data written to: ", PATH_OUT)
}

#The "mother" of all bounding
test=dt.LoadFromFeather(c(list.files("DATA/Feather_Files")),"Dri_Age_Nb")


read.feather <- function(PATH_IN, KEEP=NULL, DROP=NULL){
  setwd("/DATA/Raw_Data")
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

#######

####STEP_2#######
#Data Cleaning from step 1
#The function gets the data, cleans it and generates an output with predicted values

Clean_Data<-function(DATA,Coverage){
  ## Load RUOx library
  library(RUOx)
  ## Import the model file
  setwd("/TARIF/Pricing Innovation/2016/MCG-05-JPO/ONAU_RSP")
  RSPM <- readRDS("RSP_MODEL.ruo")
  
  ## Make some adjustments required for the RSP models
  DATA$Individual_Credit_Score  = DATA$Clt_Credit_Score_No
  DATA$Coll_Deductible_Adjusted = DATA$Cov_COL_Ded_Am;
  DATA$Coll_Deductible_Adjusted[DATA$Cov_COL_Ded_Am == 50] = 100
  
  DATA$Cov_COL_Ded_Am          = DATA$Coll_Deductible_Adjusted
  DATA$Groupe_code             = DATA$Group_Code_RSP
  DATA$Rat_Terr_Hist_Type_Cd   = DATA$Terr_Type
  DATA$Veh_rg_AB_Dis_Freq      = DATA$Rg_TDI_LC_AB_DIS
  DATA$Veh_rg_AB_Med_Freq      = DATA$Rg_TDI_LC_AB_MED
  DATA$Veh_rg_BI_Freq          = DATA$Rg_TDI_LC_BI
  DATA$Veh_rg_COLL_pp          = DATA$Rg_TDI_LC_COL
  DATA$Veh_rg_COMP_pp          = DATA$Rg_TDI_LC_CMP
  DATA$Veh_rg_DC_pp            = DATA$Rg_TDI_LC_DRC
  DATA$Comp_adjt2_new          = DATA$Terr_CMP_Adj2
  
  DATA$Veh_Rg_Ab_No            = DATA$Veh_Rg_Ab_No_OL
  DATA$Veh_Rg_Cmp_No           = DATA$Veh_Rg_Col_No_OL
  DATA$Veh_Rg_Col_No           = DATA$Veh_Rg_Cmp_No_OL
  
  DATA$Clt_Insurer_Tx          = DATA$New_Insurer_Tx
  
  gc()
  
  ## List the names of the submodels available in the RSPM object
  print(RSPM$ModelList)
  
  ## Apply the BI model to the data without keeping intermediary steps (uses a lot less memory)
  RT<-Sys.time()
  if(!missing(Coverage)){
  DATA <- APPLY.MOD.lm(RMOD = RSPM, 
                       DATA = DATA, 
                       MODEL_NAME = Coverage,
                       KEEP_DATA = T)
  }
  
  if(missing(Coverage)){
  DATA<-APPLY.MOD(RMOD = RSPM,
                  DATA= DATA,
                  MODEL_NAME = NULL,
                  Split = F)
  }
  
  #Removing NA values
  DATA<-as.data.frame(DATA)
  
  #DATA<-na.omit(DATA)
  print(Sys.time()-RT)
  
  gc()
  

  #setwd("/DATA/Feather_Files")
  #write.csv(DATA, file = "data2011.csv",row.names = FALSE)

  
  setwd("/DATA/Raw_Data")
  
  return(DATA)
  
}


rds.to.feather<-function(i,PATH_OUT){
  library(feather)
  
  #setting the directory source
  setwd("/DATA/Raw_Data")
  
  #reading the rds data
  DATA<-readRDS(paste("ONAU20",sprintf("%02d",i),"_ol.rds",sep=""))
  
  write_feather(DATA,PATH_OUT)
  
  cat("Feather Data Written to:",PATH_OUT)
}