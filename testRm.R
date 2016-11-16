
#Extracting and predicting data

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
  string<-paste0(Coverage,"PRED",sep="_")
  DATA<-[!is.na(DATA[,string]),]
  
  write_feather(DATA,PATH_OUT)
  
  
  DATA<-as.data.frame(DATA)
  
  #DATA<-na.omit(DATA)
  print(Sys.time()-RT)
  
  gc()
  
  
  #setwd("/DATA/Feather_Files")
  #write.csv(DATA, file = "data2011.csv",row.names = FALSE)
  #saveRDS(DATA,file = paste0(Coverage,"_PRED",Year,".rds",sep=""),compress = TRUE)
  
  setwd(PATH)
  write_feather(DATA,paste0(Coverage,"_PRED",Year,".fth",sep=""))
  
  
  
  setwd("/DATA/Raw_Data")
  
}

#Load from feather folder
dt.LoadFromFeather <- function(DATA,NAMES){
  ## if a feather path is put in, load from feather
  if(is.character(DATA)){
    DT.paths <- DATA
    DATA <- data.frame()
    for(i in DT.paths){
      DATA <- rbind(DATA,
                    read.feather(i,
                                 KEEP = as.vector(unlist(NAMES))))
    }
  }
  return(DATA)
}


#Step1
#Load predicted data
library(TDTools)

#Plotting Lorenz Curve
OBSERVED<-paste("Clm",params$Coverage,"Am",sep="_")
EXPOSURE<-paste("Xpo","Ern",params$Coverage,"Nb",sep="_")
MODEL<-paste(params$Coverage,"PRED",sep="_")

if(is.null(params$variable)){
  DATA<-dt.LoadFromFeather(PATH,c(OBSERVED,EXPOSURE,MODEL))
  plot.lorenZ(DATA,list(MODELS=MODEL,OBSERVED=OBSERVED,EXPOSURE=EXPOSURE))
  
}else{
  
  DATA<-dt.LoadFromFeather(PATH,c(params$variable,OBSERVED,EXPOSURE,MODEL))
  plot.lorenZ(DATA,list(MODELS=MODEL,OBSERVED=OBSERVED,EXPOSURE=EXPOSURE))
}



plot.chart