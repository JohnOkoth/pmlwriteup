
##STEP_3######
#Bucketing of variables (to create a separate script file that will do bucketing for respective variables)
data2011$Age<-as.factor(cut(data2011$Dri_Age_Nb, c(16,22,25,28,31,34,36,39,41,44,46,49,52,55,59,66,99),right = FALSE, include.lowest = TRUE))
data2011$CreditScore<-as.factor(cut(data2011$Clt_Credit_Score_No, c(0,589,634,659,680,698,714,728,740,751,760,769,777,784,791,797,804,814,833,873,999),right = FALSE, include.lowest = TRUE,na.rm=FALSE))
#######END Extraction########


###DATA AGGREGATION###
##STEP_1###
#Data Aggregation script first step for one accident year
Aggregation<-function(data,x,mycuts){
  library(dplyr)
  library(data.table)
  library(Hmisc)
  library(ggplot2)
  
  #Creating buckets for respective variable
  if(missing(mycuts)){
    data$Variable<-as.factor(cut2(x,g=15))
  }
  
  if(!missing(mycuts)){
    data$Variable<-as.factor(cut2(x,cuts=mycuts))
    #Variables that require automatic cuts
    #data$Variable=as.factor(cut2(x,  m=ceiling(length(x)*.05), onlycuts=FALSE))#Cut for Veh_Age_Nb
    #data$Variable<-as.factor(cut(x, mycuts,right=TRUE, include.lowest=FALSE))
    
  }
  
  
  #Adding NA values as factors
  #data$Variable<-addNA(as.factor(data$Variable))
  columns<-as.character(c(Variable,"Pol_Acc_Yr_Dt"))
  

  #Aggregating data experience by respective variable for an accident year with sapply function. 
  Experience1<-data.table(data)[,sapply(.SD,function(x) list(Observed=weighted.mean(Clm_COL_Am,w=Xpo_Ern_COL_Nb),
                                                             LossCost=weighted.mean(Prm_Trm_COL_Am,w=Xpo_Ern_COL_Nb),
                                                             Pred=weighted.mean(COLL_PRED,w=Xpo_Ern_COL_Nb),
                                                             Exposure=sum(Xpo_Ern_COL_Nb),SumLoss=sum(Clm_COL_Am),SumClaim=sum(Clm_COL_Nb),
                                                             #Lower=weighted.mean(Lower,w=Xpo_Ern_BI_Nb),
                                                             #Upper=weighted.mean(Upper,w=Xpo_Ern_BI_Nb),
                                                             Weight=weighted.mean(Xpo_Ern_COL_Nb,w=Xpo_Ern_COL_Nb)))[1:7],by=columns]
  
  #Adding column names.
  setnames(Experience1,c("Variable","Pol_Acc_Yr_Dt","Observed","LossCost","Pred",
                         "Exposure","SumLoss","SumClaim","Weight"))
  #setnames(Experience1,c(Variable1,"Observed","LossCost","Pred","SumXpo","SumLoss","SumClaim","Lower","Upper","Weight"))
  Experience1<-mutate(Experience1,LossRate=SumLoss/Exposure,Frequency=SumClaim/Exposure,Severity=SumLoss/SumClaim)
  
  #Scaling Loss_Rate
  Experience1$LossRate2<-as.numeric((sum(Experience1$Pred)*Experience1$LossRate)/sum(Experience1$LossRate))
  
  #Converting the data back to a data frame
  Experience1<-as.data.frame(Experience1)
  return(Experience1)
  
  
  #dir.create(file.path(mainDr,subDir))
  #setwd(file.path(mainDr,subDir))
  #filename=paste0(factor,levels(Experience1$Pol_Acc_Yr_Dt))
  #write.csv(Experience1,file =filename,row.names = FALSE)
  gc()
}


###STEP_4######
#Data aggregation for overall years from Step_3
ProfileSummaryTester4<-function(data,Variable1){
  library(dplyr)
  library(data.table)
  library(Hmisc)
  library(ggplot2)
  
  columns<-as.character(c(Variable1))
  #Getting the vector for variables to be aggregated by.
  #Aggregating data experience by respective variable and accident year with sapply function. 
  Experience1<-data.table(data)[,sapply(.SD,function(x) list(Observed=weighted.mean(LossRate,w=SumXpo),
                                                             LossRate2=weighted.mean(LossRate2,w=SumXpo),
                                                             LossCost=weighted.mean(LossCost,w=SumXpo),
                                                             Pred=weighted.mean(Pred,w=SumXpo),
                                                             Frequency=weighted.mean(Frequency,w=SumXpo),
                                                             Weight=weighted.mean(SumXpo,w=SumXpo)))[1:6],by=columns]
  
  #Adding column names.
  setnames(Experience1,c(Variable1,"Observed","LossRate2","LossCost","Pred","Frequency","Weight"))
  #Experience1<-mutate(Experience1,LossRate=SumLoss/SumXpo,Frequency=SumClaim/SumXpo,Severity=SumLoss/SumClaim)
  #Converting the data back to a data frame
  Experience1<-as.data.frame(Experience1)
  return(Experience1)
  gc()
}






#Overall Aggregation from step 1 above
BucketAggr<-function(data,Variable){
  library(dplyr)
  library(data.table)
  library(Hmisc)
  library(ggplot2)

  
  columns<-as.character(c(Variable,"Pol_Acc_Yr_Dt"))
  
  #Getting the vector for variables to be aggregated by.
  #Aggregating data experience by respective variable.
  
  Experience1<-data.table(data)[,sapply(.SD,function(x) list(Observed=weighted.mean(Observed,w=Exposure),
                                                             LossCost=weighted.mean(LossCost,w=Exposure),
                                                             Pred=weighted.mean(Pred,w=Exposure),
                                                             LossRate2=weighted.mean(LossRate2,w=Exposure),
                                                             SumLoss=weighted.mean(SumLoss,w=Exposure),
                                                             SumClaim=weighted.mean(SumClaim,w=Exposure),
                                                             Frequency=weighted.mean(Frequency,w=Exposure),
                                                             Severity=weighted.mean(Severity,w=Exposure),
                                                             Exposure=weighted.mean(Exposure,w=Exposure),
                                                             SumExpo=sum(Exposure),
                                                             Weight=weighted.mean(Weight,w=Exposure)))[1:11],by=columns]
  
  #Adding column names.
  setnames(Experience1,c(Variable,"Pol_Acc_Yr_Dt","Observed","LossCost","Pred","LossRate2","SumLoss","SumClaim",
                         "Frequency","Severity","Exposure","SumExpo","Weight"))
  #Adding new columns
  #Experience1<-mutate(Experience1,LossRate=SumLoss/Exposure,Frequency=SumClaim/Exposure,Severity=SumLoss/SumClaim)
  
  #Scaling LossRate to the predicted scale
  #Experience1$LossRate2<-as.numeric((sum(Experience1$Pred)*Experience1$LossRate)/sum(Experience1$LossRate))
  
  #Converting the data back to a data frame
  Experience1<-as.data.frame(Experience1)
  return(Experience1)
  gc()
}

#Overall Aggregation from step 1 above for Frequency and Severity buckets
FreqSevAggr<-function(data,x,mycuts){
  library(dplyr)
  library(data.table)
  library(Hmisc)
  library(ggplot2)
  
  
  #Creating buckets for respective variable
  if(missing(mycuts)){
    data$Variable<-as.factor(cut2(x,g=15))
  }
  
  if(!missing(mycuts)){
    data$Variable<-as.factor(cut2(x,cuts=mycuts))
    #data$Variable2<-as.factor(cut(x, mycuts,right=TRUE, include.lowest=FALSE))
    
  }
  
  #columns<-as.character(Variable2)
  
  #Getting the vector for variables to be aggregated by.
  #Aggregating data experience by respective variable.
  
  Experience1<-data.table(data)[,sapply(.SD,function(x) list(Observed=weighted.mean(Observed,w=Exposure),
                                                             LossCost=weighted.mean(LossCost,w=Exposure),
                                                             Pred=weighted.mean(Pred,w=Exposure),
                                                             LossRate2=weighted.mean(LossRate2,w=Exposure),
                                                             SumLoss=weighted.mean(SumLoss,w=Exposure),
                                                             SumClaim=weighted.mean(SumClaim,w=Exposure),
                                                             Exposure=weighted.mean(Exposure,w=Exposure),
                                                             SumExpo=sum(Exposure),
                                                             Weight=weighted.mean(Weight,w=Exposure)))[1:11],by=Variable]
  
  #Adding column names.
  setnames(Experience1,c("Variable","Observed","LossCost","Pred","LossRate2","SumLoss","SumClaim",
                         "Exposure","Frequemcy","Severity","SumExpo","Weight"))
  #Adding new columns
  #Experience1<-mutate(Experience1,LossRate=SumLoss/Exposure,Frequency=SumClaim/Exposure,Severity=SumLoss/SumClaim)
  
  #Scaling LossRate to the predicted scale
  #Experience1$LossRate2<-as.numeric((sum(Experience1$Pred)*Experience1$LossRate)/sum(Experience1$LossRate))
  
  #Converting the data back to a data frame
  Experience1<-as.data.frame(Experience1)
  return(Experience1)
  gc()
}

SevAggr<-function(data,x,mycuts){
  library(dplyr)
  library(data.table)
  library(Hmisc)
  library(ggplot2)
  
  
  #Creating buckets for respective variable
  if(missing(mycuts)){
    data$Variable<-as.factor(cut2(x,g=15))
  }
  
  if(!missing(mycuts)){
    data$Variable<-as.factor(cut2(x,cuts=mycuts))
    #data$Variable2<-as.factor(cut(x, mycuts,right=TRUE, include.lowest=FALSE))
    
  }
  
  #columns<-as.character(Variable2)
  
  #Getting the vector for variables to be aggregated by.
  #Aggregating data experience by respective variable.
  
  Experience1<-data.table(data)[,sapply(.SD,function(x) list(Observed=weighted.mean(Observed,w=Exposure),
                                                             LossCost=weighted.mean(LossCost,w=Exposure),
                                                             Pred=weighted.mean(Pred,w=Exposure),
                                                             LossRate2=weighted.mean(LossRate2,w=Exposure),
                                                             SumLoss=weighted.mean(SumLoss,w=Exposure),
                                                             SumClaim=weighted.mean(SumClaim,w=Exposure),
                                                             Exposure=weighted.mean(Exposure,w=Exposure),
                                                             SumExpo=sum(Exposure),
                                                             Weight=weighted.mean(Weight,w=SumClaim)))[1:11],by=Variable]
  
  #Adding column names.
  setnames(Experience1,c("Variable","Observed","LossCost","Pred","LossRate2","SumLoss","SumClaim",
                         "Exposure","Frequemcy","Severity","SumExpo","Weight"))
  #Adding new columns
  #Experience1<-mutate(Experience1,LossRate=SumLoss/Exposure,Frequency=SumClaim/Exposure,Severity=SumLoss/SumClaim)
  
  #Scaling LossRate to the predicted scale
  #Experience1$LossRate2<-as.numeric((sum(Experience1$Pred)*Experience1$LossRate)/sum(Experience1$LossRate))
  
  #Converting the data back to a data frame
  Experience1<-as.data.frame(Experience1)
  return(Experience1)
  gc()
}

