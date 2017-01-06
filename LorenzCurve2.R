#Testing another approach for calculating the gini index and lorenz curve
#Step by step computation                   

#Data Extraction





#Reference tables
LORENZ.DT <- data.frame(N=1:100)
GINI <- data.frame(Models = c("Pred", "Clm_BI_Am"), Gini_Index = 0)


#Order the Predicted
Experience1<-TestData[order(TestData$BI_PRED),]

#Basing
Experience1$Pred<-as.numeric(Experience1[,"BI_PRED"]*Experience1[,"Xpo_Ern_BI_Nb"])

#Option 1 get the cumulative exposures and losses to compute the gini index

Experience1$Cumulative_Xpo <- cumsum(Experience1[,"Xpo_Ern_BI_Nb"]) / sum(Experience1[,"Xpo_Ern_BI_Nb"])
Experience1$Cumulative_Loss <- cumsum(Experience1[,"Clm_BI_Am"]) / sum(Experience1[,"Clm_BI_Am"])
#Experience1$Cumulative_Pred <- cumsum(Experience1[,"BI_PRED"]) / sum(Experience1[,"BI_PRED"])

#Earned losses
Experience1$Cumulative_Pred2 <- cumsum(Experience1[,"Pred"]) / sum(Experience1[,"Pred"])


#Get the Gini index value
n<-nrow(Experience1)

#Getting the gini index value for the losses
GINI[GINI$Models == "Clm_BI_Am", "Gini_Index"] <- sum(Experience1$Cumulative_Loss[-1] * Experience1$Cumulative_Xpo[-n]) - sum(Experience1$Cumulative_Loss[-n] * Experience1$Cumulative_Xpo[-1])
GINI[GINI$Models == "Pred", "Gini_Index"] <- sum(Experience1$Cumulative_Pred2[-1] * Experience1$Cumulative_Xpo[-n]) - sum(Experience1$Cumulative_Pred2[-n] * Experience1$Cumulative_Xpo[-1])




#Building the Lorenz Curve
CUTS <- quantile(Experience1$Cumulative_Xpo, probs = seq(1/100, 1 - 1/100, by = 1/100))%>%unique(.)

Experience1$Cumulative_Xpo<- Hmisc::cut2(Experience1$Cumulative_Xpo, cuts = CUTS, levels.mean = T)

#Grouping by Exposure
Experience2<-Experience1 %>%group_by(Cumulative_Xpo) %>%summarize_each(funs(sum))

Experience2<-as.data.frame(Experience2)

#Limiting variables
Experence3<-Experience2[,c("Cumulative_Xpo","Clm_BI_Am","Pred")]


Experence3$CumXpo<-as.numeric(as.character(Experence3$Cumulative_Xpo))

Experence3$cLoss <- cumsum(Experence3[,"Clm_BI_Am"])/sum(Experence3[,"Clm_BI_Am"])

#Experence3$cBIPred <- cumsum(Experence3[,"BI_PRED"])/sum(Experence3[,"BI_PRED"])

Experence3$cPred <- cumsum(Experence3[,"Pred"])/sum(Experence3[,"Pred"])




K<-list(Lorenz=Experence3,Gini=GINI)

ggk<-ggplot2::ggplot(Experence3,aes(x=CumXpo,group=1))+
  geom_line(aes(y=cLoss,colour="cLoss"))+geom_line(aes(y=CumXpo))+
  geom_line(aes(y=cPred,colour="cPred"))+ylab(label = "Cum_Loss")+annotate("text", x=0.25,y=0.78, label=K$Gini)

ggplotly(ggk)
  
