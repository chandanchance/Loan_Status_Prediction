cat("\014") 
rm(list=ls())


library(caret)
library(readr)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(ROCR)
library(pROC)
library(randomForest)
library(Hmisc)
library(gmodels)
library(C50)
library(MASS)
library(caTools)


outlier.one <- function(value){
  x<-value
  low=as.numeric(quantile(x)[2] - IQR(x)*1.5)
  high=as.numeric(IQR(x)*1.5 + quantile(x)[4])
  coutn<-length(x)
  for(i in 1:coutn){ifelse(x[i]>high,x[i]<-high,ifelse(x[i]<low,x[i]<-low,x[i]<-x[i]))}
  #assign(namea,x,envir = .GlobalEnv)
  return(x)
}

Train<-read.csv("train_data.csv",header = TRUE,sep = ',')
Test<-read.csv("test_data.csv",header = TRUE,sep = ',')



Train[,13]<-ifelse(Train[,13]=='Y',1,0)


Train1<-sample.split(Train,SplitRatio = 2/3)

Train<-subset(Train,Train1==TRUE)
TrainTest<-subset(Train,Train1==FALSE)

#Train<-Train[,-1]
#Test<-Test[,-1]

for(i in 1:12){if(is.numeric(Train[,i])){Train[,i]<-impute(Train[,i])}}
for(i in 1:12){if(is.numeric(TrainTest[,i])){TrainTest[,i]<-impute(TrainTest[,i])}}

#Train[,14]<-as.factor(Train[,13])
#Test$marital_status<-ifelse(Test$marital_status=="Yes","Yes",ifelse(Test$marital_status=="No","No","No"))
#Test$dependents<-ifelse(Test$dependents=="0","0",ifelse(Test$dependents=="1","1",ifelse(Test$dependents=="2","2",ifelse(Test$dependents=="3+","3+","0"))))
#switch(Test$dependents,"0"={Test$dependents<-"0"},"1"={Test$dependents<-"1"},"2"={Test$dependents<-"2"},"3+"={Test$dependents<-"3+"},{Test$dependents<-"0"})

#Test$marital_status<-factor(Test$marital_status,levels = levels(Test$marital_status))

#Test$marital_status<-as.factor(Test$marital_status)
#Test$dependents<-as.factor(Test$dependents)
#Test$co_applicant_income<-as.integer(Test$co_applicant_income)

rtree_fit <- rpart(status ~ .,Train) 
rpart.plot(rtree_fit)

mylogit <- glm(status ~  marital_status + is_self_employed + loan_amount + loan_amount_term + 
                 credit_history + property_area - 1, data = Train[-1], family = "binomial",na.action = na.roughfix)
summary(mylogit)

#it required target variable to be factor variable.
#fit_C50 <- C5.0(as.factor(status) ~ .,Train)
#summary(fit_C50)
#levels(Test$marital_status)<-levels(Train$marital_status)
#Test<-na.roughfix(Test)
model1<-randomForest(status~.,data = Train[-1],mtry=5,na.action = na.roughfix)
#rtree_fit <- rpart(status ~ ., Train)
#rpart.plot(rtree_fit)

Train$Predicted_status <- predict(model1 ,newdata = Train[-1])
Train$Predicted_status <- ifelse(Train$Predicted_status>=0.5,1,0)

TrainTest$Predicted_status <- predict(model1 ,newdata = TrainTest[-1])
TrainTest$Predicted_status <-as.factor(ifelse(TrainTest$Predicted_status>=0.5,1,0))

Test$Predicted_status <- predict(model1 ,newdata = Test[-1])
Test$Predicted_status <- ifelse(Test$Predicted_status>=0.5,1,0)


CrossTable(Train$status, Train$Predicted_status)

a<-stepAIC(mylogit)
ans<-ifelse(Test$Predicted_status==1,"Y","N")
write.csv(cbind(Test$loan_id,ans),"lastTry2.csv")
