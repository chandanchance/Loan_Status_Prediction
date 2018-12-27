# clear all existing variables from console
cat("\014") 
# clear all data variables 
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


train <- read.csv('train_data.csv',stringsAsFactors = T)
test <- read.csv('test_data.csv')
label <- train['status']

df_combi <- rbind(train[,-13],test)
str(train)
summary(train)
summary(df_combi)

#Treat missing levels in data:
#Gender:
levels(df_combi$gender)[1] <- 'M'
levels(df_combi$gender)

#Married:
levels(df_combi$marital_status)[3] <- 'Yes'
levels(df_combi$marital_status)

#Dependents:
levels(df_combi$dependents)[5] <- '0'
levels(df_combi$dependents)

#Self_Employed:
levels(df_combi$is_self_employed)[1] <- 'No'


#Replace missing values in Loan_Amount_term:
summary(df_combi$loan_amount)
#Use rpart:
library(rpart)
library(rpart.plot)
loan_duration.rpart <- rpart(loan_amount ~ .,
                             data = df_combi[!is.na(df_combi$loan_amount),-1],
                             method = "anova")
loan_duration.rpart$cptable
rpart.plot(loan_duration.rpart)
#Use this to replace missing values in Loan Amount Term:
df_combi$loan_amount[is.na(df_combi$loan_amount)] <- predict(loan_duration.rpart, df_combi[is.na(df_combi$loan_amount),])

?factanal
#Use lm to predict Loan Amount:
lm.loan_amount <- lm(loan_amount ~ .,data = df_combi[!is.na(df_combi$loan_amount),-1])
summary(lm.loan_amount)
#Use this to replace missing values in Loan Amount:
df_combi$loan_amount[is.na(df_combi$loan_amount)] <- predict(lm.loan_amount, df_combi[is.na(df_combi$loan_amount),])
summary(df_combi)

#Treat missing values in CreditHistory:
rpart.credit_hist <- rpart(credit_history ~ .,data = df_combi[!is.na(df_combi$credit_history),-1],method = "anova")
summary(rpart.credit_hist)
rpart.plot(rpart.credit_hist)
rpart.credit_hist$cptable

#Use the data not containing missing values to predict missing values
df_combi$credit_history[is.na(df_combi$credit_history)] <- predict(rpart.credit_hist, df_combi[is.na(df_combi$credit_history),])
summary(df_combi)

#Treat missing values in loan_amount_term:
rpart.loan_amount_term <- rpart(loan_amount_term ~ .,data = df_combi[!is.na(df_combi$loan_amount_term),-1],method = "anova")
summary(rpart.loan_amount_term)
rpart.plot(rpart.loan_amount_term)
rpart.loan_amount_term$cptable

#Use the data not containing missing values to predict missing values`
df_combi$loan_amount_term[is.na(df_combi$loan_amount_term)] <- predict(rpart.loan_amount_term, df_combi[is.na(df_combi$loan_amount_term),])
summary(df_combi)

#Split back into train and test:
df_train <- df_combi[1:100,]
df_test <- df_combi[101:600,]

status<-train[,13]

df_train<-cbind(df_train,status)

Train<-df_train
Test<-df_test

for(i in 6:10){if(is.numeric(Train[,i])){Train[,i]<-outlier.one(Train[,i])}}
for(i in 6:10){if(is.numeric(Test[,i])){Test[,i]<-outlier.one(Test[,i])}}

Train[,13]<-ifelse(Train[,13]=='Y',1,0)


Train1<-sample.split(Train,SplitRatio = 2/3)

Train<-subset(Train,Train1==TRUE)
TrainTest<-subset(Train,Train1==FALSE)


#model1<-randomForest(status~.,data = Train[-1],mtry=5,na.action = na.roughfix)

#model1 <- glm(status ~  marital_status + is_self_employed + loan_amount + loan_amount_term + credit_history + property_area - 1, data = Train[-1], family = "binomial",na.action = na.roughfix)

model1 <- rpart(status ~ .,Train[-1])

Train$Predicted_status <- predict(model1 ,newdata = Train[-1])
Train$Predicted_status <- ifelse(Train$Predicted_status>=0.8,1,0)

TrainTest$Predicted_status <- predict(model1 ,newdata = TrainTest[-1])
TrainTest$Predicted_status <-as.factor(ifelse(TrainTest$Predicted_status>=0.8,1,0))

Test$Predicted_status <- predict(model1 ,newdata = Test[-1])
Test$Predicted_status <- ifelse(Test$Predicted_status>=0.8,1,0)


CrossTable(Train$status, Train$Predicted_status)
CrossTable(TrainTest$status,TrainTest$Predicted_status)
status<-ifelse(Test$Predicted_status==1,"Y","N")
loan_id<-Test$loan_id
#write.csv(cbind(loan_id,status),"onlineNA_NoOutliers_dt_0.6CutOff.csv")
