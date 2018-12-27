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
library(rpart)
library(rpart.plot)


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

#Married:
levels(df_combi$marital_status)[3] <- 'Yes'
levels(df_combi$marital_status)

#Self_Employed:
levels(df_combi$is_self_employed)[1] <- 'No'




df_combi$gender<-as.factor(ifelse(df_combi$gender=='M',1,ifelse(df_combi$gender=='F',0,NA)))

gendertable<-df_combi[!is.na(df_combi$gender),-1]
gender.rpart <- glm(gender ~ .-1,data =gendertable ,family = "binomial")

checkgender<- predict(gender.rpart, df_combi[is.na(df_combi$gender),], type = "response")
checkgender[6]<-0.7
df_combi$gender[is.na(df_combi$gender)] <-ifelse(checkgender>=0.5,1,0)

levels(df_combi$gender)[1] <- 'F'
levels(df_combi$gender)[2] <- 'M'

levels(df_combi$dependents)[5] <- NA
levels(df_combi$dependents)[4] <- 3

dependentstable<-df_combi[!is.na(df_combi$dependents),-1]
dependents.rpart <- lm(dependents ~ .-1,data =dependentstable)

checkdependents<- predict(dependents.rpart, df_combi[is.na(df_combi$dependents),])
checkdependents[c(1,8,11)]<-0
df_combi$dependents[is.na(df_combi$dependents)] <-ifelse(checkdependents>=1,1,ifelse(checkdependents>=2,2,ifelse(checkdependents>=3,3,0)))

levels(df_combi$dependents)[4] <- '3+'



#Replace missing values in Loan_Amount_term:
summary(df_combi$loan_amount)
#Use rpart:

loan_duration.rpart <- rpart(loan_amount ~ .,
                             data = df_combi[!is.na(df_combi$loan_amount),-1],
                             method = "anova")
loan_duration.rpart$cptable
rpart.plot(loan_duration.rpart)
#Use this to replace missing values in Loan Amount Term:
df_combi$loan_amount[is.na(df_combi$loan_amount)] <- predict(loan_duration.rpart, df_combi[is.na(df_combi$loan_amount),])


#Use lm to predict Loan Amount:
lm.loan_amount <- lm(loan_amount ~ .,data = df_combi[!is.na(df_combi$loan_amount),-1])
summary(lm.loan_amount)
#Use this to replace missing values in Loan Amount:
df_combi$loan_amount[is.na(df_combi$loan_amount)] <- predict(lm.loan_amount, df_combi[is.na(df_combi$loan_amount),])
summary(df_combi)

#Treat missing values in loan_amount_term:
rpart.loan_amount_term <- rpart(loan_amount_term ~ .,data = df_combi[!is.na(df_combi$loan_amount_term),-1],method = "anova")
summary(rpart.loan_amount_term)
rpart.plot(rpart.loan_amount_term)
rpart.loan_amount_term$cptable

#Use the data not containing missing values to predict missing values`
df_combi$loan_amount_term[is.na(df_combi$loan_amount_term)] <- predict(rpart.loan_amount_term, df_combi[is.na(df_combi$loan_amount_term),])
summary(df_combi)

#Treat missing values in CreditHistory:
rpart.credit_hist <- rpart(credit_history ~ .,data = df_combi[!is.na(df_combi$credit_history),-1],method = "anova")
summary(rpart.credit_hist)
rpart.plot(rpart.credit_hist)
rpart.credit_hist$cptable

#Use the data not containing missing values to predict missing values
df_combi$credit_history[is.na(df_combi$credit_history)] <- predict(rpart.credit_hist, df_combi[is.na(df_combi$credit_history),])
summary(df_combi)

df_combi$credit_history<-ifelse(df_combi$credit_history>=0.5,1,0)
df_combi[,11]<-as.factor(df_combi[,11])

df_combi$credit_history<-round(df_combi$loan_amount_term)
df_combi[,10]<-as.factor(df_combi[,10])
#Split back into train and test:
df_train <- df_combi[1:100,]
df_test <- df_combi[101:600,]

status<-train[,13]

df_train<-cbind(df_train,status)

Train<-df_train
Test<-df_test


for(i in c(7,8,9)){if(is.numeric(Train[,i])){Train[,i]<-outlier.one(Train[,i])}}
for(i in c(7,8,9)){if(is.numeric(Test[,i])){Test[,i]<-outlier.one(Test[,i])}}

Train[,13]<-ifelse(Train[,13]=='Y',1,0)


Train1<-sample.split(Train,SplitRatio = 2/3)

Train<-subset(Train,Train1==TRUE)
TrainTest<-subset(Train,Train1==FALSE)


#model1<-randomForest(status~.,data = Train[-1],mtry=5,na.action = na.roughfix)

#model1 <- glm(status ~  marital_status + is_self_employed + loan_amount + loan_amount_term + credit_history + property_area - 1, data = Train[-1], family = "binomial",na.action = na.roughfix)

model1 <- rpart(status ~ .,Train[-1])

Train$Predicted_status <- predict(model1 ,newdata = Train[-1])
Train$Predicted_status <- ifelse(Train$Predicted_status>=0.47,1,0)

TrainTest$Predicted_status <- predict(model1 ,newdata = TrainTest[-1])
TrainTest$Predicted_status <-as.factor(ifelse(TrainTest$Predicted_status>=0.47,1,0))

Test$Predicted_status <- predict(model1 ,newdata = Test[-1])
Test$Predicted_status <- ifelse(Test$Predicted_status>=0.47,1,0)


CrossTable(Train$status, Train$Predicted_status)
CrossTable(TrainTest$status,TrainTest$Predicted_status)
status<-ifelse(Test$Predicted_status==1,"Y","N")
loan_id<-Test$loan_id
#write.csv(cbind(loan_id,status),"MyNA_NoOutliers_logistic.csv")
