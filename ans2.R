cat("\014") 
rm(list=ls())


library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(randomForest)

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

Train<-Train[,-1]
Test<-Test[,-1]

for(i in 2:12){if(is.numeric(Train[,i] )){Train[,i]<-outlier.one(Train[,i])}}
#for(i in 2:12){if(is.numeric(Test[,i] & !is.na(Train[,i]))){Test[,i]<-outlier.one(Test[,i])}}

Train<-na.roughfix(Train)
#Train<-outlier(Train$co_applicant_income)

status<-Train['status']

Train[,13]<-ifelse(Train[,13]=='Y',1,0)

ohe_feats<-c('gender','marital_status','dependents','qualification','is_self_employed','property_area')

dummies <- dummyVars(~ gender +  marital_status + dependents+ qualification+ is_self_employed+ property_area, data = Train)

df_all_ohe <- as.data.frame(predict(dummies, newdata = Train))

df_all_combined <- cbind(Train[,-c(which(colnames(Train) %in% ohe_feats))],df_all_ohe)

#df_all_combined <- as.factor(ifelse(df_all_combined$age < 0,1,0))
df_all_combined<-df_all_combined[,-25]
df_all_combined<-data.matrix(df_all_combined)

y <- recode(status$status,"'Y'=1; 'N'=0")

xgboost(data = df_all_combined, 
        booster = "gbtree", 
        label = y,
        objective = "binary:logistic", 
        max.depth = 5, 
        eta = 0.5, 
        nthread = 2, 
        nround = 2, 
        min_child_weight = 1, 
        subsample = 0.5, 
        colsample_bytree = 1, 
        num_parallel_tree = 1)
