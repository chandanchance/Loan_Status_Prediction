cat("\014") 
rm(list=ls())



ans1<-read.csv("uttara_100%.csv",header = TRUE,sep = ',')
ans1$V2<-ifelse(ans1$V2==1,"Y","N")
write.csv(ans1,"uttara_100_YN.csv")
