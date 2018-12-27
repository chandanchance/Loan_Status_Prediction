an<-read.csv("a.csv",header = TRUE,sep = ',')
CrossTable(Test$Predicted_status,an$status)
