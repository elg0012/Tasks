setwd("~/Desktop/Evolution/Tasks/Project")
Data <- read.csv("DataSet.csv")
Data2 <- read.csv("DataSet2.csv")
Data
Data2
Male <- which(Data[,3] == 'M')
Female <- which(Data[,3] == 'F')
Region <- (Data[,6])
TailLength <- (Data[,9])
Weight <- (Data[,7])
plot(abs(TailLength), Weight, log="y")
 