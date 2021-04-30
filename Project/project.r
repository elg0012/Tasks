setwd("~/Desktop/Evolution/Tasks/Project")
Data <- read.csv("DataSet.csv")
Data2 <- read.csv("DataSet2.csv")
Data
Data2
Male <- which(Data[,3] == 'M')
Female <- which(Data[,3] == 'F')
Region <- (Data[,6])
TailLength <- (Data[,9])
BodyMass <- (Data[,7])
plot(abs(TailLength), BodyMass)
Mod <- lm(BodyMass ~ abs(TailLength))
abline(Mod, col="red")



citation()
Southeast <- which(Data[,6] == 'Southeast')
Southeast
West <- which(Data[,6] == 'West')
West
Northeast <- which(Data[,6] == 'Northeast')
Northeast