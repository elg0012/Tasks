setwd('~/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
Data
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
#The data columns are year, month, day, dotw, start_hour, start_minute, end_hour, end_minute, event, value, units, and caregiver
head(Data)
#Data[1,] shows the data for all of row one. Data[2,] shows the data for all of row two. Data[1:3,] shows all of the data for rows one, two, and three. Data[1:3, 4] shows the column data of column four for rows one, two, and three. Data[1:5, 1:3] shows the column data of columns one, two, and three for rows one, two, three, four, and five.
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
#Data[257, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
#There are six rows in this new object, and each row represents an entry dealing with the amount of ounces he had from a bottle. 
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
#All three of these ways of finding the same events regarding the bottle are the same because they are just different ways of doing the exact same command. Data in column nine shows if he received a bottle. The event column and column nine are the same so this justifies the next way of getting the same result, and the $event also refers to the event column (column 9). 
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d" , origin = "2019-04-18")
Data$age <- dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age) ,]
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)