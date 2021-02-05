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
#Question 1: I believe that Hypothesis 3 is testable with the data that is given. The amount of milk Beren drinks in a day has increased over time. We have all the information needed to determine if this is true. Hypotheses 1 and 2 are not testable in this situation because Beren's weight and amount of time that he naps are not given. 
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean (beren3$value[Feeds])
head(avgMilk)
#Beren eats an average of 2.36677 ounces of milk at one sitting. 
#The units of milk are ounces.
#The value column from the beren3 object was used because it shows the numbers that related to the data from "bottle" that was used when "Feeds" was created. 
#The set of square brackets with "Feeds" inside it was used to indicate what is located inside of the "Feeds" vector. This is important because it was used to find the mean of all the parts within "Feeds".
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
?cor
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par
#las=1 edited the style of the axis labels to always horizontal. mar=c(5,5,1,1) gives the number of lines of margin to be specifies on the four sides of the plot, hence why there are four numerical values. mgp=c(2, 0.5, 0) represents the values for the margin line for the axis title, axis labels, and axis line. tck=-0.01 represents the length of tick marks as a fraction of the smaller of the width or height of the plotting region. 
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#Question 2: The data is unable to be easily interpretted because there are 1) such high values of each thing making it difficult to see what it is referring and 2) because it only has one point I am assuming due to such large intervals. 
source("http://jonsmitchell.com/code/plotFxn02b.R")
#The graph we made using the code given to us in this assignment is very difficult to read and understand because there is very little information obtainable from the graph. The one on eCampus is easier to understand because it shows that as time passes throughout the day and over several days, the amount of total milk increases. This is way easier to understand even without knowing much about these types of graphs. The graph we created does not show anything regarding the time passed throughout the day. This makes it very hard to correlate anything. This causes the data to be in one place rather than spread out to where we may observe it. Hypothesis 3 from above was the one tested. 
unique(beren3$event)
#The hypothesis I would like to test is that as Beren has gotten older, he uses the bathroom more frequently. 
#I have not finished the extra credit yet. I will resubmit this later today when I do. 
Naps <- which(Data[,9] == 'nap')
beren4 <- Data[Naps,]
beren4
startHour <- (beren4$start_hour)
startMin <- (beren4$start_minute)
endHour <- (beren4$end_hour)
endMin <- (beren4$end_minute)
sleepTime1 <- (endHour - startHour)*(60) 
sleepTime2 <- (endMin - startMin)
sleepTime1
sleepTime2
totalTime <- tapply(sleepTime1, sleepTime2, sum)
totalTime
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalTime)), totalTime, type="b", pch=16, xlab="age in days", ylab="nap times in minutes")
startTime <- ((startHour)*60) + startMin
startTime
cor.test(startTime, totalTime)
#The nature of the relationship is 