setwd('~/Desktop/Evolution/Tasks/Task_04')
source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type = "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit(nruns = 10, n = 5, ngens = 100, init_p = 0.5, h=1, s=0)
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation - Observed)^2/Expectation))
Chisq
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(5, 0, 0, 35)
Observed <- c(2, 3, 10, 30)
Observed <- c(10, 10, 10, 10)
Observed <- c(20, 20, 0, 0)
Observed <- c(40, 0, 0, 0)
setwd("~/Desktop/Evolution/Tasks/Task_04")
source("http://jonsmitchell.com/code/fxn05.R")
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
results
length(results)
nrow(results)
ncol(results)
colnames(results)
head(results)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yello", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
#Based on this plot, it is easy to see that the lower the chi squared value, the more even the colored bars are. The higher the chi squared value, the higher a single bar is than the others. This plot shows us that the smaller the chi squared number, the closer the observed phenotypes were to the expected phenotypes.
Avg <- mean(Chisqs)
Avg
#The average Chi-squared is 60.99081. This value is bigger than the critical value of 11.70. 
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
backgroundAvgs
#Yes, the chi-squared differs by background. 
propSig <- length(which(Chisq > 11.70))/length(Chisqs)
percSig <- round(100*propSig)
propSig
percSig
#According to this, 0 of the trials had a significant p-value. This did not really surprise me after seeing the chi-sqared values.
#I do not think the only thing driving this number is natural selection. I believe drift also plays a role and possibly a couple other factors.
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2,0.5, 0), tck=-0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels=backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
	Data <- Chisqs[which(results[,3] == i)]
	addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
	counter <- counter + 1
}
abline(v=11.70, lty=2, lwd=2, col='black')
#The backgrounds (paper) where fewer toothpicks could blend well had the smallest distribution to the right.
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel.sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
simDraws <- function(nruns, ncols=6, nstart=10, nrounds=3, mu=0, twoway=TRUE, w=NULL)
Simulation8 <- simDraws(1e4, mu=2)
addHist(Y=8, Dat=Simulation8, Color=rgb(0,0,0,0.25))
Simulation9 <- simDraws(1ef, mu=300)
addHist(Y=8, Data=Simulation9, Color+rgb=(0,0,0,0.25))
Pop2 <- simPop(Popsize=50, nGenerations=100, initial_p=0.5, h=1, s=0, mu=0, twoway=TRUE, w=NULL)
plot(1:nrow(Pop2), Pop2[,1], ylim=c(0,1), type="l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop2), Pop2[,2], lwd=2, col="red")
legend("topleft", legend=c("a", "b"), col=c("black", "red"), lwd=2, bty="n")
Pop3 <- simPop(Popsize=50, nGenerations=100, initial_p=0.5, h=1, s=0, mu=0.001, twoway=TRUE, w=NULL)
plot(1:nrow(Pop3), Pop3[,1], ylim=c(0,1), type="l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop3), Pop3[,2], lwd=2, col="red")
legend("topleft", legend=c("a", "b"), col=c("black", "red"), lwd=2, bty="n")