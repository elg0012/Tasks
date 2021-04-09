library("phytools")
#Questions 1-3:
trees <- list()
births <- c()
Fractions <- c()
for(i in 1:100) {
	births[i] <- runif(1)
	Fractions[i] <- runif(1)
	trees[[i]] <- pbtree(b=births[i], d=(births[i]*Fractions[i]), n=100, nsim=1)
}
trees
trees[[i]]
plot(trees[[i]])
install.packages("geiger")
library("geiger")
install.packages("TreeTools")
Yes
library("TreeTools")
Yes
install.packages("rbibutils")
Yes
library("rbibutils")
tips <- sapply(trees, NTip)
logtips <- log(tips)
diversification <- sapply(trees, bd.ms)
plot(diversification, logtips, xlab="net diversification", ylab="log of total number of tips")
abline(lm(diversification~logtips), col="red")
#Question 4: The plot shows that there is a positive correlation between the net diversification and number of tips. 
cor(diversification, logtips)
#This shows positive correlation as well. 
speciation <- sapply(trees, bd.km) 
numtips <- c()
avgBL <- c()
for(i in 1:length(trees)) {
	y <- trees [[i]]
	numtips[i] <- Ntip(y)
	avgBL[i] <- mean(y$edge.length)
}
plot(speciation, avgBL, clab="speciation rate", ylab="avg branch length")
#Question 5: The plot shows that speciation and branch length are inversely proportional to each other. 
cor(speciation, avgBL)
#Question 6: The exact numerical relationship between speciation rate and average branch length is -0.1901983. 
#Question 7: 
which.max("tips")
bigTree <- trees[[66]]
plot(bigTree)
rates <- c()
traits <- list()
for(i in 1:100) { 
	rates[i] <- runif(1)
	traits[[i]] <- fastBM(tree=bigTree, sig2=rates[i])
	}
avgtrait <- sapply(traits, mean)
avgtrait
avgrate <- sapply(rates, mean)
avgrate
correlation <- cor(avgtrait, avgrate)
print(correlation)
plot(avgrate, avgtrait)
abline(lm(avgrate ~ avgtrait), col="pink")
#Question 8: The correlatio between the mean of traits and the rates is negative and its numberical value is -0.09924618.
vartraits <- sapply(traits, var)
cor(vartraits, rates)
#Question 9: The correlation between the variance of traits and rates is 0.8690232.
trait1 <- traits[1]
trait1
trait2 <- traits[2]
trait2
traitmat <- cbind(traits[[1]], traits[[2]])
traitmat
var(traitmat)
cor(traitmat[,1], traitmat[,2])
#Question 10: The correlation between the first element of traits and the second element of traits is -0.02570808. Although this number is a negative and not completely 0, I would not say it was significant due to it being such a small number.
#Extra Credit: 
?phylomorphospace
pdf("Extra Credit Plot", height=20, width=20)
phylomorphospace(bigTree, traitmat, A=NULL, xlabel="trait1", ylab="trait2")
dev.off()