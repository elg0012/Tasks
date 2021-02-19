trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

Size <- 50 
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)

Sample1
Sample2

#The populations are different in that sample 1 has 49 objetcs, and sample 2 has 46 objects. 
boxplot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")

MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

head(MatGrandma)
head(MatGrandpa)
head(PatGrandma)
head(PatGrandpa)

Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)

Focus <- makeBaby(Brenda, Alan)

#The number of the ToMom obhect should be 50% or 0.50. 

ToMom <- length (grep("mom", Focus))/length(Focus)
head(ToMom)
#These numbers should be 25% 05 0.25. 

ToMomMom <- length(grep("grandma_mom", Focus))/length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)
head(ToMomMom)
head(ToMomDad)
#These numbers did not match my expectation exactly, but they do add up to 0.5 which I guess was somewhat my expectation. 
ToDadMom <- length(grep("grandma_da", Focus))/length(Focus)
ToDadDad <- length(grep("grandma_da", Focus))/length(Focus)
head(ToDadMom)
head(ToDadDad)
#Focus is not equally related to each maternal grandparent, but it is equally related to each paternal granparent. I did not really expect these numbers to be this way. I would have expected each one to be 0.25. Although they were relatively close to 0.25, they were not exactly what I expected. The average relatedness of Focus to all four grandparents is 0.2829.
Sibling_01 <- makeBaby(Brenda, Alan)
#I expect about 0.50-0.75 of Focus DNA to be shared with Sibling_01. The actual amount shared is 0.5332 which is within the boundaries of what I expected but definitely on the lower side. 
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
head(ToSib)
#I would expect Focus to share around the same amount of DNA with all of its siblings, so 0.5332.
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
head(ManySiblings)
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#The main explanation as to why we see a range of value in these analyses is that not the exact same DNA was passed to Focus's siblings as there was passed to him/her. DNA overall changes with every sibling even though some aspects might be the same.

HWE <- function(p) {
	aa <- p^2
	ab <- 2*p*(1-p)
	bb <- (1-p)^2
	return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)

plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#This plot makes sense to me and is easy to understand based on things we have covered in class. The frequency of aa individuals increases as the frequency of the a allele increases within the population. The aa allele decreases as a decreases. Neither time nor geographic space are shown on this plot, and the units on the graph are represented as decimals or percentages. 
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa","ab","bb"), col=c("red","purple","blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#Yes. The frequency of the aa genotype in this population matches the expectation from Hardy-Weinberg. 
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#Due to the population not being as large, the overall frequency has increased. This could be because the smaller the population size, the less the frequency of b is likely appear. 

install.packages('learnPopGen')
library('learnPopGen')
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
Samples
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
tExt
Line <- lm(tExt ~Samples)
Line
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
abline
Line2 <- lm(tExt~Samples +0)
Line2
#As the population size increases, it seems as if the points become farther away from the line. This means that as the population size increases, more variation is introduced. 
#Extra Credit
install.packages("lmtest")
install.packages("sandwich")
summary(Line)
bptest(Line)
bptest(Line2)
coeftest(Line, vcov=vcovHC(Line))
install.packages("robustbase")
Linerob <- lmrob(tExt~Samples)
summary(Linerob)
plot(Samples, tExt)
abline(Line)
abline(Linerob)
pdf("r03ec-RobustHeteroskedasticity.pdf", height = 5, width = 5)
plot(Samples, tExt)
abline(Line, col="red")
abline(Linerob, col="blue")
legend(x=20, y=400, legend=c("Robust", "Linear"), col=c("blue", "red"), lwd=1.2,, cex=0.8)
dev.off()