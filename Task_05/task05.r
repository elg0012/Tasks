setwd("~/Desktop/Evolution/Tasks/Task_05")
library(learnPopGen)
coalescent.plot()
?coalescent.plot
#Each simulation begins with 10 alleles. This can be modified by using the function "n=part of the coalescent.plot()".
pdf("r05-Question1.pdf", height=5, width=5)
coalescent.plot(n=20, ngen=20, colors=NULL)
dev.off()

#Based on the coalescent.plot() function, 10 generations is roughly how long it takes for an allele to go to fixation. 
pdf("r05-Question2.pdf", height=5, width=5)
coalescent.plot(n=10, ngen=15, colors=NULL)
dev.off()

pdf("r05-Question3.pdf", height=5, width=5)
coalescent.plot(n=10, ngen=5, colors=NULL)
dev.off()

#According to the help page, I believe the lines represent relationships between parent and offspring. The number was 2 almost every time, meaning there is very little variance. 
#In these simulations, fitness plays the role in that the individual with the alleles needs the ability to mate a large amount for the alleles to every be distributed is such a large way. 
#Yes. The most recent common ancestor for the focal locus typically is alive in generation 0. 
install.packages("coala")
library(coala)
install.packages("phytools")
library(phytools)
?coala
?phytools
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2)+
feat_mutation(10)+
feat_recombination(10)+
sumstat_trees()+
sumstat_nucleotide_div()
model
stats <- simulate(model, nsim=1)
Diversity <- stats$pi
Diversity
#All the numbers are different due to adding mutation and recombination to the model function.
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#The number does not match the number of individuals simulated because there are two alleles for each individual. 
Agel <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
#The most recent common ancestor for this SNP is farther back than the first SNP. 
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
#They do not match. 
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees [[1]][1])
t1_2 <- read.tree(text=stats$trees [[1]][2])
compare.chronograms(t1_1, t1_2)
for(locus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for(n in 1:ntrees) {
		if (locus == 1 && n == 1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
	}
	else {
		outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
		}
	}
}
par(mfrow=c(1,1))
densityTree(outPhy)
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2)+
feat_mutation(10)+
feat_recombination(20)+
sumstat_trees()+
sumstat_nucleotide_div()
model
stats <- simulate(model, nsim=1)
Diversity <- stats$pi
Diversity
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Agel <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees [[1]][1])
t1_2 <- read.tree(text=stats$trees [[1]][2])
compare.chronograms(t1_1, t1_2)
for(locus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for(n in 1:ntrees) {
		if (locus == 1 && n == 1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
	}
	else {
		outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
		}
	}
}
par(mfrow=c(1,1))
densityTree(outPhy)
#I assumed that changing the recombination to a higher number would cause more distribution. This is what occured. 
model3 <- coal_model(10,50) +
feat_mutation(par_prior("theta", sample.int(100,1)))+
sumstat_nucleotide_div()
stats <- simulate(model3, nsim=40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi)
plot(theta)
plot(mean_pi, theta, xlab="diversity", ylab="rate of mutation", pch=16, cex=1.3, col="black")
abline(lm(mean_pi ~ theta), col="blue")

install.packages("phyclust")
library(phyclust)
activate_ms(priority=100)
activate_msms(priority=500, download=TRUE)

model5 <- coal_model(c(10, 12), loci_number=10, loci_length=800, ploidy=4)+
feat_selection(10, time=1, population=1)+
feat_selection(20, time=2, population=2)+
feat_size_change(0.5, 
population=1, 
time = "1", 
locus_group = "all")+
feat_size_change(0.10, 
population = 2, 
time="2", 
locus_group="all")+
feat_migration(1.5, 
pop_to=2,
pop_from=1, 
symmetric = FALSE, time ="3", locus_group="all")+
feat_mutation(20)+
sumstat_trees()+
sumstat_nucleotide_div()
model5
list_simulators()
stat2 <- simulate(model5, nsim=40)
Diversity <- stat2$pi
Diversity