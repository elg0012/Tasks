setwd("~/Desktop/Evolution/Tasks/Task_08")
library(phytools)
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
tree$tip.label
#QUESTION 1: There are 82 tips, and branched lengths are present. 
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
data
data[,1]
#QUESTION 2: "Data" contains species of lizards and their snout-vent length and includes 100 dimensions. 
svl <- setNames(data$svl, rownames(data))
svl
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc
#QUESTION 3: The estimated values are stored in a list (ace). The CI95 element is the confidence interval. 
#QUESTION 4: Two assumptions made in the estimation of the ancestral states using fastAnc are 1) that the state calculated for the root node of the tree is also the MLE of the root node, and that variance is the contrast state. 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7,0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
fossilData
#QUESTION 5: Shown below 
fossilNodes <- c()
nodeN <- c()
{
	for(i in 1:nrow(fossilData))
	i <- 1
	if(i == 1){
		print(Ancestors)
	}
}
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
Node
fossilNodes[i] <- fossilData[i, "svl"]
fossilNodes[i]
nodeN[i] <- Node
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_withoutFossils <- fastAnc(tree, svl, CI=TRUE, var=TRUE)
Ancestors_withoutFossils
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace, xlab="With Fossils", ylab="Without Fossils")
#QUESTION 7: The data for "With Fossils" increase the estimated ancestral sizes. 
#QUESTIONS 8-10: Shown below
install.packages("geiger")
library("geiger")
?fitContinuous
?geiger
fitContinuous(tree, svl, model="BM")
fitContinuous(tree, svl, model="OU")
fitContinuous(tree, svl, model="EB") 
#FastAnc uses the function including "BM" to decide on what it is going to assume is correct. In this case, the function including "EB" has the AIC meaning best fit. 