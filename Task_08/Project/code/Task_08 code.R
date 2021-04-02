setwd('~/Desktop/Evolution/Tasks/Task_08')
library('phytools')
tree <- read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree, type='fan')
tree$tip.label
#QUESTION 1: 82 tips, branch lengths are present, producing 161.
tree$edge.length
data <- read.csv('https://jonsmitchell.com/data/svl.csv', stringsAsFactors=F, row.names=1)
data
data[,1]
#QUESTION 2: There are 100 dimensions in data, and it is a list of each species of lizard and their snout-vent length.
svl <- setNames(data$svl, rownames(data))
svl
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
# QUESTION 4:by taking advantage that the sate computed for the root node of the tree contrasts algorithm, and computes variances or 95 percent confidence intervals
Ancestors
fastAnc
#QUESTION 3: the values are stored in the tips of the tree, and CI95 element can compute  95% confidence intervals on ancestral state estimates.
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
fossilNodes <- c()
nodeN <- c()
{
	# QUESTION 5
	for(i in 1:nrow(fossilData))
	i <- 1 if(i == 1) {
		print(Ancestors) }
		}
		fossilNodes <- c()
		nodeN <- c()
		Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
		fossilNodes[i] <- fossilData[i, "svl"]
		nodeN[i] <- Node
		names(fossilNodes) <- nodeN
		Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
		install.packages('geiger')
		fitContinuous(phy, dat, SE = 0,
		model = c("BM","OU","EB","rate_trend","lambda","kappa","delta","mean_trend","white"),
		bounds=list(), control = list(method =c("subplex","l-BFGS-B"),
		niter = 100, FAIL = 1e+200, hessian = FALSE, CI = 0.95), ncores=NULL, ...)
		fitContinuous <- model = c("BM","OU","EB","rate_trend","lambda","kappa","delta","mean_trend","white"),
		data(geospiza)
		aic.all <- cbind(aic.brown, aic.lambda, aic.delta, aic.kappa, aic.ou, aic.eb)
		#QUESTION 7: By comparing the anatomies of both modern and extinct species
		# QUESTION 8: It holds true
		# QUESTION 9: fit Continuous
		# QUESTION 10: yes it is different
