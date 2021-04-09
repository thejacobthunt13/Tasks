library('phytools')
#QUESTIONS 1-3
trees <- list()
births <- c()
Fractions <- c()
for(i in 1:100){
	births[i] <- runif(1)
	Fractions[i] <- runif(1)
	trees[[i]] <- pbtree(b=births[i], d=(births[i]* Fractions[i]), n=100, nsim=1)
	}
	trees
	trees[[i]]
	plot(trees[[i]])
	#?pbtree
	library('geiger')
	#?sapply
	#QUESTION 4
	install.packages('TreeTools')
	library('TreeTools')
	#Y
	tips <- sapply(trees,NTip)
	logtips <- log(tips)
	diversification <- sapply(trees, bd.ms)
	plot(diversification, logtips, xlab='log of total number of tips')
	abline(lm(diversification~logtips), col='red')
	# the graph shows a strong positive correlation between diversification and number of tips, and I expected this.
	cor(diversification, logtips)
	#positive correlation cor=0.2555332
	#QUESTION 5
	speciation <- sapply(trees, bd.km)
	#for (t in 1:length(trees)) {
		i <- 1
		numtips <- c()
		avgBL <- c()
		for(i in 1:length(trees)) {
			# choose tree
			y <- trees[[i]]
			# find number of tips
			numtips[i] <- Ntip(y)
						# find average
			branch length 
			avgBL[i] <- mean(y$edge.length) 
			}
			plot(speciation, avgBL, xlab='speciation rate', ylab='average branch length')
			#QUESTION 6
			cor(speciation, avgBL)
			#QUESTION 7
			which.max(tips)
			bigTree <- trees[[66]]
			plot(bigTree)
		rates <- c()	
		traits <- list()	
		for(i in 1:100) {
			rates[i] <- runif(1)
			traits[[i]] <- fastBM(tree = bigTree, sig2 = rates[i])
			}
			#QUESTION 8
			avgtrait <- sapply(traits, mean)
			avgrate <- sapply(rates, mean)
			avgrate
			correlation <- cor(avgtrait, avgrate)
			print(correlation)
			plot(avgrate~avgtrait)
			abline(lm(avgrate~avgtrait), col='purple')
			#simulation was -0.05422387
			#QUESTION 9
			vartraits <- sapply(traits, var)
			cor(vartraits, rates)
			#There is a positive correlation between variance of traits and rates, which woulld make sense because with the increase of rates, we would see a higher variation between traits.
			#QUESTION 10
			trait1 <- traits[1]
			trait1
			trait2 <- traits[2]
			trait2
			traitmat <- cbind(traits[[1]], traits[[2]])
			traitmat
			var(traitmat)
			cor(traitmat[,1], traitmat[,2])
			# The correlation is near zero, so I wouldn't consider it significant.
			plot(traitmat[,1], traitmat[,2]
			abline(lm(traitmat[,1]~traitmat[,2]), col='pink')