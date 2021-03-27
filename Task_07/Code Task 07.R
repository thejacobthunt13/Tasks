library("phytools")
install.packages("phytools")
install.packages("ape")
text.string <- 
"(((((((cow, pig), whale),(bat,(lemur,human))), (robin,iguana)),coelacanth),(gold_fish, trout)),shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(fram="circle", bg='white', cex=1)
# QUESTION 1: shark
vert.tree
# QUESTION 2 Yes
str(vert.tree)
tree <- read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
# QUESTION 3 no.tip.labels
# QUESTION 4 type=radial
# QUESTION 5 tiplabels col="red"
# QUESTION 6
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
n <- length(AnolisTree$tip.label)
ee <- setNames
(AnolisTree$edge.length[sapply(1:n,function(x,y)which(y==x), y=AnolisTree$edge[,2])],AnolisTree$tip.label)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
fit.bd(tree)




