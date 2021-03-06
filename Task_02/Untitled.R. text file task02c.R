setwd('~/Desktop/Evolution/Tasks/Task_02')
unique(beren3$event)
Hypothesis: I believe that the amount of solid food that Beren eats will increase and correlate with his increase in age.
Feeds <- which(beren3$event == "solids")
avgSolid <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenAnova <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the solid food", ylab = "amount of solid food consumed (pcs)" )
?par
par(las=1, mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="pieces of solid food")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02c-totalSolidFoodByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="pieces of solid food")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()