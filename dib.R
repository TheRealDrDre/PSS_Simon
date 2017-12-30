## ------------------------------------------------------------------
## Analysis of Behavioral and Simulation Data from the PSS/Simon
## Experiment, published as:
## 
##   Stocco, A., Murray, N. L., Yamasaki, B. L., Renno, T. J., 
##   Nguyen, J., & Prat, C. S. "Individual differences in the Simon
##   effect are underpinned by differences in competitive dynamics 
##   in the basal ganglia: An experimental verification and a 
##   computational model". Cognition, 2017.
##
## ------------------------------------------------------------------


library(Rmisc)
library(psych)
library(ppcor)

d <- read.table("data/summary-data.txt", header=T)

d <- subset(d, d$ChooseA_ACC > 0)

tiff("dibFig4.tiff", res=300, width=4, height=4, units="in")
hist(d$OS_Score, col="grey", border="white", xlab = "Span score", ylab="Count", main="Distribution of Span Scores")
abline(v=mean(d$OS_Score, na.rm=T), lwd=2, lty=2)
abline(h=0)
box(bty="o")
dev.off()

# PSS

choose <- d[c("ChooseA_ACC")]
names(choose) <- c("Accuracy")
choose$Measure <- "Choose"

avoid <- d[c("AvoidB_ACC")]
names(avoid) <- c("Accuracy")
avoid$Measure <- "Avoid"

pss <- merge(choose, avoid, all=T)

tiff("dibFig1.tiff", res=300, width=4, height=4, units="in")
boxplot(Accuracy ~ Measure, pss, col="grey85", border="grey25", main="Summary of PSS", ylab="Accuracy", xlab="Measure")
dev.off()

# Simon, RT

con <- d[c("Congruent_SimonRT")]
names(con) <- c("RT")
con$Type <- "Congruent"

incon <- d[c("Incongruent_SimonRT")]
names(incon) <- c("RT")
incon$Type <- "Incongruent"

simonrt <- merge(con, incon, all=T)

tiff("dibFig2.tiff", res=300, width=4, height=4, units="in")
boxplot(RT ~ Type, simonrt, col="grey85", border="grey25", main="Summary of Simon task\n(Response Times)", ylab="Response Times (seconds)", xlab="Trial Type", ylim=c(0,800))
dev.off()

# Simon, ACC

con <- d[c("Congruent_SimonACC")]
names(con) <- c("Accuracy")
con$Type <- "Congruent"

incon <- d[c("Incongruent_SimonACC")]
names(incon) <- c("Accuracy")
incon$Type <- "Incongruent"

simonacc <- merge(con, incon, all=T)

tiff("dibFig3.tiff", res=300, width=4, height=4, units="in")
boxplot(Accuracy ~ Type, simonacc, col="grey85", border="grey25", main="Summary of Simon task\n(Accuracies)", ylab="Response Times (seconds)", xlab="Trial Type", ylim=c(0,1))
dev.off()
