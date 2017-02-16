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

## Quick and handy definition of standard error (for plots)
##
se <- function(data) {
  sd(data[!is.na(data)])/sqrt(length(data[!is.na(data)])-1)
}


## ------------------------------------------------------------------
## 1. BEHAVIORAL DATA ANALYSIS
## ------------------------------------------------------------------

data <- read.table("data/summary-data.txt", header=T, sep="\t")
data <- subset(data, data$ChooseA_ACC >= 0)


## 1.1 Analysis of the PSS data
## ------------------------------------------------------------------

## Reformat into canonical dataframe format
pss_data_1 <- data[c(1,2)]
pss_data_1$Condition <- "Choose"
names(pss_data_1)[2] <- "Accuracy"

pss_data_2 <- data[c(1,3)]
pss_data_2$Condition <- "Avoid"
names(pss_data_2)[2] <- "Accuracy"
pss_data <- merge(pss_data_1, pss_data_2, all=T)

# Classic lack of differences

t.test(Accuracy ~ Condition, pss_data, paired=T)
cor.test(data$AvoidB_ACC, data$ChooseA_ACC)

## 1.2  Simon Task
## ------------------------------------------------------------------

## Reformat into canonical dataframe format
simon_data_1 <- data[c(1, 5, 8)]
simon_data_1$Condition <- "Incongruent"
names(simon_data_1)[2] <- "Latency"
names(simon_data_1)[3] <- "Accuracy"

simon_data_2 <- data[c(1, 6, 9)]
simon_data_2$Condition <- "Congruent"
names(simon_data_2)[2] <- "Latency"
names(simon_data_2)[3] <- "Accuracy"

simon_data <- merge(simon_data_1, simon_data_2, all=T)

# Descriptive stats 

tapply(simon_data$Accuracy, simon_data$Condition, mean)
tapply(simon_data$Accuracy, simon_data$Condition, sd)

tapply(simon_data$Latency, simon_data$Condition, mean)
tapply(simon_data$Latency, simon_data$Condition, sd)

# Significant Simon effects

t.test(Latency ~ Condition, simon_data, paired=T)
t.test(Accuracy ~ Condition, simon_data, paired=T)

# Test on normalized accuracy data
t.test(asin(sqrt(Accuracy)) ~ Condition, simon_data, paired=T)


## 1.3 PSS Simon Correlations
## ------------------------------------------------------------------
## As predicted, AvoidB and Incongruent Simon RT are correlated.
## ------------------------------------------------------------------

## Significant effects between AvoidB and Incong. RT or Simon Effect
cor.test(data$AvoidB_ACC, data$Incongruent_SimonRT)
cor.test(data$AvoidB_ACC, data$Congruent_SimonRT)
cor.test(data$AvoidB_ACC, data$CongruencyEffect_SimonRT)

## No significant correlations with ChooseA
cor.test(data$ChooseA_ACC, data$Incongruent_SimonRT)
cor.test(data$ChooseA_ACC, data$Congruent_SimonRT)
cor.test(data$ChooseA_ACC, data$CongruencyEffect_SimonRT)


## 1.4  Accounting for WM differences
## ------------------------------------------------------------------
## WM scores measured with Unsworth's Operation Span test.
## ------------------------------------------------------------------


# Correlations between WM and Simon RTs
cor.test(data$OS_Score, data$Incongruent_SimonRT)
cor.test(data$OS_Score, data$Congruent_SimonRT)
cor.test(data$OS_Score, data$CongruencyEffect_SimonRT)


# Correlations between WM and Simon ACC
cor.test(data$OS_Score, data$Incongruent_SimonACC)
cor.test(data$OS_Score, data$Congruent_SimonACC)

# Correlations between WM and PSS
cor.test(data$OS_Score, data$AvoidB_ACC)
cor.test(data$OS_Score, data$ChooseA_ACC)

## 1.5 Partialling out WM scores 
## ------------------------------------------------------------------
## Remove WM scores through partial correlations does not alter the
## results.
## ------------------------------------------------------------------

pcor.test(data$AvoidB_ACC, data$Incongruent_SimonRT, data$OS_Score)
pcor.test(data$AvoidB_ACC, data$Congruent_SimonRT, data$OS_Score)
pcor.test(data$AvoidB_ACC, data$CongruencyEffect_SimonRT, data$OS_Score)

pcor.test(data$ChooseA_ACC, data$Incongruent_SimonRT, data$OS_Score)
pcor.test(data$ChooseA_ACC, data$Congruent_SimonRT, data$OS_Score)
pcor.test(data$ChooseA_ACC, data$CongruencyEffect_SimonRT, data$OS_Score)

## 1.6 Contrast of Correlations
## ------------------------------------------------------------------
## Contrast of correlations: Correlations with AvoidB are 
## significantly larger that correlations with ChooseA
## ------------------------------------------------------------------

paired.r(xy=cor(data$AvoidB_ACC, data$Incongruent_SimonRT), 
         xz=cor(data$ChooseA_ACC, data$Incongruent_SimonRT), n = 50)

paired.r(xy=cor(data$AvoidB_ACC, data$CongruencyEffect_SimonRT), 
         xz=cor(data$ChooseA_ACC, data$CongruencyEffect_SimonRT), n = 50)



## ------------------------------------------------------------------
## 2. BEHAVIORAL GRAPHICS (Figures 4 and 5 in the paper)
## ------------------------------------------------------------------

plot.correlation <- function(thedata=data, xvar="ChooseA_ACC", yvar="Pre_Incongruent_SimonRT", 
                             xlab="", ylab="", corpos=c(0.2, 0.2), xlim=c(0, 1), ...) {
  #par(mar=c(4,4,3,2))
  attach(thedata)
  mod <- lm(as.formula(paste(yvar, xvar, sep="~")))
  
  range <- (xlim[2] - xlim[1])
  newx <- data.frame(X=seq(xlim[1] - 1/2 *range, xlim[2] + 1/2 * range, range/1000)) 
  names(newx) <- c(xvar)  # Rename to X variable in data frame	
  newy <- predict(mod, newdata=newx, interval="confidence", level=0.95) 	
  detach(thedata)
  plot(thedata[[xvar]], thedata[[yvar]], pch=21, bg="white", col="white", cex=1.5, fg="grey15", xlab="", ylab="", col.axis="grey15", col.lab="grey15", xlim=xlim, ...)
  p <- cor.test(thedata[[xvar]], thedata[[yvar]])$p.value
  if (p < 0.05) {
    line <- "red"
    shade <- "#FF222222"
  } else {
    line <- "black"
    shade <- "#22222222"
  }
  polygon(c(newx[[xvar]], rev(newx[[xvar]])), c(newy[,2], rev(newy[,3])), col=shade, border=F)
  points(thedata[[xvar]], thedata[[yvar]], pch=21, bg="#11111166", col="white", cex=1.5)
  abline(mod, col=line, lwd=2, lty=3)
  
  r <- round(cor(thedata[[xvar]], thedata[[yvar]]), 2)
  #detach(data)
  text(x=corpos[1], y=corpos[2], col=line, labels=substitute(italic(R) == x, list(x=r)))
  mtext(ylab, side=2, line=3, col="grey15")
  mtext(xlab, side=1, line=2, col="grey15")
}



figure5 <- function() {
  par(mar=c(0,0,0,0))
  
  layout(mat = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 6, 11, 12, 13, 14), nrow = 5, byrow=F),
         widths=c(1/5, 1, 1), heights=c(1/5, 1, 1, 1, 1/5))
  plot.new()
  plot.new()
  text("Congruent Trials\nResponse Times", 
       srt=90, x=1, y=0.5, adj=c(0.5,0))
  
  plot.new()
  text("Incongruent Trials\nResponse Times", 
       srt=90, x=1, y=0.5, adj=c(0.5,0))
  
  
  plot.new()
  text("Simon Effect (ms)\n(Incongruent - Congruent)", 
       srt=90, x=1, y=0.5, adj=c(0.5,0))
  
  plot.new()
  # Title
  plot.new()
  text("Correlations between PSS Task\nand Simon Task", x=0.5, y=0.5, cex=1.5)
  
  par(mar=c(2,3,2,1))
  
  plot.correlation(xvar="ChooseA_ACC", yvar="Congruent_SimonRT", 
                   corpos=c(0.15, 600), ylim=c(200, 700))
  plot.correlation(xvar="ChooseA_ACC", yvar="Incongruent_SimonRT", 
                   corpos=c(0.15, 700), ylim=c(300, 800))
  plot.correlation(xvar="ChooseA_ACC", yvar="CongruencyEffect_SimonRT", 
                   corpos=c(0.15, 150), ylim=c(-50, 250))
  abline(h=0, lwd=1, col="darkgrey")
  
  par(mar=c(0,3,1,1))
  plot.new()
  text("Choose Accuracy", x=0.5, y=1, adj=c(0.5,1))
  
  par(mar=c(2,3,2,1))
  
  plot.correlation(xvar="AvoidB_ACC", yvar="Congruent_SimonRT", 
                   corpos=c(0.85, 600), ylim=c(200, 700))
  plot.correlation(xvar="AvoidB_ACC", yvar="Incongruent_SimonRT", 
                   corpos=c(0.85, 700), ylim=c(300, 800))
  plot.correlation(xvar="AvoidB_ACC", yvar="CongruencyEffect_SimonRT", 
                   corpos=c(0.85, 150), ylim=c(-50, 250))
  abline(h=0, lwd=1, col="darkgrey")
  
  par(mar=c(0,3,1,1))
  plot.new()
  text("Avoid Accuracy", x=0.5, y=1, adj=c(0.5,1))
  
}

#tiff("figure4.tiff", width=7, height=5, res = 150, units="in")
figure4 <- function() {
  layout(mat = matrix(c(1, 1, 1, 2, 3, 3, 4, 5, 6), nrow = 3, byrow=T),
         widths=c(1, 1, 1), heights=c(1/10, 1/10, 1))
  par(mar=c(0,0,0,0))
  plot.new()
  text("Summary of Behavioral Measures", x=0.5, y=1, adj=c(0.5,1), cex=1.5)

  par(mar=c(0,3,1,1))
  plot.new()
  text("PSS Task", x=0.5, y=1, adj=c(0.5,1), cex=1.5)
  
  plot.new()
  text("Simon Task", x=0.5, y=1, adj=c(0.5,1), cex=1.5)
  
  par(mar=c(2,4,2,2))
  
  xs <- barplot(tapply(pss_data$Accuracy, pss_data$Condition, mean), 
                ylim=c(0, 1.1), ylab="Accuracy", col=c("grey75", "grey75"),
                border="grey75")
  ys <- tapply(pss_data$Accuracy, pss_data$Condition, mean)
  ses <- tapply(pss_data$Accuracy, pss_data$Condition, se)
  arrows(xs, ys, xs, ys +ses, length = 0.1, angle=90)
  arrows(xs, ys, xs, ys - ses, length = 0.1, angle=90)
  abline(h=0, col="black")
  title(main="(A)")
  
  xs <- barplot(tapply(simon_data$Accuracy, simon_data$Condition, mean), 
          ylim=c(0, 1.1), ylab="Accuracy", col=c("grey55", "grey55"),
          border="grey55")
  ys <- tapply(simon_data$Accuracy, simon_data$Condition, mean)
  ses <- tapply(simon_data$Accuracy, simon_data$Condition, se)
  arrows(xs, ys, xs, ys +ses, length = 0.1, angle=90)
  arrows(xs, ys, xs, ys - ses, length = 0.1, angle=90)
  segments(xs[1], max(ys + ses) + 0.05, xs[2], max(ys + ses) + 0.05)
  text("*", x=mean(xs), y=max(ys + ses) + 0.05 , adj=c(0.5, 0))
  abline(h=0, col="black")
  title(main="(B)")
  
  
  xs <- barplot(tapply(simon_data$Latency, simon_data$Condition, mean), 
          ylim=c(0, 600), ylab="Response Times (ms)", col=c("grey55", "grey55"),
          border="grey55")
  
  ys <- tapply(simon_data$Latency, simon_data$Condition, mean)
  ses <- tapply(simon_data$Latency, simon_data$Condition, se)
  arrows(xs, ys, xs, ys +ses, length = 0.1, angle=90)
  arrows(xs, ys, xs, ys - ses, length = 0.1, angle=90)
  segments(xs[1], max(ys + ses) + 0.05 * 600, xs[2], max(ys + ses) + 0.05 * 600)
  text("*", x=mean(xs), y=max(ys + ses) + 0.05 * 600, adj=c(0.5, 0))
  abline(h=0, col="black")
  title(main="(C)")
  
}

## ------------------------------------------------------------------
## 3. ACT-R MODEL'S FIT TO DATA
## ------------------------------------------------------------------
## Here we examine how well the model fits the data
## ------------------------------------------------------------------

## 3.1 Define target performance
## ------------------------------------------------------------------

target <- colMeans(data[,c(9,8,6,5)])  # Target data: Cong. ACC, Incong. ACC, Cong. RT, Incong. RT
target[3:4] <- target[3:4]/1000   # Format RTs in ACT-R units (seconds)

## 3.2 Load the model simulations
## ------------------------------------------------------------------
test <- read.table('model/grid-sims/final.txt', header=F, sep=",")
names(test) <- c("Alpha", "LF", "EGS", "ANS", "Bias", "D1", "D2", "Con_ACC", "Con_RT", "Incon_ACC", "Incon_RT")
test$SimonEffect <- test$Incon_RT - test$Con_RT

## The special case where D1 == D2 == 1, representing average performance
equal <- subset(test, test$D1 == 1 & test$D2 == 1)

## Error function 1: Minimize distance from the four base values
error <- function(a, b, c, d) {
  ta <- target[1]
  tb <- target[2]
  tc <- target[3]
  td <- target[4]
  
  vec <- 100 * c(mean(c(a,b)), mean(c(c,d)), a - b, c - d)
  tvec <- 100 * c(mean(c(ta,tb)), mean(c(tc,td)), ta - tb, tc - td)
  
  sum((vec - tvec) ** 2)
}

verror <- Vectorize(error)

equal$Error <- verror(equal$Con_ACC, equal$Incon_ACC,
                      equal$Con_RT, equal$Incon_RT)



## Error function 2: Minimize distance from congruent conditions
## and effects.
error2 <- function(a, b, c, d) {
  ta <- target[1]
  tb <- target[2]
  tc <- target[3]
  td <- target[4]
  
  vec <- 100 * c(a, c, a - b, c - d)
  tvec <- 100 * c(ta, tc, ta - tb, tc - td)
  
  sum((vec - tvec) ** 2)
}


verror2 <- Vectorize(error2)

equal$Error2 <- verror2(equal$Con_ACC, equal$Incon_ACC,
                        equal$Con_RT, equal$Incon_RT)


# Both criteria converge on the same set of parameters
model <- subset(equal, equal$Error == min(equal$Error))
model <- subset(equal, equal$Error2 == min(equal$Error2))



# Generates a random factor
model$Run <- paste("Run", 1:dim(model)[1])

## ------------------------------------------------------------------
## 3.2 Comparison of model and data 
## ------------------------------------------------------------------

model_1 <- model[c("Con_RT", "Con_ACC", "Run")]
model_1$Condition <- "Congruent"
names(model_1)[1] <- "Latency"
names(model_1)[2] <- "Accuracy"

model_2 <- model[c("Incon_RT", "Incon_ACC", "Run")]
model_2$Condition <- "Incongruent"
names(model_2)[1] <- "Latency"
names(model_2)[2] <- "Accuracy"

model_comp <- merge(model_1, model_2, all=T)
names(model_comp)[3] <- "Subject"
model_comp$Source <- "Model"

model_comp$Latency <- 1000 * model_comp$Latency

simon_comp <- simon_data
simon_comp$Source <- "Data"
comp <- merge(model_comp, simon_comp, all=T)

## RMSEs of model fit
## ------------------

rt.diff <- tapply(comp$Latency, list(comp$Condition, comp$Source), mean)
sqrt(mean((rt.diff[,1] - rt.diff[,2])**2))

acc.diff <- tapply(comp$Accuracy, list(comp$Condition, comp$Source), mean)
sqrt(mean((acc.diff[,1] - acc.diff[,2])**2))

## Chi-squared tests of fit
## ------------------------

## Accuracy is binomial, we can calculate the values directly. 
chisq.test(200 * acc.diff)

## in case of RTs, we need to use the test for normal distributions
rt.var <- tapply(comp$Latency, list(comp$Condition, comp$Source), var)
X2 <- sum(((rt.diff[,1] - rt.diff[,2]) ** 2) / (rt.var[,1] ** 2)) 
1 - pchisq(X2,1)



## --------------------------------------------------------------- ##
## 3.3 Calculating the Correlations between D2/D1 and Simon effects 
## --------------------------------------------------------------- ##


nd1 <- length(unique(test$D1))
nd2 <- length(unique(test$D2))


con <- c(rep("D1", nd1), rep("D2", nd2))
test$Cond <- con

test$Cond[test$D2 != 1] <- "D2"
test$Cond[test$D1 != 1] <- "D1"

calculate.r <- function(data, var1, var2, alpha, lf, egs, ans, bias) {
  subset <- subset(data, data$Alpha == alpha & data$LF == lf & data$EGS == egs & data$ANS == ans & data$Bias == bias)
  cor(round(subset[[var1]], 2), round(subset[[var2]], 2))
}

d1test <- subset(test, test$Cond == "D1")
d2test <- subset(test, test$Cond == "D2")


d1test$Con_RT <- round(d1test$Con_RT, 2)
d2test$Con_RT <- round(d2test$Con_RT, 2)
d1test$Incon_RT <- round(d1test$Incon_RT, 2)
d2test$Incon_RT <- round(d2test$Incon_RT, 2)
d1test$SimonEffect <- round(d1test$SimonEffect, 2)
d2test$SimonEffect <- round(d2test$SimonEffect, 2)


space <- aggregate(test[c("Con_RT")], list(Alpha = test$Alpha,
                                           EGS = test$EGS,
                                           ANS = test$ANS,
                                           LF = test$LF,
                                           Bias = test$Bias),
                   length)


space$r_D2_Inc <- 0
space$r_D2_Eff <- 0



for (a in unique(space$Alpha)) {
  for (l in unique(space$LF)) {
    for (e in unique(space$EGS)) {
      for (n in unique(space$ANS)) {
        for (b in unique(space$Bias)) {
          space$r_D2_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d2test, "D2", "Incon_RT", a, l, e, n, b)
          space$r_D2_Eff[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d2test, "D2", "SimonEffect", a, l, e, n, b)
        }
      }
    }
  }
} 

space$r_D2_Inc[is.na(space$r_D2_Inc)] <- 0
space$r_D2_Eff[is.na(space$r_D2_Eff)] <- 0


space$diff_d2_r <- space$r_D2_Inc - space$r_D2_Con
space$diff_d2_r[is.na(space$diff_d2_r)] <- 0


# Re-instantiate the correct, non-approx values for d1test and d2test
d1test <- subset(test, test$Cond == "D1")
d2test <- subset(test, test$Cond == "D2")

## To get stable estimates, we need to fix the parameters to their best values

d1_optimal <- subset(d1test, d1test$Alpha == 0.3 & d1test$LF == 0.25 & d1test$EGS<= 0.2 & d1test$ANS==0.2 & d1test$Bias >= 1)
d1_optimal <- aggregate(d1_optimal[c("Incon_RT", "Con_RT")], list(D1=d1_optimal$D1), mean)
d2_optimal <- subset(d2test, d2test$Alpha == 0.3 & d2test$LF == 0.25 & d2test$EGS<= 0.2 & d2test$ANS==0.2 & d2test$Bias >= 1)
d2_optimal <- aggregate(d2_optimal[c("Incon_RT", "Con_RT")], list(D2=d2_optimal$D2), mean)



## ------------------------------------------------------------------
## 4. GRAPHICS OF MODEL FIT (Figures 7 and 8 in the paper)
## ------------------------------------------------------------------

#tiff("figure7.tiff", width=6, height=5, units = "in", res = 150)
figure7 <- function() {
  layout(mat = matrix(c(1, 1, 2, 3), nrow = 2, byrow=T),
         widths=c(1, 1), heights=c(1/10, 1))
  
  par(mar=c(0,4,0,2))
  plot.new()
  text("Comparison of Model and Data", x=0.5, y=1, adj=c(0.5,1), cex=1.5)
  
  
  par(mar=c(2,4,2,2))
  
  xs <- barplot(tapply(comp$Accuracy, list(comp$Condition, comp$Source), mean), 
                ylim=c(0, 1.15), ylab="Accuracy", col=c("grey75", "grey55"), 
                beside=T, legend=T, args.legend=list(x="topleft", bty="n", border="white"),
                border="white")
  ys <- tapply(comp$Accuracy, list(comp$Condition, comp$Source), mean)
  ses <- tapply(comp$Accuracy, list(comp$Condition, comp$Source), se)
  arrows(xs, ys, xs, ys + ses, length = 0.1, angle=90)
  arrows(xs, ys, xs, ys - ses, length = 0.1, angle=90)
  abline(h=0, col="black")
  title(main="(A) Accuracy")
  
  xs <- barplot(tapply(comp$Latency, list(comp$Condition, comp$Source), mean), 
                ylim=c(0, 650), ylab="Response Times (ms)", col=c("grey75", "grey55"), 
                beside=T, legend=T, args.legend=list(x="topleft", bty="n", border="white"),
                border="white")
  ys <- tapply(comp$Latency, list(comp$Condition, comp$Source), mean)
  ses <- tapply(comp$Latency, list(comp$Condition, comp$Source), se)
  arrows(xs, ys, xs, ys + ses, length = 0.1, angle=90)
  arrows(xs, ys, xs, ys - ses, length = 0.1, angle=90)
  abline(h=0, col="black")
  title(main="(B) Response Times")
}


cor.plot <- function(y1, y2) {
  x <- seq(0.5, 1.5, 0.125)
  
  plot.new()
  plot.window(c(0.4, 1.6), c(440, 500))
  axis(1, at = seq(0.5, 1.5, 0.25))
  axis(2, at = seq(440, 500, 10))
  
  lines(x, y1, type="l", lty=1, col="grey35")
  points(x, y1, pch=21, bg="grey35", cex=1.5, col="grey35")
  m <- lm(y1 ~ x)
  abline(m$coefficients[1], m$coefficients[2], col="grey25", lty=3, lwd=2)
  
  lines(x, y2, type="l", lty=1, col="grey75")
  points(x, y2, pch=21, bg="grey75", cex=1.5, col="grey75")
  m <- lm(y2 ~ x)
  abline(m$coefficients[1], m$coefficients[2], col="grey55", lty=3, lwd=2)
  box(bty="o")
  
  legend(x="topright", legend=c("Congruent", "Incongruent"), 
         pch=21, lty=1, bty="n", pt.bg =c("grey75", "grey35"), col =c("grey75", "grey35"))
}


figure8 <- function() {
  layout(matrix(c(1,2), ncol=2))
  cor.plot(d1_optimal$Incon_RT*1000, d1_optimal$Con_RT * 1000)
  title(xlab=expression(italic(d)[1]), ylab = "Response times (ms)",
        main=expression(bold(paste("(A) Effects of ", italic(d)[1], " on Simon Task"))))
  
  
  cor.plot(d2_optimal$Incon_RT*1000, d2_optimal$Con_RT * 1000)
  title(xlab=expression(italic(d)[2]), ylab = "Response times (ms)",
        main=expression(bold(paste("(B) Effects of ", italic(d)[2], " on Simon Task"))))
  
}

## --------------------------------------------------------------- ##
## 5.  PARAMETER SPACE PARITIONING FOR SIMON TASK MODEL SIMULATIONS
## --------------------------------------------------------------- ##


approx.equal <- function(x, y) {
  if (abs(x-y)/abs(min(c(-0.00001,x,y))) < 0.02) {
    T
  } else {
    F
  }
}

approx.order <- function(x, y) {
  if (approx.equal(x, y)) {
    0
  } else {
    sign(x - y)
  }
}

pairing <- function(x, y) {
  3 * x + y
}

unpairing <- function(z) {
  y <- z %% 3
  x <- (z - y) /3
  c(x, y)
}

pattern <- function(a, b, c, d) {
  p1 <- 1 + approx.order(a, b)  # Accuracies
  p2 <- 1 + approx.order(c, d)  # RTs
  # now p1 and p2 are in [0,1,2]. We can pair them
  pairing(p1, p2)
}

vpattern <- Vectorize(pattern)


test$Pattern <- 0
test$Run <- 1:dim(test)[1]
test$Pattern <- vpattern(test$Con_ACC, test$Incon_ACC,
                         test$Con_RT, test$Incon_RT)

equal <- subset(test, test$D1 == 1 & test$D2 == 1)


# Final stats for PSP: Main effects
# ---------------------------------

psp <- tapply(equal$Con_RT, equal$Pattern, length)
psp <- psp /  (dim(equal)[1])

rpsp <- round(psp*100, 1)


## PSP FOR CORRS

cor.pattern <- function(a, b) {
  p1 <- 1 + sign(round(a,2)) # D1 Con vs. D1 Incon / D1 Eff vs D2 Eff
  p2 <- 1 + sign(round(b,2)) # D1 Con vs D2 Incon
  paste(p1, p2, sep="")
}


vcor.pattern <- Vectorize(cor.pattern)

space$CorPattern <-0
space$CorPattern <- vcor.pattern(space$r_D2_Eff, space$r_D2_Inc)

cpsp <- tapply(space$Alpha, space$CorPattern, length)

rcpsp <- round(100 * cpsp / sum(c(cpsp)), 1)



figure9 <- function() {
  layout(matrix(c(1,2), ncol=2))
  pie(rpsp, col=rev(grey.colors(3)), labels = c("Correct\npattern\n(99.8%)", "", ""), border=NA, edges=2000, radius=1, lwd=2)
  title(main="(A) PSP: Group averages")
  
  pie(rcpsp, col=rev(grey.colors(9)), border="white", edges = 2000, radius = 1,
      labels = c("Correct\npattern\n(76.0%)", rep("", 8)))
  title(main="(B) PSP: Correlations with PSS")
}