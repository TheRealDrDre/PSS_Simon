# Damn
library(Rmisc)
library(matlab)
library(pwr)
test <- read.table('grid-sims4/final.txt', header=F, sep=",")
names(test) <- c("Alpha", "LF", "EGS", "ANS", "Bias", "D1", "D2", "Con_ACC", "Con_RT", "Incon_ACC", "Incon_RT")
test$SimonEffect <- test$Incon_RT - test$Con_RT

target <- c(0.98, 0.88, 0.421, 0.489)  # Target data


## --------------------------------------------------------------- ##
## PARAMETER SPACE PARITIONING FOR SIMON TASK DATA
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


#for (r in unique(test$Run)) {
#  print(r)
#  z <- subset(test, test$Run == r)
#  a <- z$Con_ACC
#  b <- z$Incon_ACC
#  c <- z$Con_RT
#  d <- z$Incon_RT
#  test$Pattern[test$Run == r] <- pattern(a, b, c, d)
#}

#plot.multid <- function() {
#  colors <- rainbow(9)
#  plot.new()
#  plot.window(c(0,1), c(0,1.2))
#  axis(1)
#  axis(2)
#  for (r in 1:300) {
    #print(r)
#    z <- subset(test, test$Run == r)
#    x = c(0, 1:6 / 6)
#    y = c(z$Alpha, z$LF, z$EGS, z$ANS, z$Bias/10, z$D1/2, z$D2/2)
#    lines(x=x, y=y, col="red", lwd=0.2)
#  }
#  text(x=x, y = rep(1.1, 7), labels = names(test)[1:7])
#}


s1 <- tapply(test$Pattern, list(EGS = test$EGS, LF = test$LF, ANS=test$ANS), median)
s2 <- tapply(test$Pattern, list(Alpha = test$Alpha, Bias = test$Bias), median)
s3 <- tapply(test$Pattern, list(D1 = test$D1, D2  = test$D2), median)

# Final stats for PSP
# -------------------

equal <- subset(test, test$D1 == 1 & test$D2 == 1)
psp <- tapply(equal$Con_RT, equal$Pattern, length)
psp <- psp /  (dim(equal)[1])

rpsp <- round(psp*100, 1)
pie(rpsp, labels = paste(c("Incong > Cong", "Incong = Cong", "Incong < Cong"), 
                         paste(rpsp, "%", sep="")), col=grey(rev(1:3/4)), 
    border=NA, edge=400, radius=1, lwd=2)
title(main="Parameter Space Partitioning")

# Model flexbility graph

N = 100
x <- (1:N)/N
y <- (1:N)/N

m <- outer(x,y)

# Create matrix image

for (i in x) {
  for (j in y) {
    z <- subset(test, test$Con_RT <= i & test$Con_RT > (i - 1/N) 
                & test$Incon_RT <= j & test$Incon_RT > (j - 1/N))
    #print(dim(z[1]))
    m[i*N,j*N] <- dim(z)[1]
  }
}


m[m<1] <- 0

image(m, col=jet.colors(20))
points(x=target[3], y=target[4], col="white", lwd=2, pch=4)
grid()
title(main="Model Flexibility: Response Times", xlab="Congruent trials (secs)", ylab="Incongruent trials (secs)")
box(bty="o")


# Greyscale version
# -----------------
image(m, col=grey(20:5/20))
points(x=target[3], y=target[4], col="red", lwd=2, pch=4)
grid()
title(main="Model Flexibility: Response Times", xlab="Congruent trials (secs)", ylab="Incongruent trials (secs)")
box(bty="o")


# Better Greyscale version
# ------------------------

levelplot(m, col.regions=gray(20:1/20))
points(x=target[3], y=target[4], col="red", lwd=2, pch=3)
grid()
title(main="Model Flexibility: Response Times", xlab="Congruent trials (secs)", ylab="Incongruent trials (secs)")
box(bty="o")


# Flexibility p-value
length(m[m>=1]) / (N**2)


## ACCURACIES
## ==========

macc <- outer(x,y)

# Create matrix image
# -------------------

for (i in x) {
  for (j in y) {
    z <- subset(test, test$Con_ACC <= i & test$Con_ACC > (i - 1/N) 
                & test$Incon_ACC <= j & test$Incon_ACC > (j - 1/N))
    #print(dim(z[1]))
    macc[i*N,j*N] <- dim(z)[1]
  }
}

macc[macc<1] <- 0
image(macc, col=jet.colors(20))
grid()
title(main="Model Flexibility: Accuracy", xlab="Congruent trials", ylab="Incongruent trials")
box(bty="o")

# Greyscale version
# -----------------
image(macc[50:100, 50:100], col=grey(20:5/20))
grid()
points(x=target[1], y=target[2], col="red", lwd=2, pch=4)
title(main="Model Flexibility: Accuracy", xlab="Congruent trials (secs)", ylab="Incongruent trials (secs)")
box(bty="o")


# Flexibility p-value
length(macc[macc>=1]) / (N**2)

## --------------------------------------------------------------- ##
## ERROR FUNCTION AND FIT OPTIMIZATION
## --------------------------------------------------------------- ##


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


#plot(test$Con_ACC, test$Incon_ACC, xlim=c(0,1), ylim=c(0,1))
#plot(test$Con_RT, test$Incon_RT, xlim=c(0,1), ylim=c(0,1))


#plot(equal$Con_ACC, equal$Incon_ACC, xlim=c(0,1), ylim=c(0,1), col="#33333399")
##points(x=c(0.98), y=c(0.88), col="red", pch=3)
#plot(equal$Con_RT, equal$Incon_RT, xlim=c(0,1), ylim=c(0,1), col="#33333399")
#points(x=c(0.421), y=c(0.489), col="red", pch=3)

subset(equal, equal$Error2 == min(equal$Error2))

## --------------------------------------------------------------- ##
## CORRELATIONS
## --------------------------------------------------------------- ##


nd1 <- length(unique(test$D1))
nd2 <- length(unique(test$D2))


con <- c(rep("D1", nd1), rep("D2", nd2))
test$Cond <- con

test$Cond[test$D2 != 1] <- "D2"
test$Cond[test$D1 != 1] <- "D1"

z <- subset(test, test$Cond == "D1")
unique(z$D2)
z <- subset(test, test$Cond == "D2")
unique(z$D1)

calculate.beta <- function(data, var1, var2, alpha, lf, egs, ans, bias) {
  subset <- subset(data, data$Alpha == alpha & data$LF == lf & data$EGS == egs & data$ANS == ans & data$Bias == bias)
  m <- lm(subset[[var1]] ~ subset[[var2]])
  m$coefficients[2]
}

calculate.r <- function(data, var1, var2, alpha, lf, egs, ans, bias) {
  subset <- subset(data, data$Alpha == alpha & data$LF == lf & data$EGS == egs & data$ANS == ans & data$Bias == bias)
  cor(round(subset[[var1]], 2), round(subset[[var2]], 2))
}
#vcalcbeta <- Vectorize(calculate.beta)

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


space$beta_D1_Inc <- 0
space$beta_D2_Inc <- 0
space$beta_D1_Con <- 0
space$beta_D2_Con <- 0

space$r_D1_Inc <- 0
space$r_D2_Inc <- 0
space$r_D1_Con <- 0
space$r_D2_Con <- 0
space$r_D1_Eff <- 0
space$r_D1_Eff <- 0



for (a in unique(space$Alpha)) {
  for (l in unique(space$LF)) {
    for (e in unique(space$EGS)) {
      for (n in unique(space$ANS)) {
        for (b in unique(space$Bias)) {
          #space$beta_D1_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d1test, "D1", "Incon_RT", a, l, e, n, b)
          #space$beta_D2_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d2test, "D2", "Incon_RT", a, l, e, n, b)
          #space$beta_D1_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d1test, "D1", "Con_RT", a, l, e, n, b)
          #space$beta_D2_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d2test, "D2", "Con_RT", a, l, e, n, b)
          space$r_D1_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d1test, "D1", "Incon_RT", a, l, e, n, b)
          space$r_D2_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d2test, "D2", "Incon_RT", a, l, e, n, b)
          space$r_D1_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d1test, "D1", "Con_RT", a, l, e, n, b)
          space$r_D2_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d2test, "D2", "Con_RT", a, l, e, n, b)
          space$r_D1_Eff[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d1test, "D1", "SimonEffect", a, l, e, n, b)
          space$r_D2_Eff[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d2test, "D2", "SimonEffect", a, l, e, n, b)
          
        }
      }
    }
  }
} 

space$r_D1_Con[is.na(space$r_D1_Con)] <- 0
space$r_D1_Inc[is.na(space$r_D1_Inc)] <- 0
space$r_D2_Con[is.na(space$r_D2_Con)] <- 0
space$r_D2_Inc[is.na(space$r_D2_Inc)] <- 0
space$r_D1_Eff[is.na(space$r_D1_Eff)] <- 0
space$r_D2_Eff[is.na(space$r_D2_Eff)] <- 0


space$diff_d2_r <- space$r_D2_Inc - space$r_D2_Con
space$diff_d2_r[is.na(space$diff_d2_r)] <- 0


nonoise <- subset(space, space$EGS==0)

# Re-instantiate the correct, non-approx values for d1test and d2test
d1test <- subset(test, test$Cond == "D1")
d2test <- subset(test, test$Cond == "D2")


d1_optimal <- subset(d1test, d1test$Alpha == 0.3 & d1test$LF == 0.25 & d1test$EGS== 0.1 & d1test$ANS==0.2 & d1test$Bias == 10)
plot(d1_optimal$D1, d1_optimal$Incon_RT)

d2_optimal <- subset(d2test, d2test$Alpha == 0.3 & d2test$LF == 0.25 & d2test$EGS== 0.1 & d2test$ANS==0.2 & d2test$Bias ==10)
plot(d2_optimal$D2, d2_optimal$Incon_RT)



plot(d2_optimal$D2, d2_optimal$Incon_RT, type="l", ylim = c(0.450,0.52))
points(d2_optimal$D2, d2_optimal$Incon_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Incon_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Incon_RT, pch=21, bg="white")

plot(d2_optimal$D2, d2_optimal$SimonEffect, type="l", ylim = c(0, 0.05))
points(d2_optimal$D2, d2_optimal$SimonEffect, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$SimonEffect, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$SimonEffect, pch=21, bg="white")


#
plot(d2_optimal$D2, d2_optimal$Incon_RT, type="l", ylim = c(0.440,0.51))
points(d2_optimal$D2, d2_optimal$Incon_RT, pch=21, bg="black")
lines(d2_optimal$D2, d2_optimal$Con_RT, type="l", lty=2)
points(d2_optimal$D2, d2_optimal$Con_RT, pch=21, bg="white")
grid()

plot(d1_optimal$D1, d1_optimal$Incon_RT, type="l", ylim = c(0.440,0.5))
points(d1_optimal$D1, d1_optimal$Incon_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Con_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Con_RT, pch=21, bg="white")
grid()

cor.plots <- function() {
  layout(matrix(c(1,2), ncol=2))
  plot(d2_optimal$D2, d2_optimal$Incon_RT, type="l", ylim = c(0.440,0.5))
  points(d2_optimal$D2, d2_optimal$Incon_RT, pch=21, bg="black")
  m <- lm(d2_optimal$Incon_RT ~ d2_optimal$D2)
  abline(m$coefficients[1], m$coefficients[2], col="grey25", lty=2)
  
  lines(d2_optimal$D2, d2_optimal$Con_RT, col="grey")
  points(d2_optimal$D2, d2_optimal$Con_RT, pch=21, bg="grey")
  m <- lm(d2_optimal$Con_RT ~ d2_optimal$D2)
  abline(m$coefficients[1], m$coefficients[2], col="grey75", lty=2)
  
  grid()
  
  plot(d1_optimal$D1, d1_optimal$Incon_RT, type="l", ylim = c(0.440,0.5))
  points(d1_optimal$D1, d1_optimal$Incon_RT, pch=21, bg="black")
  lines(d1_optimal$D1, d1_optimal$Con_RT, type="l", lty=2)
  points(d1_optimal$D1, d1_optimal$Con_RT, pch=21, bg="white")
  grid()
}
  
# Different version of "optimal"
d1_optimal <- subset(d1test, d1test$Alpha %in% c(0.25, 0.5, 0.75, 1) & d1test$LF <= 0.4 & d1test$EGS<= 0.2 & d1test$ANS<=0.2 & d1test$Bias <= 10)
d1_optimal <- aggregate(d1_optimal[c("Incon_RT", "Con_RT")], list(D1=d1_optimal$D1), mean)
d2_optimal <- subset(d2test, d2test$Alpha  %in% c(0.25, 0.5, 0.75, 1) & d2test$LF <= 0.4 & d2test$EGS<= 0.2 & d2test$ANS<=0.2 & d2test$Bias <= 10)
d2_optimal <- aggregate(d2_optimal[c("Incon_RT", "Con_RT")], list(D2=d2_optimal$D2), mean)


plot(d2_optimal$D2, d2_optimal$Incon_RT, type="l", ylim = c(0.43,0.48), xlim=c(0.5,1.5))
points(d2_optimal$D2, d2_optimal$Incon_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Incon_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Incon_RT, pch=21, bg="white")


plot(d2_optimal$D2, d2_optimal$Con_RT, type="l", ylim = c(0,2))
points(d2_optimal$D2, d2_optimal$Con_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Con_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Con_RT, pch=21, bg="white")


# For sims4

# Different version of "optimal"
d1_optimal <- subset(d1test, d1test$LF <= 1 & d1test$ANS<=1 & d1test$Bias <= 10)
d1_optimal <- aggregate(d1_optimal[c("Incon_RT", "Con_RT")], list(D1=d1_optimal$D1), mean)
d2_optimal <- subset(d2test, d2test$LF <= 1 & d2test$ANS<=1 & d2test$Bias <= 10)
d2_optimal <- aggregate(d2_optimal[c("Incon_RT", "Con_RT")], list(D2=d2_optimal$D2), mean)


plot(d2_optimal$D2, d2_optimal$Incon_RT, type="l", ylim = c(0.55, 0.6), xlim=c(0.5,1.5))
points(d2_optimal$D2, d2_optimal$Incon_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Incon_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Incon_RT, pch=21, bg="white")


plot(d2_optimal$D2, d2_optimal$Con_RT, type="l", ylim = c(0,2))
points(d2_optimal$D2, d2_optimal$Con_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Con_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Con_RT, pch=21, bg="white")



zd1 <- subset(d1test, d1test$EGS == 0.0)
zd2 <- subset(d2test, d2test$EGS == 0.0)


azd1 <- aggregate(zd1[c("Con_RT", "Incon_RT")], list(D1=zd1$D1), mean)
azd2 <- aggregate(zd2[c("Con_RT", "Incon_RT")], list(D2=zd2$D2), mean)

plot(azd2$D2, azd2$Incon_RT, type="l", ylim = c(0.56,0.58))
points(azd2$D2, azd2$Incon_RT, pch=21, bg="black")
lines(azd1$D1, azd1$Incon_RT, type="l", lty=2)
points(azd1$D1, azd1$Incon_RT, pch=21, bg="white")


## PSP FOR CORRS

cor.pattern <- function(a, b, c, d) {
  p1 <- 1 + approx.order(a, b) # D1 Con vs. D2 Con / D1 Eff vs D2 Eff
  p2 <- 1 + approx.order(c, d) # D1 Incon - D2 Incon
  pairing(p1, p2)
}


cor.pattern2 <- function(a, b, c, d) {
  p1 <- 1 + sign(round(b,2)) # D1 Con vs. D1 Incon / D1 Eff vs D2 Eff
  p2 <- 1 + sign(round(d,2)) # D1 Con vs D2 Incon
  p3 <- 1 + sign(round(a,2))
  p4 <- 1 + sign(round(c,2))               
  paste(p1, p2, p3, p4, sep="")
}

cor.pattern3 <- function(a, b) {
  p1 <- 1 + sign(round(a,2)) # D1 Con vs. D1 Incon / D1 Eff vs D2 Eff
  p2 <- 1 + sign(round(b,2)) # D1 Con vs D2 Incon
  paste(p1, p2, sep="")
}


vcor.pattern <- Vectorize(cor.pattern)
vcor.pattern2 <- Vectorize(cor.pattern2)
vcor.pattern3 <- Vectorize(cor.pattern3)

space$CorPattern <-0
space$CorPattern2 <-0
space$CorPattern3 <-0
space$CorPattern <- vcor.pattern(space$r_D1_Con, space$r_D2_Con, space$r_D1_Inc, space$r_D2_Inc)
space$CorPattern2 <- vcor.pattern2(space$r_D1_Eff, space$r_D2_Eff, space$r_D1_Inc, space$r_D2_Inc)
space$CorPattern3 <- vcor.pattern3(space$r_D2_Eff, space$r_D2_Inc)
cpsp <- tapply(space$Alpha, space$CorPattern3, length)

rcpsp <- round(100 * cpsp / sum(c(cpsp)), 1)

pie(rcpsp, col=rev(grey.colors(9)), border="white", edges = 1000)
pie(rcpsp, col=rev(jet.colors(80)), border="white", edges = 1000)



pie.plot <- function() {
  layout(matrix(c(1,2), ncol=2))
  pie(rpsp, col=rev(grey.colors(3)), labels = c("Correct\npattern\n(99.8%)", "", ""), border=NA, edges=2000, radius=1, lwd=2)
  title(main="(A) PSP: Group averages")
  
  pie(rcpsp, col=rev(grey.colors(9)), border="white", edges = 2000, radius = 1,
      labels = c("Correct\npattern\n(76.0%)", rep("", 8)))
  title(main="(B) PSP: Correlations with PSS")
}

# Error function for patterns

cor.target <- c(0.12, -0.20, 0.1, -0.38, 0.01, -0.43)

cor.error <- function(a, b, c, d, e, f) {
  sum((c(a, b, c, d, e, f) - cor.target)**2)
}

vcor.error <- Vectorize(cor.error)

space$CorError <- vcor.error(space$r_D1_Con, space$r_D2_Con, space$r_D1_Inc, space$r_D2_Inc, space$r_D1_Eff, space$r_D2_Eff)


