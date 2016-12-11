# Damn

test <- read.table('grid-sims1/final.txt', header=F, sep=",")
names(test) <- c("Alpha", "LF", "EGS", "ANS", "Bias", "D1", "D2", "Con_ACC", "Con_RT", "Incon_ACC", "Incon_RT")

approx.equal <- function(x, y) {
  if (abs(x-y)/min(c(x,y)) < 0.02) {
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

plot.multid <- function() {
  colors <- rainbow(9)
  plot.new()
  plot.window(c(0,1), c(0,1.2))
  axis(1)
  axis(2)
  for (r in 1:300) {
    #print(r)
    z <- subset(test, test$Run == r)
    x = c(0, 1:6 / 6)
    y = c(z$Alpha, z$LF, z$EGS, z$ANS, z$Bias/10, z$D1/2, z$D2/2)
    lines(x=x, y=y, col="red", lwd=0.2)
  }
  text(x=x, y = rep(1.1, 7), labels = names(test)[1:7])
}

d <- round(tapply(test$Con_ACC, test$Pattern, length) / 135000, 4)
pie(d)

## --------------------------------------------------------------- ##
## ERROR FUNCTION
## --------------------------------------------------------------- ##

target <- c(0.98, 0.88, 0.421, 0.489)  # Target data

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

test$Error <- verror(test$Con_ACC, test$Incon_ACC,
                     test$Con_RT, test$Incon_RT)


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

test$Error2 <- verror2(test$Con_ACC, test$Incon_ACC,
                     test$Con_RT, test$Incon_RT)


plot(test$Con_ACC, test$Incon_ACC, xlim=c(0,1), ylim=c(0,1))
plot(test$Con_RT, test$Incon_RT, xlim=c(0,1), ylim=c(0,1))

equal <- subset(test, test$D1==1 & test$D2==1)

plot(equal$Con_ACC, equal$Incon_ACC, xlim=c(0,1), ylim=c(0,1), col="#33333399")
points(x=c(0.98), y=c(0.88), col="red", pch=3)
plot(equal$Con_RT, equal$Incon_RT, xlim=c(0,1), ylim=c(0,1), col="#33333399")
points(x=c(0.421), y=c(0.489), col="red", pch=3)
subset(equal, equal$Error2 == min(equal$Error2))

## --------------------------------------------------------------- ##
## CORRELATIONS
## --------------------------------------------------------------- ##

nd1 <- length(unique(test$D1))
nd2 <- length(unique(test$D2))
con <- c(rep("D1", nd1), rep("D2", nd2))
test$Cond <- con

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
  cor(subset[[var1]], subset[[var2]])
}
#vcalcbeta <- Vectorize(calculate.beta)

d1test <- subset(test, test$Cond == "D1")
d2test <- subset(test, test$Cond == "D2")

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


for (a in unique(space$Alpha)) {
  for (l in unique(space$LF)) {
    for (e in unique(space$EGS)) {
      for (n in unique(space$ANS)) {
        for (b in unique(space$Bias)) {
          space$beta_D1_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d1test, "D1", "Incon_RT", a, l, e, n, b)
          space$beta_D2_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d2test, "D2", "Incon_RT", a, l, e, n, b)
          space$beta_D1_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d1test, "D1", "Con_RT", a, l, e, n, b)
          space$beta_D2_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.beta(d2test, "D2", "Con_RT", a, l, e, n, b)
          space$r_D1_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d1test, "D1", "Incon_RT", a, l, e, n, b)
          space$r_D2_Inc[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d2test, "D2", "Incon_RT", a, l, e, n, b)
          space$r_D1_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d1test, "D1", "Con_RT", a, l, e, n, b)
          space$r_D2_Con[space$Alpha == a & space$LF == l & space$EGS == e & space$ANS == n & space$Bias == b] <- calculate.r(d2test, "D2", "Con_RT", a, l, e, n, b)
          
        }
      }
    }
  }
} 

space$r_D1_Con[is.na(space$r_D1_Con)] <- 0
space$r_D1_Inc[is.na(space$r_D1_Inc)] <- 0
space$r_D2_Con[is.na(space$r_D2_Con)] <- 0
space$r_D2_Inc[is.na(space$r_D2_Inc)] <- 0


space$diff_d2_r <- space$r_D2_Inc - space$r_D2_Con
space$diff_d2_r[is.na(space$diff_d2_r)] <- 0




error3 <- function() {
  
}


d1_optimal <- subset(d1test, d1test$Alpha == 0.25 & d1test$LF == 0.25 & d1test$EGS== 0 & d1test$ANS==0.2 & d1test$Bias == 10)
plot(d1_optimal$D1, d1_optimal$Incon_RT)

d2_optimal <- subset(d2test, d2test$Alpha == 0.25 & d2test$LF == 0.25 & d2test$EGS== 0 & d2test$ANS==0.2 & d2test$Bias == 10)
plot(d2_optimal$D2, d2_optimal$Incon_RT)

plot(d2_optimal$D2, d2_optimal$Incon_RT, type="l", ylim = c(0.490,0.520))
points(d2_optimal$D2, d2_optimal$Incon_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Incon_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Incon_RT, , pch=21, bg="white")


# Different version of "optimal"
d1_optimal <- subset(d1test, d1test$Alpha %in% c(0.25, 05) & d1test$LF == 0.25 & d1test$EGS== 0 & d1test$ANS==0.2 & d1test$Bias <= 10)
d1_optimal <- aggregate(d1_optimal[c("Incon_RT")], list(D1=d1_optimal$D1), mean)
d2_optimal <- subset(d2test, d2test$Alpha %in% c(0.25, 05) & d2test$LF == 0.25 & d2test$EGS== 0 & d2test$ANS==0.2 & d2test$Bias <= 10)
d2_optimal <- aggregate(d2_optimal[c("Incon_RT")], list(D2=d2_optimal$D2), mean)



plot(d2_optimal$D2, d2_optimal$Incon_RT, type="l", ylim = c(0.490,0.520))
points(d2_optimal$D2, d2_optimal$Incon_RT, pch=21, bg="black")
lines(d1_optimal$D1, d1_optimal$Incon_RT, type="l", lty=2)
points(d1_optimal$D1, d1_optimal$Incon_RT, , pch=21, bg="white")
