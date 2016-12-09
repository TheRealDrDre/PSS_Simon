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

## --------------------------------------------------------------- ##
## CORRELATIONS
## --------------------------------------------------------------- ##

