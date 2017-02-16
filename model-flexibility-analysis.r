## ------------------------------------------------------------------
## Additional analysis of Simulation Data from the PSS/Simon
## Experiment, published as:
## 
##   Stocco, A., Murray, N. L., Yamasaki, B. L., Renno, T. J., 
##   Nguyen, J., & Prat, C. S. "Individual differences in the Simon
##   effect are underpinned by differences in competitive dynamics 
##   in the basal ganglia: An experimental verification and a 
##   computational model". Cognition, 2017.
##
## ------------------------------------------------------------------
## Note: This analysis does not appear in the paper, but it is worth
## including as a measure of model complexity. Model Flexiility 
## Analysis (MFA) is described in:
##
##    Veksler, V. D., Myers, C. W., & Gluck, K. A. (2015). 
##    "Model flexibility analysis". Psychological review, 122(4), 
##    755-769.
##
## ------------------------------------------------------------------

library(Rmisc)
library(matlab)
library(pwr)

#test <- read.table('grid-sims4/final.txt', header=F, sep=",")
test <- read.table('model/grid-sims/final.txt', header=F, sep=",")
names(test) <- c("Alpha", "LF", "EGS", "ANS", "Bias", "D1", "D2", "Con_ACC", "Con_RT", "Incon_ACC", "Incon_RT")
test$SimonEffect <- test$Incon_RT - test$Con_RT

target <- c(0.98, 0.88, 0.421, 0.489)  # Target data

## ------------------------------------------------------------------
## MODEL FLEXIBILITY ANALYSIS
## ------------------------------------------------------------------

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
    m[i*N, j*N] <- dim(z)[1]
  }
}


m[m < 1] <- 0


# Greyscale version
# -----------------
image(m, col=grey(20:5/20))
points(x=target[3], y=target[4], col="red", lwd=2, pch=4)
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
