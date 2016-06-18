data <- read.table("sims2.txt", sep=",", header=F)
source("functions.R")
names(data) <- c("D1", "D2", "Cong_ACC", "Cong_RT", "Incong_ACC", "Incong_RT")
plot.by.2factors(data, "Incong_RT", "D1", "D2")
plot.by.2factors(data, "Incong_RT", "D2", "D1")

plot.by.1factor(data, "Incong_RT", "D2")
plot.by.1factor(data, "Cong_RT", "D2")

plot.by.1factor(data, "Incong_RT", "D1")
plot.by.1factor(data, "Cong_RT", "D1")
