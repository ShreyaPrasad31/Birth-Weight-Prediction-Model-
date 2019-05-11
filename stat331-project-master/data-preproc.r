births <- read.csv("chds_births.csv")
# 17 covariates

pairs(births[, 1:5], pch=19)
pairs(births[, c(1, 4,6,9,11,14)], pch=19)
pairs(births[, c(1, 15,16,17, 18)], pch=19)
pairs(births[, c(1, 2, 3, 5, 10,)], pch=19)

