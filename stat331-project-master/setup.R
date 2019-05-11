#
# This script produces birth.data, which is the data to be used for
# the regression model, and train.inds, the indices for the train-set
# and num.train which is the size of the training set
# and MSPE function for calculating MSPE for a given model (uses train.ind)
source("optalloc.R")
source("get_imputed_income.R")
births <- read.csv("chds_births.csv")
meth.names <- c('Caucasian','Caucasian','Caucasian','Caucasian','Caucasian','Caucasian', 'Mexican', 'African-American', 'Asian', 'Mixed', 'Other')
med.names <- c('elementary', 'middle', 'hs', 'hs + trade', 'hs + college', 'college', 'trade', 'unclear')
feth.names <- c('Caucasian','Caucasian','Caucasian','Caucasian','Caucasian','Caucasian', 'Mexican', 'African-American', 'Asian', 'Mixed', 'Other')
fed.names <- c('elementary', 'middle', 'hs', 'hs + trade', 'hs + college', 'college', 'trade', 'unclear')
marital.names <- c(NA, 'married', 'separated', 'divorced', 'widowed', 'never married')
income.names <- c('<2500', '2500-4999', '5000-7499', '7500-9999', '10000-12499', '12500-14999', '15000-17499', '20000-22499', '>22500')
smoke.names <- c('never', 'now', 'until pregnancy', 'used to')
time.names <- c('never', 'still smokes', 'during pregnancy', 'less than a year', '1-2yrs', '2-3yrs', '3-4yrs', '5-9yrs', '10+yrs', 'quit - unknown then')
number.names <- c('never', '1-4', '5-9', '10-14', '15-19', '20-29', '30-39', '40-60', '>60', 'smoked, amount unknown')

births$meth <- meth.names[births$meth + 1]
births$feth<- feth.names[births$feth + 1]
births$fed <- fed.names[births$fed + 1]
births$marital <- marital.names[births$marital+1]
births$income <- income.names[fdata$income + 1]
births$smoke <- smoke.names[births$smoke + 1]
births$time <- time.names[births$time + 1]
births$number <- number.names[births$number + 1]

keeps <- c("wt", "gestation", "parity", "time", "number", "smoke", "mage", "mwt", "mht", "meth","income")
cat.var <- c("smoke", "number", "time", "income")
birth.data <- births[keeps]
birth.data <- na.omit(birth.data)

#Initial Models
ntot <- dim(birth.data)[1]
ntrain <- 1000

train.ind <- c(NA)
for (c in cat.var) {
  train.ind.curr <- OPTALLOC(birth.data, c, "wt", ntrain/length(cat.var), 23430)
  train.ind <- unique(c(train.ind, train.ind.curr))
}
train.ind <- na.omit(train.ind)
num.train <- length(train.ind)

MSPE <- function(M, train) {
  print(M$call)
  print(sum((birth.data$wt[-train] - predict(M, newdata = birth.data[-train,]))^2))
}