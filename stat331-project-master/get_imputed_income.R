#This file only contains the code necessary to get the imputed income covariate (does not include analysis code required to select imputed dataset)

#Reading in the data 
fdata <- read.csv("chds_births.csv")
head(fdata) 
#Calculat the number of missing valuesin each column 
na_count <- sapply(fdata, function(y) sum(length(which(is.na(y)))))
na.count <- data.frame(na_count)
na.count



#Check the columns that have more than 10% of the data missing 
count <- sapply(fdata, function(y) length(y))
na_percent <- (na_count/count)*100
na_percent <- data.frame(na_percent)
na_percent
sapply(fdata, function(x) sum(is.na(x)))


#Visualizing the missing data
#library(VIM)
#miss_plot <- aggr(fdata, col=c('navyblue','yellow'),
#                  numbers=TRUE, sortVars=TRUE,
#                  labels=names(fdata), cex.axis=.7,
#                  gap=3, ylab=c("Missing data","Pattern"))
fdata$fht <- NULL
fdata$fwt <- NULL 
colnames(fdata)
head(fdata)
library(mice)
md.pattern(fdata)

#pbox(fdata, pos = 1, int = FALSE, cex = 0.7)
#Imputting the data with "pmm" method as referenced from "https://stefvanbuuren.name/mice/"
imp <- mice(fdata, m = 5, maxit = 50, meth = 'pmm', seed = 500)
#summary(imp)
densityplot(imp)
stripplot(imp, pch = 20, cex = 1.2)

modelFit1 <- with(data = imp, lm(wt ~ gestation + parity + meth + mage + income))
#summary(modelFit1)

combine <- pool(modelFit1)

imp_1 <- data.frame(complete(imp,1))
imp_2 <- data.frame(complete(imp,2))
imp_3 <- data.frame(complete(imp,3))
imp_4 <- data.frame(complete(imp,4))
imp_5 <- data.frame(complete(imp,5))

mp_list <- list(imp_1, imp_2, imp_3, imp_4, imp_5)

#Get sample 4 income values

imputed_income <- imp_4$income

#Final dataset is fdata
fdata$income <- imputed_income