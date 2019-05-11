OPTALLOC <- function(df, stratum, y, n, seed){
  # 
  # OPTALLOC returns the training set selected via.
  # stratified random sampling with optimal allocation
  # for a given categorical variable "stratum"
  #   Input 
  #     df: a dataframe
  #     stratum: the categorical variable whose levels
  #               are the strata
  #     y: the response covariate name
  #     n: the desired size of the training set
  #     seed: the seed for the random number generator
  #
  set.seed(seed) # set seed
  N <- length(df[,stratum]) #population size
  
  #initialize vectors
  vars <- c(rep (NA, length(unique(df[stratum]))))
  Wh <- c(rep(NA, length(unique(df[stratum]))))
  nh <- c(rep(NA, length(unique(df[stratum]))))
  counter <- 1
  
  #group by strata
  for (x in split(birth.data, birth.data[stratum])) {
    if (is.na(var(x[1][,y]))) {
      vars[counter] <- 0
    } else {
      vars[counter] <- var(x[1][,y])
    }
    Wh[counter] <- length(x[1][,y])/N
    nh[counter] <- sqrt(vars[counter])*(length(x[1][,y])/N)
    if(length(x[1]) > 0 && nh[counter] ==0 ){
      nh[counter] <- 1
    }
    counter <- counter + 1
  }
  
  #calculate nh
  den <- sum(nh)
  nh <- round(nh/den * n)
  
  #stratified sampling
  counter <- 1
  train.inds = c(NA)
  for (x in split(birth.data, birth.data[stratum])) {
    train.inds = c(train.inds, sample(as.numeric(rownames(x)), nh[counter]))
    counter <- counter + 1
  }
  
  return(train.inds)
}


