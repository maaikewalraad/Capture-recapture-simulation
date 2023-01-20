# rm(list=ls()) # Remove environment
require(data.table)
require(dplyr)

crc_data <- function(n1, list_cor) {
  
  set.seed(2000) # Set seed for reproducibility 
  # Error handling
  n1 <- abs(n1)
  n1 <- round(n1, 0)
  list_cor <- round(list_cor, 2)
  if (!(between(list_cor, -0.99, 0.99))) {
    stop("Please provide a correlation value between -0.99 and 0.99.")
  }
  dt <- data.table()
  dt[, n1 := c(rep(1, n1*0.9), rep(0, n1*0.1))]
  # Data simulation should be (partly) based on actual ratio (see below)
  av_rec <- 0.42 # around 42% gets recaptured from the first sample (theory-based)
  rec_rate <- av_rec + I(rnorm(1) / 100) # Add noise to this,  /100 because the recapture rate is in percentages 
  ratio_n2 <- rec_rate + list_cor*rec_rate # Dependency determines ratio {0/1} which also determines amount of n_11
  n_10 <- round((1-ratio_n2)*n1)
  n_11 <- round(ratio_n2*n1)
  if (I(n_10 + n_11) != n1) {
    stop("Second sample size incorrectly calculated.")
  }
  dt[, n2 := c(rep(0, n_10), rep(1, n_11))] 
  return(dt)
  
}

###################
# FILL IN VALUES  #
###################

dt_list <- list()
size_of_samples <- 100000 # How many units are you going to capture two (separate) times? 
sample_dependencies <- seq(-0.99, 0.99, length.out = 5) # Correlation: values between -0.99 and 0.99
for (i in 1:length(sample_dependencies)) {
  
  dt <- crc_data(size_of_samples, sample_dependencies[i])
  dt <- as.data.table(dt)
  dt[, Dependency := sample_dependencies[i]]
  dt_list[[i]] <- dt
  
  
}

saveRDS(dt_list, "dt_list.RData") 


