# Homework 2 - Macro-Finance ----------------------------------------------
# Loading packages&data ---------------------------------------------------
# Loading libraries 

library('readxl')
library('tidyverse')
rm(list=ls())
# Loading data

data <- filter(read_excel('JSTdatasetR5.xlsx', sheet = 'Data'), country == 'Australia')

# Log real consumption growth ----------------------------------------------------------------

# Log real consumption growth

log_realcons <- log(data$rconpc, base = exp(1))

# Element [1] is the growth between 1871 and 1870

# The last element is the growth between 2017-2016

realcons_growth <- vector(mode='numeric', length = length(log_realcons)-1)

for (i in seq_along(realcons_growth)) {
  
  realcons_growth[i] <- log_realcons[i+1] - log_realcons[i]
  
}

# mean of ln∆c for Australia

mean_congrowth <- mean(realcons_growth)

# variance of ln∆c for Australia

var_congrowth <- var(realcons_growth)

# Risky return & safe rate from the data ----------------------------------

# Ingredients to compute risky return and safe rate from the data

cpi_ratio<- vector(mode='numeric', length = length(data$cpi)-1)

# The first element is the ratio between 1870 1871

# The last element is the ration 2016/2017

for (i in seq_along(cpi_ratio)) {
  
  cpi_ratio[i] <- (data$cpi[i] / data$cpi[i+1])
  
}

# The first element of real equity return refers to the year 1871

# We start from 1871 because the first element of cpi ratio vector is 1870/1871

real_eq_ret <-vector(mode='numeric', length = length(data$eq_tr)-1)

for (i in seq_along(real_eq_ret)) {
  
  real_eq_ret[i] <- ((1 + data$eq_tr[i+1]) * cpi_ratio[i]) - 1
  
}

# The first element of real bill rate refers to the year 1871

real_bill_rate <-vector(mode='numeric', length = length(data$bill_rate)-1)

for (i in seq_along(real_bill_rate)) {
  
  real_bill_rate[i] <- ((1 + data$bill_rate[i+1]) * cpi_ratio[i]) - 1
  
}

# Risky return from the data : 0.08386252 ||| We are disregarding the NAs

m_real_eq_ret <- mean(real_eq_ret, na.rm = T)

# Safe rate from the data : 0.02018873  ||| We are disregarding the NAs

m_real_bill_rate <- mean(real_bill_rate , na.rm = T)

# Risk premium from data

risk_premium <- m_real_eq_ret - m_real_bill_rate 

# Risky return & safe rate from the model ---------------------------------

# Now we compute the implied safe rate and implied risky rate

# Assumptions about the parameters of the model

gamma <- 10 # high risk aversion, we should expect high risk premia
delta <- 0.1 # impatience

r_safe_model <- delta + gamma * mean_congrowth - 0.5 * (gamma)^2 * var_congrowth
r_safe_model
# not taking the last 2 values as we have NAs

beta <- (cov(real_eq_ret[1:145],realcons_growth[1:145])) / var(realcons_growth[1:145])
beta

risky_rate_model <- exp(r_safe_model) + (2 * gamma * var_congrowth) - 1
risky_rate_model
model_risk_premium <- risky_rate_model - r_safe_model

# Summary -----------------------------------------------------------------

ingredients <- as.data.frame(matrix(data=NA,nrow = 1, ncol = 3))
colnames(ingredients) <- c("mean_real_consumption_growth",
                           "variance_real_consumption_growth",
                           "consumption_beta")
ingredients[1,1] <- mean_congrowth
ingredients[1,2] <- var_congrowth
ingredients[1,3] <- beta


model_results <- as.data.frame(matrix(data=NA,nrow = 1, ncol = 3))
colnames(model_results) <- c("model_implied_risky_rate",
                             "model_implied_safe_rate",
                             "model_risk_premium")
model_results[1,1] <- risky_rate_model 
model_results[1,2] <- r_safe_model
model_results[1,3] <- model_risk_premium 

data_results <- as.data.frame(matrix(data=NA,nrow = 1, ncol = 3))
colnames(data_results) <- c("data_risky_rate",
                             "data_safe_rate",
                            "data_risk_premium")
data_results[1,1] <- m_real_eq_ret
data_results[1,2] <- m_real_bill_rate
data_results[1,3] <- risk_premium

