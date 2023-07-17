# Problem set-up
# y = 50 + 10x + \epsilon, \quad \text{where } \epsilon \sim N(0,16).
# Generate 500 samples of 20 observations drawing one observation for each level of $x=1,1.5,, \dots,10$ for each sample. 
# set.seed(1001)  
remove(list=ls())
library('tidyverse')
x <- seq(0.5, 10, by=0.5)

generate.response <- function(x){
  y <- 50 + 10*x + rnorm(20, mean=0, sd=4)
  return(y)
}
y_mtx <- matrix(rep(x, 500), ncol=500, byrow=FALSE)
y_mtx <- apply(y_mtx, 2, generate.response)

#------------------------------------------------------------------
# Part (a) - For each sample compute the least-squares estimates of the slope and intercept.
estimate.parameters <- function(response, predictor=x){
  mod = lm(response ~ predictor)
  beta0_hat = mod$coefficients[1]
  beta1_hat = mod$coefficients[2]
  return(c(beta0_hat, beta1_hat))
}

estimated_betas <- matrix(rep(NA,500*2), ncol=2)
colnames(estimated_betas) <- c("beta0_hat", "beta1_hat")
for(k in 1:500){
  estimated_betas[k,] = estimate.parameters(y_mtx[ ,k])
}
(estimated_betas <- data.frame(estimated_betas))

# With your group: 
# Construct histograms of the sample vlaues of $\hat{\beta}_0$ and $\hat{\beta}_1$ (the estimated_betas) by uncommenting the code below and filling in the ???. Discuss the shape of these histograms. 

#ggplot(estimated_betas, aes(x=???)) + 
#  geom_histogram(bins=40) + 
#  labs(title= "Histogram of ???", x = "???", y = "???")


#ggplot(estimated_betas, aes(x=???)) + 
#  geom_histogram(bins=40) + 
#  labs(title= "Histogram of ???", x = "???", y = "???")

# Discussion: 


#------------------------------------------------------------------
# Part (b) - For each sample, compute an estimate of $E[Y \mid x=5]$. 
estimate.observation <- function(response,predictor=x){
  mod <- lm(response ~ predictor)
  y_hat <- mod$coefficients[1] + mod$coefficients[2]*5
  return(y_hat=y_hat)
}

y_hats <- rep(NA,500)
for(k in 1:500){
  y_hats[k] = estimate.observation(y_mtx[ ,k])
}
(y_hats <- data.frame(y_hats))

# With your group:
# Construct a histogram of the estimates you obtained (the 500 y_hat values). Discuss the shape of the histogram 

#ggplot(y_hats, aes(x=???)) + 
#  geom_histogram(bins=40) + 
#  labs(title= "Histogram of ???", x = "???", y = "???")

# Discussion: 


#------------------------------------------------------------------
# Part (c) - For each sample, compute a $95\%$ CI on the slope. 
CI.slope <- function(response,predictor=x){
  mod <- lm(response ~ predictor)
  t_crit <- qt(0.05/2, df=20-2, lower.tail=FALSE)
  se_beta1 <- summary(mod)$coefficients[2,2] ##?
  beta1_hat <- mod$coefficients[2]
  LB <- beta1_hat - t_crit*se_beta1
  UB <- beta1_hat + t_crit*se_beta1
  return(c(LB, UB))
}

LB_slope <- rep(NA,500)
UB_slope <- rep(NA, 500)
for(k in 1:500){
  conf_int = CI.slope(y_mtx[ ,k])
  LB_slope[k] = conf_int[1]
  UB_slope[k] = conf_int[2]
}
cbind(LB_slope,UB_slope)

# Use the next two lines of code to determine how many of these intervals contain the true value $\beta_1=10$? Is this what you would expect?
((LB_slope<=10)&(UB_slope>=10))
sum((LB_slope<=10)&(UB_slope>=10))

# Discussion: 


#------------------------------------------------------------------
# Part (d) - For each estimate of $E[Y \mid x=5]$ in part (b), compute the $95\%$ CI. 
CI.mean.response <- function(response,predictor=x){
  dat <- data.frame(response, predictor)
  remove(list=c("response","predictor"))
  mod <- lm(response ~ predictor, dat)
  y_hat <- data.frame(response = mod$coefficients[1] + mod$coefficients[2]*5, predictor=5)
  PI <- predict(mod, y_hat, interval="confidence", level = 0.95)
  return(c(PI[2], PI[3]))
}

LB_mean_response <- rep(NA,500)
UB_mean_response <- rep(NA, 500)
for(k in 1:500){
  conf_int = CI.mean.response(y_mtx[ ,k])
  LB_mean_response[k] = conf_int[1]
  UB_mean_response[k] = conf_int[2]
}
cbind(LB_mean_response,UB_mean_response)

# Use the next two lines of code to determine how many of these intervals contain the true value of $E[Y \mid x=5]=100$? Is this what you would expect? 
((LB_mean_response<=100)&(UB_mean_response>=100))
sum((LB_mean_response<=100)&(UB_mean_response>=100))


# Discussion: 
