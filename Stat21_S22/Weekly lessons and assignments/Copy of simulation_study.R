## Problem set-up
##  y = 50 + 10x + \epsilon, where \epsilon is independently drawn from a N(0,16) distribution 
##  Generate 500 samples of 20 observations drawing one observation for each level of $x=1,1.5,2, \dots,10$ for each sample. 

set.seed(1001)  # this makes all our code results reproducible 
remove(list=ls())  # this makes sure we clear all previously defined environmental variables 
library('tidyverse')
x <- seq(0.5, 10, by=0.5) 

N <- 500  ## This is the number of samples of size 20 that we are simulating, play around with changing this! 
            
generate.response <- function(x){
              y <- 50 + 10*x + rnorm(20, mean=0, sd=4)
              return(y)
}
            
y_mtx <- matrix(rep(x, N), ncol=N, byrow=FALSE)
y_mtx <- apply(y_mtx, 2, generate.response)

            
## Part 1
### For each sample compute the least-squares estimates of the slope and intercept.
              
estimate.parameters <- function(response, predictor=x){
   mod = lm(response ~ predictor)
   beta0_hat = mod$coefficients[1]
   beta1_hat = mod$coefficients[2]
   return(c(beta0_hat, beta1_hat))
}
                
estimated_betas <- matrix(rep(NA,N*2), ncol=2)
colnames(estimated_betas) <- c("beta0_hat", "beta1_hat")
for(k in 1:N){
   estimated_betas[k,] = estimate.parameters(y_mtx[ ,k])
}
                
(estimated_betas <- data.frame(estimated_betas))

 
ggplot(estimated_betas, aes(x=beta0_hat)) + 
    geom_histogram(bins=40) + 
    labs(title= "Histogram of the estimated intercept for a SLR model", 
         x = "beta0_hat values for each sample", y = "Density")

ggplot(estimated_betas, aes(x=beta1_hat)) + 
     geom_histogram(bins=40) + 
     labs(title= "Histogram of the estimated slope for a SLR model", 
          x = "beta1_hat values for each sample", y = "Density")
 
            
            
## Part 2
### For each sample, compute an estimate of $E[Y \mid x=5]$. 
              
estimate.observation <- function(response,predictor=x){
     mod <- lm(response ~ predictor)
     y_hat <- mod$coefficients[1] + mod$coefficients[2]*5
     return(y_hat=y_hat)
}
                
y_hats <- rep(NA,N)
for(k in 1:N){
   y_hats[k] = estimate.observation(y_mtx[ ,k])
}

(y_hats <- data.frame(y_hats))

ggplot(y_hats, aes(x=y_hats)) + 
      geom_histogram(bins=40) + 
      labs(title= "Histogram of the fitted response for x=5", 
           x = "Different y_hat values for each sample", y = "Density")

## Part 3 
### For each sample, compute a $95\%$ CI on the slope, $\beta_1$
              
CI.slope <- function(response,predictor=x){
  mod <- lm(response ~ predictor)
  t_crit <- qt(0.05/2, df=20-2, lower.tail=FALSE)
  se_beta1 <- summary(mod)$coefficients[2,2] ##?
  beta1_hat <- mod$coefficients[2]
  LB <- beta1_hat - t_crit*se_beta1
  UB <- beta1_hat + t_crit*se_beta1
  return(c(LB, UB))
}


LB_slope <- rep(NA,N)
UB_slope <- rep(NA, N)
for(k in 1:N){
   conf_int = CI.slope(y_mtx[ ,k])
   LB_slope[k] = conf_int[1]
   UB_slope[k] = conf_int[2]
}

cbind(LB_slope,UB_slope)

((LB_slope<=10)&(UB_slope>=10))

sum((LB_slope<=10)&(UB_slope>=10))

    
## Part 4 
### For each estimate of $E[Y \mid x=5]$ in part 2, compute the $95\%$ CI for the mean response

CI.mean.response <- function(response,predictor=x){
  dat <- data.frame(response, predictor)
  remove(list=c("response","predictor"))
  mod <- lm(response ~ predictor, dat)
  y_hat <- data.frame(response = mod$coefficients[1] + mod$coefficients[2]*5, predictor=5)
  PI <- predict(mod, y_hat, interval="confidence", level = 0.95)
  return(c(PI[2], PI[3]))
}


LB_mean_response <- rep(NA,N)
UB_mean_response <- rep(NA, N)
for(k in 1:N){
    conf_int = CI.mean.response(y_mtx[ ,k])
    LB_mean_response[k] = conf_int[1]
    UB_mean_response[k] = conf_int[2]
}
cbind(LB_mean_response,UB_mean_response)

sum((LB_mean_response<=100)&(UB_mean_response>=100))

  