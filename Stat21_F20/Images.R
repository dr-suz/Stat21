library(tidyverse)
library(gridExtra)
set.seed(10002)



dat <- tibble(x1 = rnorm(150), x2 = runif(150,-2,2), x3 = c(rnorm(90, -2,1), rnorm(150-90, 1, 1)), x4 = runif(150,-3,3))

ggplot(dat, aes(x=x1)) + 
  geom_histogram(aes(y=..density..),       
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(bw=0.7) + xlim(-5,5)  


ggplot(dat, aes(x=x2)) + 
  geom_histogram(aes(y=..density..),       
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(bw=0.7) + xlim(-5,5)  


ggplot(dat, aes(x=x3)) + 
  geom_histogram(aes(y=..density..),       
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(bw=0.7) + xlim(-5,5)  


ggplot(dat, aes(x=x4)) + 
  geom_histogram(aes(y=..density..),       
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(bw=0.7) + xlim(-5,5)  


