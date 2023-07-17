rm(list = ls())

library(tidyverse)

setwd("~/Dropbox/z MyStuff Dropbox/Swat docs/Stat 21/Class1_files")

read_table2("BeefDemand.txt", col_names = TRUE)
    ##Note the comments under the column/variable names in the .txt file
    ##o/w could have used: read_table("BeefDemand.txt", header=TRUE)

BeefTable <- read_table2("BeefDemand.txt", col_names = TRUE) %>% 
              view()

BeefTable %>% names() 


dev.new()
ggplot(data = BeefTable) + 
  geom_point(mapping = aes(x = RealBeefPrice, y = BeefConsump))
dev.off()


## all of the following plots show no evidence of a relationship between the predictor and the response variables
## this is because there truely is no relationship in these examples! 
## the data (response and predictor variables) were generated independently
##uniformily random variables 
myX <- runif(100, min = 140, max = 240)
myY <- runif(100, min = 140, max = 240)
dev.new()
ggplot() + 
  geom_point(mapping = aes(x = myX, y = myY))
dev.off()

##nomally distributed random variables
myX2 <- rnorm(100, mean = 180, sd = 3)
myY2 <- rnorm(100, mean = 180, sd = 3)
dev.new()
ggplot() + 
  geom_point(mapping = aes(x = myX2, y = myY2))
dev.off()


##poisson distributed random variables
myX3 <- rpois(100, lambda = 180)
myY3 <- rpois(100, lambda = 180)
dev.new()
ggplot() + 
  geom_point(mapping = aes(x = myX3, y = myY3))
dev.off()

#======================================================
library(swirl)


