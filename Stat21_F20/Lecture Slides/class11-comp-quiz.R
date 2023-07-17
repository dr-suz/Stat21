## Residual plot with non-constant variance
library(tidyverse)
set.seed(430)
x <- rnorm(45, 2, 0.6)
y4 <- 0.5*x +c(rnorm(9,0,0.1), rnorm(9,0,0.2), rnorm(9,0,2), rnorm(9,0,1.9), rnorm(9,0,0.15))
y1 <- 3.1*x + rnorm(45, 0, 0.3)
y2 <- x + rnorm(45, 0, 0.2)
y3 <- 0.1*x - rnorm(45, 0, 0.5)

input_data <- tibble(x,y1,y2,y3,y4)

lm1 <- lm(y1~x, input_data)
lm2 <- lm(y2~x, input_data)
lm3 <- lm(y3~x, input_data)
lm4 <- lm(y4~x, input_data)

resid_data <- input_data %>% mutate(resid1 = lm1$residuals,
                                    fitted1 = lm1$fitted.values,
                                    resid2 = lm2$residuals,
                                    fitted2 = lm2$fitted.values,
                                    resid3 = lm3$residuals,
                                    fitted3 = lm3$fitted.values,
                                    resid4 = lm4$residuals,
                                    fitted4 = lm4$fitted.values)

ggplot(resid_data, aes(x=fitted1, y=resid1)) + 
  geom_point() + 
  labs(title = "Residual Plot", x="Fitted values", y="Residuals") +
  geom_hline(yintercept=0) + ylim(-1,1)
ggplot(resid_data, aes(x=fitted2, y=resid2)) + 
  geom_point() + 
  labs(title = "Residual Plot", x="Fitted values", y="Residuals") +
  geom_hline(yintercept=0) + ylim(-1,1)
ggplot(resid_data, aes(x=fitted3, y=resid3)) + 
  geom_point() + 
  labs(title = "Residual Plot", x="Fitted values", y="Residuals") +
  geom_hline(yintercept=0) + ylim(-1.5,1.5)
ggplot(resid_data, aes(x=fitted4, y=resid4)) + 
  geom_point() + 
  labs(title = "Residual Plot", x="Fitted values", y="Residuals") +
  geom_hline(yintercept=0) + ylim(-6,6)


## Identify estimates from R output 
#The number of live births per 1000 people aged 15-4 in the US starting in 1965-1999
birth_dat <- tibble(yr = c(1965, 1970, 1975, 1980, 1985, 1990, 1995, 1999),
                    rate = c(19.4, 18.4, 14.8, 15.9, 15.6, 16.4, 14.8, 4.5))
birth_SLR <- lm(rate ~ yr, birth_dat)
summary(birth_SLR)
