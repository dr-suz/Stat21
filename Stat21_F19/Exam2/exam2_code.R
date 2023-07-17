biomass <- read_table2("~/Google Drive Swat/Swat docs/Stat 21/Data/biomass_data.txt", col_names = TRUE)

### Small model

biomass %>% select(BIO, pH, K) %>% pairs(labels=c("biomass","pH","K"),pch=16) 

cor(biomass$pH, biomass$K)

MLR_small <- lm(BIO ~ pH + K, data = biomass)
MLR_small_sum <- summary(MLR_small)

resid_data_small <- biomass %>% mutate(resid = MLR_small_sum$residuals,
                                       yhat = MLR_small$fitted.values)


ggplot(resid_data_small, aes(x=yhat, y=resid)) +
#  geom_segment(aes(x=yhat, xend=yhat , y=rep(0, length(resid)), yend = resid)) + 
  geom_point() + 
  geom_hline(yintercept=0) +
  labs(title="Residual plot for biomass data", subtitle="Small model with two predictor variables",
       x="Predicted biomass (gm^-2)", y="Residuals")

QQsmall <- ggplot(resid_data_small, aes(sample = yhat))
QQsmall + stat_qq() + stat_qq_line() + 
  labs(title = "Normal probability plot small model", subtitle = "Biomass data")


### Large model

MLR_large <- lm(BIO ~ as_factor(Location) + pH + K , data = biomass)
MLR_large_sum <- summary(MLR_large)

resid_data_large <- biomass %>% mutate(resid = MLR_large_sum$residuals,
                                       yhat = MLR_large$fitted.values)

ggplot(resid_data_large, aes(x=yhat, y=resid)) +
  #  geom_segment(aes(x=yhat, xend=yhat , y=rep(0, length(resid)), yend = resid)) + 
  geom_point() + 
  geom_hline(yintercept=0) +
  labs(title="Residual plot for biomass data", subtitle="Large model with four predictor variables (1 categorical)",
       x="Predicted biomass (gm^-2)", y="Residuals")

QQlarge <- ggplot(resid_data_large, aes(sample = yhat))
QQlarge + stat_qq() + stat_qq_line() + 
  labs(title = "Normal probability plot large model", subtitle = "Biomass data")



### Large model with y transformation

biomass2 <- biomass %>% mutate(log_biomass = log(BIO))


MLR_large2 <- lm(log_biomass ~ as_factor(Location) + pH + K , data = biomass2)
MLR_large_sum2 <- summary(MLR_large2)

resid_data_large2 <- biomass2 %>% mutate(resid = MLR_large_sum2$residuals,
                                       yhat = MLR_large2$fitted.values)

ggplot(resid_data_large2, aes(x=yhat, y=resid)) +
  #  geom_segment(aes(x=yhat, xend=yhat , y=rep(0, length(resid)), yend = resid)) + 
  geom_point() + 
  geom_hline(yintercept=0) +
  labs(title="Residual plot for biomass data", subtitle="Large model with three predictor variables (1 categorical)",
       x="Predicted biomass (gm^-2)", y="Residuals")

QQlarge <- ggplot(resid_data_large2, aes(sample = yhat))
QQlarge + stat_qq() + stat_qq_line() + 
  labs(title = "Normal probability plot large model", subtitle = "Biomass data")



### Small model with y transformation

MLR_small2 <- lm(log_biomass ~ as_factor(Location) + pH + K , data = biomass2)
MLR_small_sum2 <- summary(MLR_small2)

resid_data_small2 <- biomass2 %>% mutate(resid = MLR_small_sum2$residuals,
                                         yhat = MLR_small2$fitted.values)

ggplot(resid_data_small2, aes(x=yhat, y=resid)) +
  #  geom_segment(aes(x=yhat, xend=yhat , y=rep(0, length(resid)), yend = resid)) + 
  geom_point() + 
  geom_hline(yintercept=0) +
  labs(title="Residual plot for biomass data", subtitle="Small model with two predictor variables",
       x="Predicted biomass (gm^-2)", y="Residuals")

QQlarge <- ggplot(resid_data_small2, aes(sample = yhat))
QQlarge + stat_qq() + stat_qq_line() + 
  labs(title = "Normal probability plot large model", subtitle = "Biomass data")



