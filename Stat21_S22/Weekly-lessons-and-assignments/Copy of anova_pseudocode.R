
## This is correct
model <- lm(response ~ factor(predictor), data_obj)
anova(model)


##This is not correct and will not work the way you expect it to 
model <- anova(response ~ factor(predictor), data_obj)
summary(model)