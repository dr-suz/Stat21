## Comp quiz class 7 
set.seed(100)


## Which plot shows strongest deviation from normality 
samp_data <- tibble(X1 = rnorm(70, 5 , 3), X2 = rnorm(70), X3 = rnorm(70, -2, 0.7), Y = rcauchy(70))

ggplot(samp_data, aes(sample = (X1-mean(X1))/sd(X1))) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(title = "Normal probability plot") + ylim(-6,6)

ggplot(samp_data, aes(sample = (X2-mean(X2))/sd(X2))) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(title = "Normal probability plot") + ylim(-6,6)

ggplot(samp_data, aes(sample = (X3-mean(X3))/sd(X3))) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(title = "Normal probability plot") + ylim(-6,6)

ggplot(samp_data, aes(sample = (Y-mean(Y))/sd(Y))) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(title = "Normal probability plot") + ylim(-6,6)


## which plot shows systematic changes in variability 
samp_data2 <- tibble(quant_varb1 = c(rnorm(40, 2, 0.8), rnorm(40, -4, 0.8), rnorm(40, 0, 0.8)), 
                     cat_varb1 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)),
                     quant_varb2 = c(rnorm(40, 2, 1.25), rnorm(40, -4, 0.25), rnorm(40, 0, 0.8)), 
                     cat_varb2 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)),
                     quant_varb3 = c(rnorm(40, 2, 0.78), rnorm(40, -4, 0.82), rnorm(40, 0, 0.8)), 
                     cat_varb3 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)),
                     quant_varb4 = c(rnorm(40, 2, 0.79), rnorm(40, -4, 0.8), rnorm(40, 0, 0.81)), 
                     cat_varb4 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)))
ggplot(samp_data2, aes(x=cat_varb1, y=quant_varb1)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 
ggplot(samp_data2, aes(x=cat_varb2, y=quant_varb2)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 
ggplot(samp_data2, aes(x=cat_varb3, y=quant_varb3)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 
ggplot(samp_data2, aes(x=cat_varb4, y=quant_varb4)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 


## which plot shos groups with differing variabilities 
samp_data3 <- tibble(quant_varb1 = c(rnorm(40, 2, 0.8), rnorm(40, -4, 0.8), rnorm(40, 0, 0.8)), 
                     cat_varb1 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)),
                     quant_varb2 = c(rnorm(40, 2, 0.25), rnorm(40, -4, 1.3), rnorm(40, 0, 0.8)), 
                     cat_varb2 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)),
                     quant_varb3 = c(rnorm(40, 2, 0.78), rnorm(40, -4, 0.82), rnorm(40, 0, 0.8)), 
                     cat_varb3 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)),
                     quant_varb4 = c(rnorm(40, 2, 0.79), rnorm(40, -4, 0.8), rnorm(40, 0, 0.81)), 
                     cat_varb4 = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)))
ggplot(samp_data3, aes(x=cat_varb1, y=quant_varb1)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 
ggplot(samp_data3, aes(x=cat_varb2, y=quant_varb2)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 
ggplot(samp_data3, aes(x=cat_varb3, y=quant_varb3)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 
ggplot(samp_data3, aes(x=cat_varb4, y=quant_varb4)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 



## given boxplot, are the means different due to randomness or a relationship between the variables
samp_data4 <- tibble(quant_varb = c(rnorm(40, 2, 0.85), rnorm(40, -4, 0.78), rnorm(40, 0, 0.8)), 
                     cat_varb = c(rep("class1", 40), rep("class2", 40), rep("class3", 40)))
ggplot(samp_data4, aes(x=cat_varb, y=quant_varb)) + 
  geom_boxplot() + 
  labs(title = "Boxplot") + xlab("Categorical variable") + ylab("Quantitative variable") 


## Car data 
fake_cars <- tibble(mph = rnorm(350, 25, 2), life = rnorm(350, 10, 1), weight = rnorm(350, 3000, 60))

mean(fake_cars$mph)
sd(fake_cars$mph)
var(fake_cars$mph)

mean(fake_cars$life)
sd(fake_cars$life)
var(fake_cars$life)

mean(fake_cars$weight)
sd(fake_cars$weight)
var(fake_cars$weight)




