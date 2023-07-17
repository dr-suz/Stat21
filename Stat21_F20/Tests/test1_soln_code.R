### Test 1 
### Solution Code 
### Stat 021 F 2010 
###########################
# LIBRARIES
###########################
library(ggplot2)
library(tidyverse)
library(gridExtra)
#library(gcookbook)
library(knitr)
# the packages below are for formatting tables 
library(kableExtra)


SAT_data <- read_table2(url("http://www.swarthmore.edu/NatSci/sthornt1/DataFiles/SAT_data2.txt"))



## Problem 1


SAT_data <- SAT_data %>% mutate(Income_fct = fct_inorder(Income))
SAT_anova <- aov(SAT_score ~ Income_fct, SAT_data)
summary(SAT_anova)

ggplot(SAT_data, aes(x=Income_fct, y=SAT_score)) +
  geom_boxplot() + 
  labs(title="Boxplot of SAT scores over income levels")

SAT_data1 <- SAT_data %>% filter(Income_fct=="<20K")
ggplot(SAT_data1, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for <$20K")

SAT_data2 <- SAT_data %>% filter(Income_fct=="20K-40K")
ggplot(SAT_data1, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $20K-$40K")

SAT_data3 <- SAT_data %>% filter(Income_fct=="40K-60K")
ggplot(SAT_data3, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $40K-$60K")

SAT_data4 <- SAT_data %>% filter(Income_fct=="60K-80K")
ggplot(SAT_data4, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $60K-80K")

SAT_data5 <- SAT_data %>% filter(Income_fct=="80K-100K")
ggplot(SAT_data5, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $80K-$100K")

SAT_data6 <- SAT_data %>% filter(Income_fct=="100K-120K")
ggplot(SAT_data6, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $100K-$120K")

SAT_data7 <- SAT_data %>% filter(Income_fct=="120K-140K")
ggplot(SAT_data7, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $120K-$140K")

SAT_data8 <- SAT_data %>% filter(Income_fct=="140K-160K")
ggplot(SAT_data8, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $140K-$160K")

SAT_data9 <- SAT_data %>% filter(Income_fct=="160K-200K")
ggplot(SAT_data9, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for $160K-$200K")

SAT_data10 <- SAT_data %>% filter(Income_fct==">200K")
ggplot(SAT_data10, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for >$20K")



## Problem 2


TukeyHSD(SAT_anova)


## Problem 3


income_SAT2 <- tibble(income = c(10000, 30000, 50000, 70000, 90000, 110000,
                                 130000, 150000, 180000, 300000),
                      median_SAT = c(1323, 1398, 1461, 1503, 1545, 1580, 1594,
                                     1619, 1636, 1721))

SLR_SAT <- lm(median_SAT ~ income, income_SAT2)
summary(SLR_SAT)

income_SAT2 <- income_SAT2 %>% mutate(resids = SLR_SAT$residuals,
                                      fits = SLR_SAT$fitted.values)
ggplot(income_SAT2, aes(sample=median_SAT)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for residuals")

ggplot(income_SAT2, aes(x=fits, y=resids)) +
  geom_point() + 
  labs(title="Residuals vs fitted values", x="Fitted values", y="Residuals") +
  geom_hline(yintercept=0)

household_income <- tibble(income_bracket = factor(c("<15,000", "15,000-24,999", "25,000-34,999",
                                                     "35,000-49,999", "50,000-74,999", "75,000-99,999",
                                                     "100,000-149,999", "150,000-199,999", ">=200,000")),
                           households = c(11689.041, 10276.08, 10661.433, 15028.767, 21194.415, 
                                          15799.473, 19909.905, 10661.433, 13230.453))
colnames(household_income) <- c("Income Bracket", "Number of Households")


## Problem 4


household_income <- tibble(
  income_bracket = factor(c("<15,000", "15,000-24,999", "25,000-34,999",
                            "35,000-49,999", "50,000-74,999", "75,000-99,999",
                            "100,000-149,999", "150,000-199,999", ">=200,000")),
  households = c(11689.041, 10276.08, 10661.433, 15028.767, 21194.415, 15799.473, 
                 19909.905, 10661.433, 13230.453))

total_households <- sum(household_income$households)
chisq.test(household_income$households, p=rep(1/9, 9))


household_income_race <- tibble(income_bracket = factor(c(rep(c("<15,000", "15,000-24,999", "25,000-34,999",
                                              "35,000-49,999", "50,000-74,999", "75,000-99,999",
                                              "100,000-149,999", "150,000-199,999", ">=200,000"),3)), levels=c("<15,000", "15,000-24,999", "25,000-34,999","35,000-49,999", "50,000-74,999", "75,000-99,999", "100,000-149,999", "150,000-199,999", ">=200,000")),
                                households = c(6195.364, 6195.364, 6365.1, 9335.48, 13748.616, 10863.104, 14257.824, 7892.724, 10014.424,
                                               2933.288, 1961.21, 1944.156, 2336.398, 2865.072, 1671.292, 1841.832, 716.268, 784.484, 
                                               444.99, 342.3, 355.992, 595.602, 883.134, 855.75, 1225.434, 855.75, 1293.894),
                                race = factor(c(rep("White",9),rep("Black",9),rep("Asian",9))))



## Problem 5



household_income_race <- tibble(
  income_bracket = factor(c(rep(c("<15,000", "15,000-24,999", "25,000-34,999", 
                                  "35,000-49,999", "50,000-74,999", "75,000-99,999",
                                  "100,000-149,999", "150,000-199,999", ">=200,000"),3)), 
                          levels=c("<15,000", "15,000-24,999", 
                                   "25,000-34,999","35,000-49,999", 
                                   "50,000-74,999", "75,000-99,999", 
                                   "100,000-149,999", "150,000-199,999", ">=200,000")),
  households = c(6195.364, 6195.364, 6365.1, 9335.48, 13748.616, 10863.104, 14257.824, 
                 7892.724, 10014.424, 2933.288, 1961.21, 1944.156, 2336.398, 2865.072, 
                 1671.292, 1841.832, 716.268, 784.484, 444.99, 342.3, 355.992, 595.602, 
                 883.134, 855.75, 1225.434, 855.75, 1293.894),
  race = factor(c(rep("White",9),rep("Black",9),rep("Asian",9))))

income_race_table <- matrix(c(444.990,  2933.288,  6195.364,
         342.300,  1961.210,  6195.364,
         355.992,  1944.156,  6365.100,
         595.602,  2336.398,  9335.480,
         883.134,  2865.072, 13748.616,
         855.750,  1671.292, 10863.104, 
         1225.434,  1841.832, 14257.824,
         855.750,   716.268,  7892.724,
         1293.894,   784.484, 10014.424), ncol=3, nrow=9, byrow=TRUE)
chisq.test(income_race_table)

