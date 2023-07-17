## ice cream ratings 
## randomly select 20 ratings from each brand 
## use the sugar content and brand name as predictors of the ratings 
## source: https://www.kaggle.com/tysonpo/ice-cream-dataset (sugar content looked up individually for each data point)

##------------- Data collection
library('tidyverse')
ic_data <- read_csv('ice-cream-ratings.csv') %>% select(-c(X4,X5,X6))
ic_data1 <- rep(ic_data$rating, ic_data$rating_count)
ic_data2 <- rep(ic_data$key, ic_data$rating_count)
length(ic_data1) == length(ic_data2 )

ic_data_rank <- tibble(key = ic_data2, rating = ic_data1)

ic_data_rank <- ic_data_rank %>% 
                 separate(key, sep="_", into=c("id", "brand"), convert = TRUE) 


bj_samp <- ic_data_rank %>% filter(brand=="bj") %>% sample_n(12)
bj_samp2 <- rbind(ic_data_rank %>% filter(brand=="bj")  %>% filter(rating<2) %>% sample_n(2), 
                       ic_data_rank %>% filter(brand=="bj")  %>% filter((rating>=2)&(rating<=4.2)) %>% sample_n(4))

breyers_samp <- ic_data_rank %>% filter(brand=="breyers") %>% sample_n(12)
breyers_samp2 <- rbind(ic_data_rank %>% filter(brand=="breyers")   %>% filter(rating<2) %>% sample_n(2),
                       ic_data_rank %>% filter(brand=="breyers")   %>% filter((rating>=2)&(rating<=4.2)) %>% sample_n(4))

talenti_samp <- ic_data_rank %>% filter(brand=="talenti") %>% sample_n(12)
talenti_samp2 <- ic_data_rank %>% filter(brand=="talenti")   %>% filter(rating<=4.2) %>% sample_n(6)


bj_samp <- rbind(bj_data, bj_samp2)

##-----


bj_samp <- bj_samp %>% mutate(sugar = c(30, 33, 34, 36, 35, 33, 33, 31, 36, 37, 31, 35, 33, 33, 36, 30, 32, 33),
                              serving_size = c(137, 138, 138, 141, 145, 138, 138, 143, 141, 142, 135, 142,140, 140, 143, 135, 135, 140))
## Id: 50, 50, 41, 3, 18, 24

sug1 <- c(33, 33, 36, 30, 32, 33)
serv1 <- c(140, 140, 143, 135, 135, 140)
sug1/serv1

breyers_samp <- breyers_samp %>% mutate(sugar = c(11, 18, 17, 11, 4,18, 22, 18, 17, 17, 7, 7, 21, 21, 17, 15, 7, 19),
                                        serving_size = c(50, 83, 86, 50, 81, 87, 88, 88, 81, 82, 90, 90, 89, 89, 82, 82, 91, 88))
## ID: 49, 49, 64, 15, 8, 0
sug2 <- c(21, 21, 17, 15, 7, 19)
serv2 <- c(89, 89, 82, 82, 91, 88)
sug2/serv2

talenti_samp <- talenti_samp %>% mutate(sugar = c(33, 37, 35, 33, 30, 28, 31, 27, 33, 30, 33, 34,30, 30, 32, 26, 30, 26),
                                        serving_size = c(127, 131, 131, 127, 128, 118, 129, 127, 133, 128, 127, 117,128, 128, 126, 133, 128, 133))
## ID: 23, 23, 21, 42, 23, 42
sug3 <- c(30, 30, 32, 26, 30, 26)
serv3 <- c(128, 128, 126, 133, 128, 133)
sug3/serv3

ice_cream_ratings <- rbind(bj_samp, breyers_samp, talenti_samp) %>% 
                      mutate(sugar_content = sugar/serving_size) %>%
                      select(c(brand, rating, sugar_content))
##-------- Analysis

IC_data <- ice_cream_data %>% mutate(brand_cat = factor(brand))

MLR_IC <- lm(rating ~ sugar_content + brand_cat , IC_data)
summary(MLR_IC)
IC_data_all <- IC_data %>% mutate(resids = MLR_IC$residuals,
                                  fits = MLR_IC$fitted.values)
ggplot(IC_data_all, aes(x=fits, y=resids)) + 
  geom_point() + 
  geom_hline(yintercept=0) + 
  labs(title="Ice Cream Residual Plot", x="Fitted values", y="Residuals")

#plot(IC_data_all$fits, IC_data_all$resids)
#identify(IC_data_all$fits, IC_data_all$resids)
# observation 13, 16, and 30 

ggplot(IC_data_all, aes(sample=scale(resids))) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for residuals")



#bj_data <- IC_data %>% filter(brand=="bj")
SLR_bj <- lm(rating ~ sugar_content, bj_data)
summary(SLR_bj)
bj_data_all <- bj_data %>% mutate(resids = SLR_bj$residuals,
                                  fits = SLR_bj$fitted.values)
ggplot(bj_data_all, aes(x=fits, y=resids)) + 
  geom_point()

#breyers_data <- IC_data %>% filter(brand=="breyers")
SLR_breyers <- lm(rating ~ sugar_content, breyers_data)
summary(SLR_breyers)
breyers_data_all <- breyers_data %>% mutate(resids = SLR_breyers$residuals,
                                            fits = SLR_breyers$fitted.values)
ggplot(breyers_data_all, aes(x=fits, y=resids)) + 
  geom_point()

talenti_data <- IC_data %>% filter(brand=="talenti")
SLR_talenti <- lm(rating ~ sugar_content, talenti_data)
summary(SLR_talenti)
talenti_data_all <- talenti_data %>% mutate(resids = SLR_talenti$residuals,
                                            fits = SLR_talenti$fitted.values)
ggplot(talenti_data_all, aes(x=fits, y=resids)) + 
  geom_point()
