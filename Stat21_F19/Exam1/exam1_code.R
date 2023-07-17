library("tidyverse")
setwd("~/Google Drive Swat/Swat docs/Stat 21/Data")

### Diamond data 
diamond_dat <- read_table2("diamond_dat.txt",skip=2, col_names = FALSE)
colnames(diamond_dat) <- c("size", "price")

ggplot(diamond_dat, aes(x=size, y=price)) +
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE) +
  labs(title="Simple Linear Reguression", subtitle="Diamond size as a predictor of diamond price",
       x="Size \n (carats)", y="Price \n (Singapore dollars") 

diamond_mod <- lm(price~size, data=diamond_dat)
diamond_mod_summary <- summary(diamond_mod)

residuals_QQ <- ggplot() + aes(sample=diamond_mod_summary$residuals) + stat_qq() + stat_qq_line()  
ypred_QQ <- ggplot() + aes(sample=diamond_mod$fitted.values) + stat_qq() + stat_qq_line()  
yobs_QQ <- ggplot() + aes(sample=diamond_dat$price) + stat_qq() + stat_qq_line()  

resid_x_plot <- ggplot() + aes(x=diamond_dat$size, y=diamond_mod_summary$residuals) +
  geom_point() + 
  labs(x="Size \n (carats)", y="Residuals") 

resid_ypred_plot <- ggplot() + aes(x=diamond_mod$fitted.values, y=diamond_mod_summary$residuals) +
  geom_point() + 
  labs(x="Predicted price", y="Residuals") 

#-------
diamond_dat_trans <- diamond_dat %>% mutate(grams = size*.2)
diamond_mod_trans <- lm(price~grams, data=diamond_dat_trans)
diamond_mod_summary_trans <- summary(diamond_mod_trans)

ggplot() + aes(sample=diamond_mod_summary_trans$residuals) + stat_qq() + stat_qq_line()  
ggplot() + aes(sample=diamond_mod_trans$fitted.values) + stat_qq() + stat_qq_line()  
ggplot() + aes(sample=diamond_dat_trans$price) + stat_qq() + stat_qq_line()  

ggplot() + aes(x=diamond_dat_trans$grams, y=diamond_mod_summary_trans$residuals) +
  geom_point() + labs(x="Size \n (grams)", y="Residuals") 

ggplot() + aes(x=diamond_mod_trans$fitted.values, y=diamond_mod_summary_trans$residuals) +
  geom_point() + labs(x="Predicted price", y="Residuals") 


### Running data 
run_dat <- read_table2("Four-Mile-Run-data.txt")
head(run_dat)

summary(lm(calories~aveSpeed, run_dat))
