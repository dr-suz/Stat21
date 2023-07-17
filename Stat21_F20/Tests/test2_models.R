pizza <- read_table2("pizza.txt", col_names=TRUE, na= " ")
names(pizza)  

mod_pizza <- lm(Calories ~ Fat  + factor(Type), pizza)
summary(mod_pizza)


pizza_all <- pizza %>% mutate(resids = mod_pizza$residuals,
                                  fits = mod_pizza$fitted.values)
ggplot(pizza_all, aes(x=fits, y=resids)) + 
  geom_point()
ggplot(pizza_all, aes(sample=scale(resids))) +
  stat_qq() + 
  stat_qq_line() 
##--------------


coasters <- read_csv("coasters.csv")
names(coasters)

coasters1 <- coasters %>% filter(!is.na(Drop) & !is.na(Duration)) %>% 
                          mutate(Track_cat = factor(Track),
                                 Inversions_cat = factor(Inversions))
mod_MLR <- lm(Speed ~ Track_cat + Height + Duration + Length + Drop + Inversions_cat, coasters1)
summary(mod_MLR)$sigma
coasters1_all <- coasters1 %>% mutate(resids = mod_MLR$residuals,
                                      fits = mod_MLR$fitted.values)
ggplot(coasters1_all, aes(x=fits, y=resids)) + 
  geom_point()
ggplot(coasters1_all, aes(sample=scale(resids))) +
  stat_qq() + 
  stat_qq_line() 

boxcox(mod_MLR, lambda=seq(-3,3))
lambda = 1.8
#coasters2 <- coasters1 %>% mutate(speed2 = (Speed^lambda-1)/lambda)
#mod_MLR2 <- lm(speed2 ~ Track_cat + Height + Duration + Length + Drop + Inversions_cat,coasters2)
#summary(mod_MLR2)$sigma
#coasters2_all <- coasters2 %>% mutate(resids = mod_MLR2$residuals,
#                                      fits = mod_MLR2$fitted.values)
#ggplot(coasters2_all, aes(x=fits, y=resids)) + 
#  geom_point()
#ggplot(coasters2_all, aes(sample=resids)) +
#  stat_qq() + 
#  stat_qq_line() 
#library('faraway')




coasters_steel <- coasters1 %>% filter(Inversions==0)#filter(Track=="Steel")
mod_steel <- lm(Speed ~ Height + Duration + Length + Drop, coasters_steel)
summary(mod_steel)$sigma
steel_all <- coasters_steel %>% mutate(resids = mod_steel$residuals,
                                      fits = mod_steel$fitted.values)
ggplot(steel_all, aes(x=fits, y=resids)) + 
  geom_point()



coasters_wood <- coasters1 %>% filter(Inversions==1)#filter(Track=="Wood")
mod_wood <- lm(Speed ~ Height + Duration + Length + Drop, coasters_wood)
summary(mod_wood)$sigma
wood_all <- coasters_wood %>% mutate(resids = mod_wood$residuals,
                                       fits = mod_wood$fitted.values)
ggplot(wood_all, aes(x=fits, y=resids)) + 
  geom_point()

