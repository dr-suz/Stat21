set.seed(101)
x <- runif(200, 0, 35)
y1 <- 3.1*x + rnorm(200,0,1)
slr1 <- lm(y1~x)

y2 <- 3.1*(x^2) + rnorm(200,0,50)
slr2 <- lm(y2~x)

y3 <- 3.1*sqrt(x) + rnorm(200,0,1)
slr3 <- lm(y3~x)

y4 <- -3.1*log(x) + rnorm(200,0,1)
slr4 <- lm(y4~x)

ex.data <- tibble(x, y1, resid1 = slr1$residuals, fit1 = slr1$fitted.values, 
                  y2, resid2 = slr2$residuals, fit2 = slr2$fitted.values, 
                  y3, resid3 = slr3$residuals, fit3 = slr3$fitted.values, 
                  y4, resid4 = slr4$residuals, fit4 = slr4$fitted.values)

ggplot(ex.data, aes(x=fit1, y=resid1)) + 
  geom_point() + 
  labs(x="Fitted values", y="Residuals")

ggplot(ex.data, aes(x=fit2, y=resid2)) + 
  geom_point() + 
  labs(x="Fitted values", y="Residuals")

ggplot(ex.data, aes(x=fit3, y=resid3)) + 
  geom_point() + 
  labs(x="Fitted values", y="Residuals")

ggplot(ex.data, aes(x=fit4, y=resid4)) + 
  geom_point() + 
  labs(x="Fitted values", y="Residuals")
