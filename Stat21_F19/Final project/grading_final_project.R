library("devtools")
library("tidyverse")

pck_name <- "RegAnalysis"
devtools::install(pck_name)
library("RegAnalysis")

setwd("~/Google Drive Swat/Swat docs/Stat 21/Final project/Submissions")
lib_path <- "/Users/m/Library/R/3.6/library"
## ---> Documentation:  

##---- test on the msleep data set ----------------------------------
msleep2 <- msleep %>% 
           select(order,sleep_total,brainwt) %>% 
           filter(((order=="Carnivora")|(order=="Primates")|(order=="Rodentia"))) %>% 
           mutate(order_cat = fct_infreq(order)) %>%
           na.omit()  
input1 <- msleep2 %>% select(brainwt, order_cat) %>% data.frame() 
input2 <- msleep2 %>% select(sleep_total) %>% as_vector() 

## function 1  --->  
check.varb.types(as_vector(msleep2$order_cat))  
##factor 
check.varb.types(as_vector(msleep2$brainwt))  
##double
check.varb.types(as_vector(msleep2$sleep_total))  
##double 

## function 2 --->  
good.fit1(input1, input2, 0.01)  
## pvalue = 0.08472, fail to reject

## function 3 --->  
good.fit2(input1, input2)  
## 15.38%

## function 4 --->  
estimated.lm(input1, input2)  
## brainwt -3.2556 1.9797
## order_catCarnivora -2.235 1.212
## order_catPrimates  -0.8032 1.2215

## function 5 ---> 
sig.predictor.test(input1, input2, 1, 0.01)  
## pvalue = 0.1143, fail to reject 

## function 6 --->  
resid_plot(input1, input2)


##--- test on package included data set ------------------------------

test_data <- %>% na.omit()
class(test_data)
head(test_data)
  test_input1 <- 
  test_input2 <- 

## ---> Dataset: 
  
## function 1  --->
check.varb.types(as_vector(test_input1$order_cat))   
check.varb.types(as_vector(test_input1$brainwt))   
check.varb.types(as_vector(test_input2))  ##double 

## function 2  --->
good.fit1(test_input1, test_input2, 0.01)  

## function 3  --->
good.fit2(test_input1, test_input2)  

## function 4  --->
estimated.lm(test_input1, test_input2)  

## function 5  --->
sig.predictor.test(test_input1, test_input2, input3, 0.01)  

## function 6 --->
resid_plot(test_input1, test_input2)



###########
remove.packages(pck_name)
# uninstall(pck_name)
# Uses remove.package to uninstall the package. To uninstall a package from a non-default library, use withr::with_libpaths().


