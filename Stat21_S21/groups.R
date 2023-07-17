##Create groups of 3-4 randomly from roster 

roster <- readxl::read_excel("/Users/sthornt1/Google Drive/Swat docs/Stat021/Stat21_S21/roster.xlsx", col_names = FALSE)
colnames(roster) = c("email", "section")
library(tidyverse)
sec1 <- roster %>% filter(section=="sec1")
sec2 <- roster %>% filter(section=="sec2")

## Note it looks like one student is missing from Section 2 
set.seed(420)
(sec1_groups <- sample(sec1$email, length(sec1$email), replace=FALSE))
(sec2_groups <- sample(sec2$email, length(sec2$email), replace=FALSE))
