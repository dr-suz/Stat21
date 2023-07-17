library(tidyverse)
income_SAT <- tibble(income = factor(c("<20K", "20K-40K", "40K-60", "60K-80K",
                                       "80K-100K", "100K-120K", "120K-140K", "140K-160K",
                                       "160K-200K", ">200K")),
                     median_SAT = c(1323, 1398, 1461, 1503, 1545, 1580, 1594,
                                    1619, 1636, 1721))
set.seed(101)
income1_reading = rnorm(109651, 437, 104); income1_math = rnorm(109651, 460, 117); income1_writing = rnorm(109651, 432, 102); income1_total = round(income1_reading + income1_math + income1_writing, 0)

income2_reading = rnorm(150390, 465, 102); income2_math = rnorm(150390, 479, 108); income2_writing = rnorm(150390, 455, 99); income2_total = round(income2_reading + income2_math + income2_writing, 0)

income3_reading = rnorm(141307, 490, 102); income3_math = rnorm(141307, 500, 105); income3_writing = rnorm(141307, 478, 99); income3_total = round(income3_reading + income3_math + income3_writing, 0)

income4_reading = rnorm(135872, 504, 101); income4_math = rnorm(135872, 514, 104); income4_writing = rnorm(135872, 492, 100); income4_total = round(income4_reading + income4_math + income4_writing, 0)

income5_reading = rnorm(119051, 518, 101); income5_math = rnorm(119051, 529, 103); income5_writing = rnorm(119051, 505, 100); income5_total = round(income5_reading + income5_math + income5_writing, 0)

income6_reading = rnorm(100383, 528, 102); income6_math = rnorm(100383, 541, 104); income6_writing = rnorm(100383, 518, 101); income6_total = round(income6_reading + income6_math + income6_writing, 0)

income7_reading = rnorm(51194, 533, 102); income7_math = rnorm(51194, 546, 104); income7_writing = rnorm(51194, 523, 101); income7_total = round(income7_reading + income7_math + income7_writing, 0)

income8_reading = rnorm(38997, 540, 102); income8_math = rnorm(38997, 554, 104); income8_writing = rnorm(38997, 531, 102); income8_total = round(income8_reading + income8_math + income8_writing, 0)

income9_reading = rnorm(45567, 547, 102); income9_math = rnorm(45567, 561, 105); income9_writing = rnorm(45567, 540, 104); income9_total = round(income9_reading + income9_math + income9_writing, 0)

income10_reading = rnorm(69907, 568, 104); income10_math = rnorm(69907, 586, 105); income10_writing = rnorm(69907, 567, 106); income10_total = round(income10_reading + income10_math + income10_writing, 0)

income_SAT1 <- tibble(income = factor(c(rep("<20K", length(income1_total)), 
                                        rep("20K-40K", length(income2_total)),
                                        rep("40K-60K", length(income3_total)),
                                        rep("60K-80K", length(income4_total)),
                                        rep("80K-100K", length(income5_total)),
                                        rep("100K-120K", length(income6_total)),
                                        rep("120K-140K", length(income7_total)),
                                        rep("140K-160K", length(income8_total)),
                                        rep("160K-200K", length(income9_total)),
                                        rep(">200K", length(income10_total)))),
                      SAT_score = c(income1_total, income2_total, income3_total, 
                                    income4_total, income5_total, income6_total, 
                                    income7_total, income8_total,income9_total, 
                                    income10_total))

income_SAT1_samp <- income_SAT1[sample(1:(length(income_SAT1$SAT_score)), 100),] 
#anova_mod <- aov(SAT_score ~ income, income_SAT1_samp)
#summary(anova_mod)
#TukeyHSD(anova_mod)


write.table(income_SAT1_samp, file="SAT_data.txt", row.names=FALSE, col.names=TRUE)

write_csv(income_SAT1_samp, path="SAT_data.csv")
