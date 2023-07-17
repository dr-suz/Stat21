library("tidyverse")
library("gridExtra")

exam2_data <- read_csv("~/Google Drive Swat/Swat docs/Stat 21/Exam2/exam2_grades.csv", col_names=TRUE)
colnames(exam2_data) <- c("student", "overall", "Q1", "Q2", "Q3", "Q4", "Q5")
head(exam2_data)

plot1 <- ggplot(exam2_data) 
#plot2 <- ggplot(exam2_data %>% mutate(adj_overall = overall+8))
my_grobs = list(plot1 + geom_boxplot(aes(y=Q1)) + ylim(0,10),
                plot1 + geom_boxplot(aes(y=Q2)) + ylim(0,24),
                plot1 + geom_boxplot(aes(y=Q3)) + ylim(0,16),
                plot1 + geom_boxplot(aes(y=Q4)) + ylim(0,30),
                plot1 + geom_boxplot(aes(y=Q5)) + ylim(0,30),
                plot1 + geom_boxplot(aes(y=overall)) + ylim(40,100))

lay <- rbind(c(1,2,3),
             c(4,5,6))

dev.new()
grid.arrange(grobs = my_grobs, layout_matrix = lay)


