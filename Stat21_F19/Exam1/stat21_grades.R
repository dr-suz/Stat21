library("tidyverse")
library("gridExtra")

exam1_data <- read_csv("Exam grades.csv", col_names=TRUE)
colnames(exam1_data) <- c("student", "overall", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7")
head(exam1_data)

plot1 <- ggplot(exam1_data) 
plot2 <- ggplot(exam1_data %>% mutate(adj_overall = overall+8))
my_grobs = list(plot2 + geom_boxplot(aes(y=Q1)) + ylim(0,10),
                plot2 + geom_boxplot(aes(y=Q2)) + ylim(0,10),
                plot2 + geom_boxplot(aes(y=Q3)) + ylim(0,10),
                plot2 + geom_boxplot(aes(y=Q4)) + ylim(0,10),
                plot2 + geom_boxplot(aes(y=Q5)) + ylim(0,10),
                plot2 + geom_boxplot(aes(y=Q6)) + ylim(0,25),
                plot2 + geom_boxplot(aes(y=Q7)) + ylim(0,25),
                plot2 + geom_boxplot(aes(y=adj_overall)) + ylim(50,100))

lay <- rbind(c(1,2,3,4,5),
             c(NA,NA,6,7,NA),
             c(NA,NA,8,NA,NA))
dev.new()
grid.arrange(grobs = my_grobs, layout_matrix = lay)

View(exam1_data)
min(exam1_data$Q5)
sort(exam1_data$overall)
sort(exam1_data$overall + 8)
# exam1_data %>% mutate(adj_overall = overall+8) %>% filter(adj_overall<70)

exam1_data$student[exam1_data$overall + 8 <70]

my_grade <- function(hw, participation, exam1, exam2, final){
  return(100*(0.3*hw + 0.05*participation + 0.2*exam1 + 0.2*exam2 + 0.25*final))
}

my_grade(hw=0.97, participation=1, exam1=0.60, exam2=0.80, final=0.75)

