SAT_data <- read_table2(url("http://www.swarthmore.edu/NatSci/sthornt1/DataFiles/SAT_data2.txt"))

SAT_data <- SAT_data %>% mutate(Income_fct = fct_inorder(Income))
SAT_anova <- aov(SAT_score ~ Income_fct, SAT_data)

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

TukeyHSD(SAT_anova)
TukeyHSD(SAT_anova, conf.level = 0.9)


## Combine two largest categories! 
SAT_data_collapsed <- SAT_data %>% 
                      mutate(Income_fct_new = 
                               fct_collapse(SAT_data$Income_fct, ">140K" = c("140K-160K","160K-200K",">200K")))

ggplot(SAT_data_collapsed, aes(x=Income_fct_new, y=SAT_score)) +
  geom_boxplot() + 
  labs(title="Boxplot of SAT scores over income levels")

SAT_data8.2 <- SAT_data_collapsed %>% filter(Income_fct_new==">140K")
ggplot(SAT_data8.2, aes(sample=SAT_score)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title="Normal prob plot for >= $140K")

SAT_anova_collapsed <- aov(SAT_score ~ Income_fct_new, SAT_data_collapsed)
TukeyHSD(SAT_anova_collapsed)
TukeyHSD(SAT_anova_collapsed, conf.level = 0.90)
