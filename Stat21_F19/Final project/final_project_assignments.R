#### random assignments

library("tidyverse")
names <- read_table2("student_names.txt", col_names=FALSE)
first_name <- names[,1]
View(first_name)

datasets_no <- sample(1:112, size=29, replace=FALSE)
assignments1 <- cbind(first_name, datasets_no)

assignments1[,2]


new.data <- setdiff(1:112, datasets_no)
set.seed(100)
datasets_no2 <- sample(new.data, size=(29-10), replace=FALSE)
new.data2 <- setdiff(1:112, c(datasets_no2,datasets_no))
datasets_no3 <- sample(new.data2, size=(29-10), replace=FALSE)

#### checking specific packages

library("datasets")
install.packages("fivethirtyeight")
library("fivethirtyeight")

names(candy_rankings)
write.csv(candy_rankings, 
          file="~/Google Drive Swat/Swat docs/Stat 21/Data/candy_rankings.csv")

maternal_mort <- read_csv("~/Google Drive Swat/Swat docs/Stat 21/Data/Maternal_mortality.csv", col_names = TRUE)

AFL_attendance <- read_csv("~/Google Drive Swat/Swat docs/Stat 21/Data/AFL_crowd_attendance.csv", col_names = TRUE)


dim(maternal_mort[((maternal_mort$`Country Name`=="India")|
                   (maternal_mort$`Country Name`=="Japan")|
                   (maternal_mort$`Country Name`=="China")|
                   (maternal_mort$`Country Name`=="Russian Federation")|
                   (maternal_mort$`Country Name`=="Vietnam")),])

maternal_mort_asia <- maternal_mort[((maternal_mort$`Country Name`=="India")|
                 (maternal_mort$`Country Name`=="Japan")|
                 (maternal_mort$`Country Name`=="China")|
                 (maternal_mort$`Country Name`=="Russian Federation")|
                 (maternal_mort$`Country Name`=="Vietnam")),]

maternal_mort_asia_and_more <- maternal_mort[((maternal_mort$`Country Name`=="India")|
                                                (maternal_mort$`Country Name`=="Japan")|
                                                (maternal_mort$`Country Name`=="China")|
                                                (maternal_mort$`Country Name`=="Russian Federation")|
                                                (maternal_mort$`Country Name`=="Vietnam")|
                                                (maternal_mort$`Country Name`=="Fragile and conflict affected situations")|
                                                (maternal_mort$`Country Name`=="IDA blend")),]

maternal_mort_americas <-  maternal_mort[((maternal_mort$`Country Name`=="Mexico")|
                                            (maternal_mort$`Country Name`=="United States")|
                                            (maternal_mort$`Country Name`=="Chile")|
                                            (maternal_mort$`Country Name`=="Argentina")|
                                            (maternal_mort$`Country Name`=="Canada")),]

write.csv(maternal_mort_asia_and_more, 
          file="~/Google Drive Swat/Swat docs/Stat 21/Data/maternal_mortality_asia_and_more.csv", col.names = TRUE)
write.csv(maternal_mort_americas, 
          file="~/Google Drive Swat/Swat docs/Stat 21/Data/maternal_mortality_americas.csv", col.names = TRUE)


bechdel1 <- bechdel %>% filter(year <= 2000) 
bechdel2 <- bechdel %>% filter(year > 2000)

write.csv(bechdel1, 
          file="~/Google Drive Swat/Swat docs/Stat 21/Data/bechdel_old.csv",
          row.names = TRUE)

spi_english <- spi_global_rankings[
  ((spi_global_rankings$league=="English League Championship")|
  (spi_global_rankings$league=="English League One")|
  (spi_global_rankings$league=="English League Two")),]

write.csv(spi_english, 
          file="~/Google Drive Swat/Swat docs/Stat 21/Data/spi_english.csv",
          col.names = TRUE)


spi_spanish <- spi_global_rankings[
  ((spi_global_rankings$league=="Spanish Primera Division")|
     (spi_global_rankings$league=="Spanish Segunda Division")),]

spi_americas <- spi_global_rankings[
  ((spi_global_rankings$league=="Argentina Primera Division")|
     (spi_global_rankings$league=="Brasileiro Série A")|
     (spi_global_rankings$league=="Mexican Primera Division Torneo Apertura")),]


write.csv(spi_americas, 
          file="~/Google Drive Swat/Swat docs/Stat 21/Data/spi_americas.csv",
          col.names = TRUE)

write.csv(CO2, 
          file="~/Google Drive Swat/Swat docs/Stat 21/Data/co2.csv")

###-----
install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/world-bank/sh.sta.mmrt/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  i=2
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}


####-------

names <- c("Adora","Ankur","Christina","Colin","Daniel","Hunter","John" ,
           "Misha" ,"Noah" ,"Rachel")
length(names)
sample(1:10, replace=FALSE)
