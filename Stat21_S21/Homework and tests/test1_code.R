set.seed(404)

###

success <- c(31133, 12005)
n <- c(72402, 27604)
prop.test(success, n)

100006 - 72402

100006*(0.8 + rnorm(1, 0, 0.1))

72402*.43

27604*(.43 + rnorm(1,0,0.001))


####

total_GNC = 2000
total_CW = 49163
total_CW*(0.2+rnorm(1, 0, 0.1))
#8809 supervisors

total_CM = 48843
total_CM*(0.2+rnorm(1, 0, 0.1))
#5471 supervisors

gender.data <- matrix(c(1994, 6, 40354, 8809, 43372, 5471), byrow=TRUE, ncol=2) 
chisq.test(gender.data)


#####
library("truncnorm")

set.seed(404)
N = 1835+1200
prop.white = rtruncnorm(N,a=0, b=Inf,66,18)
prop.black = rtruncnorm(N,a=0, b=Inf,14,13)
prop.hispanic = rtruncnorm(N,a=0, b=Inf,13,13)
prop.frpl = rtruncnorm(N,a=0, b=Inf,42,10)

school_data = tibble(Hispanic = prop.hispanic*N, 
                     Black = prop.black*N,
                     White = prop.white*N)

school_data
dim(school_data)
summary(school_data)
n = length(school_data)


District2000 = 7982

District2015 = 11977

n = c(7982, 11877)

prop_black = c(0.345, 0.499)
prop_white = c(0.308, 0.761)

success_black = prop_black*n
success_white = prop_white*n

prop.test(success_black, n)

prop.test(success_white, n)


n_new <- c(11877,11877)
success_new <- n_new*c(0.499, 0.761)
prop.test(success_new, n_new)



school_data <- read_table2(url("http://www.swarthmore.edu/NatSci/sthornt1/DataFiles/school_segregation_rates.txt"))
head(school_data)


# difference in the proportion of white vs black, white vs hispanic, black vs hispanic
# plot histograms of the differences 
# estimate how large the differences are at a 0.95 level 
# find standard error of prop white, black, hispanic alone, then find standard error of the three differences 
# explain why we'd want to look at the differenced data 

# is theproportion of frpl different from the proportion in XXXX 

write.table(round(school_data,2), file="school_segregation_rates.txt", row.names=FALSE)
