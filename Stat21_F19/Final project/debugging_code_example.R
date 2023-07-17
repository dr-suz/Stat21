##########Working example##########

simp.fun <- function(my.data.frame, my.vec){
  
  if(class(my.data.frame)!="data.frame"){
    return("Error: first input must be a data frame")}
  if(length(my.data.frame[,1])!=length(my.vec)){
    return("Error: the dimensions of the input do not match")}
  
  return("No errors to report")
}


example.df <- as.data.frame(matrix(rnorm(500, 4, 1), nrow=5))
example.vec <- rnorm(5,3,5)
simp.fun(example.df, example.vec)





#######Debugging############
simp.fun <- function(my.data.frame, my.vec){
  
#  my.data.frame <- as.data.frame(matrix(rnorm(500, 4, 1), nrow=5))
#  my.vec <- rnorm(5,3,5)
  
  if(class(my.data.frame)!="data.frame"){
    return("Error: first input must be a data frame")}
  if(length(my.data.frame[ , 1])!=length(my.vec)){
    return("Error: the dimensions of the input do not match")}
  
  return("No errors to report")
}



example.df <- as.data.frame(matrix(rnorm(500, 4, 1), nrow=5))
example.vec <- rnorm(5,3,5)
simp.fun(example.df, example.vec)

dim(example.df)
dim(example.vec)
length(example.vec)



remove(my.data.frame,my.vec)







#########Class type testing##########
## Not my recommended method (can use if you want)
ex.tibble <- as.tibble(matrix(rnorm(500, 4, 1), nrow=5))
class(ex.tibble)
(class(ex.tibble)[2]=="tbl")


## My recommended method 
ex.df <- as.data.frame(matrix(rnorm(500, 4, 1), nrow=5))
class(ex.df)










