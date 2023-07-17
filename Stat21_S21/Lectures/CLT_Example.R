## CLT Applied to the Sample Means of Many SRS from Binomial population
set.seed(100)  ## This makes sure R startes generating random numbers from the sampe spot, that way we will all get the same (although random) draws

library("tidyverse") # Make sure you install this R packaged by going to Tools -> Install Packages before you call it into your R session by running this line of code


# Here I've created a data set called `many_SRS_from_Bern` that has 10,000 columns corresponding to 10,000 different random samples from a $Bernoulli(p)$ distribution. 
n <- 10e2
numb_of_samples <- 10e3
pr_success <- 0.3
sample <- matrix(rep(NA,n*numb_of_samples), ncol=numb_of_samples)
for(i in 1:numb_of_samples){
  sample[,i] <- rbinom(n, 1, pr_success)   # This specifies we are randomly drawing (one) sample of size $n$ from a Binomial population 
}
# Each of these different data sets in the columns of of `sample` contains $n=1000$ observations of either a success $(1)$ or a failure $(0)$.


# Here I've computed $\hat{p}$ (the sample mean of many observations of 0's and 1's) for each of these 10,000 samples and stored these values in a tibble called `sampling_proportions`

many_SRS_from_Bern <- sample
sample_proportions <- rep(NA, numb_of_samples)   
for(i in 1:numb_of_samples){      
  sample_proportions[i] <- mean(many_SRS_from_Bern[ ,i]) 
}
sampling_proportions <- tibble(phat = sample_proportions)

# Now we can plot the histogram of all of the observed sample_proportions 
ggplot(data=sampling_proportions, aes(x=phat)) + 
  geom_histogram(aes(y=..density..))+ 
  labs(title="Sampling distribution of the sample proportion", subtitle = "n=1000", x="Sample p_hat values", y="Density") 



## Exercise: Try making $n$ a smaller number than 10e2 and run the code again. What changes do you see in the histogram? 
##           Next try playing around with different values for pr_success and see how this changes the histogram. Do you encounter any error messages?