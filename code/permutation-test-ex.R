#' ---
#' title: Code for a two-sample permutation test
#' author: Math 445
#' date: Spring 2016
#' ---

#' ### Getting set up

#' Loading the necessary packages
library(dplyr)
library(ggplot2)

#' Loading the data
mosquitoes <- read.table("https://raw.githubusercontent.com/math445-LU/2016/master/data/mosquitoes-beer.csv",
                         sep = ",", header = TRUE)

#' ### Calculating the observed difference in means
trt_means <- 
  mosquitoes %>%
  group_by(treatment) %>%
  summarise(avg = mean(count))

observed <- trt_means$avg[1] - trt_means$avg[2]
observed

#' ### Option 1: Using a for loop to run the permutations
#' Running the permutations
N <- 10^5 - 1
result <- numeric(N)
for(i in 1:N) {
  index <- sample(nrow(mosquitoes), size = 25, replace = FALSE)
  result[i] <- mean(mosquitoes$count[index]) - mean(mosquitoes$count[-index])
}

# Plotting the null dsn
null_dsn <- data.frame(stat = result)
ggplot(data = null_dsn) + 
  geom_histogram(mapping = aes(x = stat)) + 
  geom_vline(xintercept = observed)

# Calculating the p-value
(sum(result >= observed) + 1) / (N + 1)


#' ### Option 2: Using replicate to run the permutations
N <- 10^5 - 1

#' Writing a function that runs the permutations
permute <- function(values, ngrp1) {
  index <- sample(length(values), size = ngrp1, replace = FALSE)
  stat <- mean(values[index]) - mean(values[-index])
  return(stat)
}

#' Running the permutations
result <- replicate(N, permute(values = mosquitoes$count, ngrp1 = 25))

#' Plotting the null dsn
null_dsn <- data.frame(stat = result)
ggplot(data = null_dsn) + 
  geom_histogram(mapping = aes(x = stat)) + 
  geom_vline(xintercept = observed)

#' Calculating the p-value
(sum(result >= observed) + 1) / (N + 1)
