## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
  tidy=FALSE,     # display code as typed
  size="footnotesize",   # slightly smaller font for code
  fig.width=5,
  fig.height=3
)

library(ggplot2)

## ----cache=TRUE----------------------------------------------------------
flights <- read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/FlightDelays.csv")

## ----cache=TRUE----------------------------------------------------------
# Grab the delays from each airline
aa_delay <- subset(flights, select = Delay, subset = Carrier=="UA", drop = T)
ua_delay <- subset(flights, select = Delay, subset = Carrier=="AA", drop = T)

# Calculating the observed proportion for AA
mean(aa_delay > 20)

# Calculating the observed proportion for UA
mean(ua_delay > 20)

# Calculating the observed difference in proportions
observed <- mean(aa_delay > 20) - mean(ua_delay > 20)
observed

## ----cache=TRUE----------------------------------------------------------
# Extract the column of interest
delays <- flights$Delay

# Initialize everything
n <- length(delays)
m <- length(aa_delay)
N <- 10^4 - 1  #set number of times to repeat this process
result <- numeric(N)

# Create the permutation distributions
for(i in 1:N) {
  index <- sample(n, m, replace = FALSE)
  result[i] <- mean(delays[index]>20) - mean(delays[-index] > 20)
}

## ----cache=TRUE----------------------------------------------------------
ggplot(data = data.frame(result)) + 
  geom_histogram(mapping = aes(x = result), colour = "gray20") + 
  geom_vline(xintercept = observed, colour = "orange") +
  xlab("Difference in proportions") + 
  ggtitle("Delay times > 20 minutes")

## ------------------------------------------------------------------------
2 * (sum(result >= observed) + 1) / (N + 1)

## ----cache=TRUE----------------------------------------------------------
# Calculating the observed statistic
observedb <- var(ua_delay) / var(aa_delay)
observedb

# Initialize everything
n <- length(delays)
m <- length(aa_delay)
N <- 10^4 - 1  #set number of times to repeat this process
resultb <- numeric(N)

# Create the permutation distributions
for(i in 1:N) {
  index <- sample(n, m, replace = FALSE)
  resultb[i] <- var(delays[-index]) / var(delays[index]) # var(UA) / var(AA)
}

# Plot the permutation distribution
ggplot(data = data.frame(resultb)) + 
  geom_histogram(mapping = aes(x = resultb), colour = "gray20") + 
  geom_vline(xintercept = observed, colour = "orange") +
  xlab("Ratio of variances") + 
  ggtitle("Variance of delay times")

# calculate the p-value
(sum(resultb >= observedb) + 1) / (N + 1)

## ----cache=TRUE----------------------------------------------------------
N <- 10^4 - 1

# Define delays, ua_delay, and aa_delay as in the previous problem

# Calculate the observed statistics
observed_sum_ua <- sum(ua_delay)
observed_mean_ua <- mean(ua_delay)
observed_mean_diff <- mean(ua_delay) - mean(aa_delay)

# Initialize everything for the permutations
n <- length(delays)
m <- length(ua_delay)  #number of UA observations
sum_ua <- numeric(N)
mean_ua <- numeric(N)
mean_diff <- numeric(N)

# Run the permutations
for (i in 1:N) {
  index <- sample(n,  m, replace = FALSE)
  sum_ua[i] <- sum(delays[index])
  mean_ua[i] <- mean(delays[index])
  mean_diff[i] <- mean(delays[index]) - mean(delays[-index])
}

## ----fig.width = "\\textwidth", warning=FALSE, message=FALSE-------------
p1 <- ggplot(data = data.frame(sum_ua)) + 
  geom_histogram(mapping = aes(x = sum_ua), colour = "gray20") + 
  geom_vline(xintercept = observed_sum_ua, colour = "orange") +
  xlab("Total delay") + 
  ggtitle("Total UA delay")

p2 <- ggplot(data = data.frame(mean_ua)) + 
  geom_histogram(mapping = aes(x = mean_ua), colour = "gray20") + 
  geom_vline(xintercept = observed_mean_ua, colour = "orange") +
  xlab("Mean delay") + 
  ggtitle("Mean UA delay")

p3 <- ggplot(data = data.frame(mean_diff)) + 
  geom_histogram(mapping = aes(x = mean_diff), colour = "gray20") + 
  geom_vline(xintercept = observed_mean_diff, colour = "orange") +
  xlab("Difference of means") + 
  ggtitle("Difference of means")

library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

## ------------------------------------------------------------------------
# Calculate the two-sided p-values
2 * (sum(sum_ua <= observed_sum_ua) + 1) / (N + 1)  
2 * (sum(mean_ua <= observed_mean_ua) + 1) / (N + 1)
2 * (sum(mean_diff <= observed_mean_diff) + 1) / (N + 1)

## ------------------------------------------------------------------------
cereals <- read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/Cereals.csv")

with(cereals, table(Age, Shelf))

## ------------------------------------------------------------------------
result_12b <- chisq.test(cereals$Age, cereals$Shelf)
result_12b

## ------------------------------------------------------------------------
result_12b$expected

## ----cache=TRUE----------------------------------------------------------
# Define the chisq function giveen on page 56 of the textbook
chisq <- function(obs) {
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  RES <- sum((obs - expected)^2 / expected)
  return(RES)
}

# Calculate the observed counts
observed <- chisq(table(cereals$age, cereals$shelf))

# Initialize
N <- 10^4 - 1
result <- numeric(N)

# Permute
for (i in 1:N) {
   age.permuted <- sample(cereals$age)
   cereal.table <- table(age.permuted, cereals$shelf)
   result[i] <- chisq(cereal.table)
}

# Calculate the p-value
(sum(result > observed) + 1) / (N + 1)

## ----cache=TRUE----------------------------------------------------------
chisq.test(cereals$Age, cereals$Shelf, simulate.p.value = TRUE, B = 10^4 - 1)

## ----cache=TRUE----------------------------------------------------------
gss2002 <- read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/GSS2002.csv")

table(gss2002$Gender, gss2002$Pres00)

## ------------------------------------------------------------------------
result_16b <- chisq.test(gss2002$Gender, gss2002$Pres00)
result_16b 

## ------------------------------------------------------------------------
result_16b$expected

## ----cache=TRUE----------------------------------------------------------
# The observed test statistic
observed <- chisq(table(gss2002$Gender, gss2002$Pres00))

# Initializing
N <- 10^4 - 1
result <- numeric(N)

# Permuting
for (i in 1:N) {
  Pres00.permuted <- sample(gss2002$Pres00)
  pres.table <- table(gss2002$Gender, Pres00.permuted)
  result[i] <- chisq(pres.table)
}

# Calulcating a p-value
(sum(result > observed) + 1)/(N + 1)

## ----include=FALSE-------------------------------------------------------
integrand <- function(y) (1/9)*y^2
c1 <- integrate(integrand, lower = 0, upper = 1.25)
c2 <- integrate(integrand, lower = 1.25, upper = 1.75)
c3 <- integrate(integrand, lower = 1.75, upper = 2.25)
c4 <- integrate(integrand, lower = 2.25, upper = 2.75)
c5 <- integrate(integrand, lower = 5.75, upper = 3)

## ------------------------------------------------------------------------
qnorm(p = c(0.2, 0.4, 0.6, 0.8), mean = 22, sd = 7)

## ------------------------------------------------------------------------
# Enter in the data
values <- c(1.28, 4.53, 5.50, 7.91, 8.23, 9.67, 9.82, 10.28, 10.45, 11.91,
            12.57, 13.75, 13.80, 14.00, 14.05, 16.02, 16.18, 16.25, 16.58, 16.68,
            16.87, 17.61, 17.63, 17.71, 18.13, 18.42, 18.43, 18.44, 19.62, 20.401,
            20.73, 20.74, 21.29, 21.51, 21.66, 21.87, 22.67, 23.11, 24.40, 24.55,
            24.66, 25.30, 25.46, 25.91, 26.12, 26.61, 26.72, 29.28, 31.93, 36.94)

# Create a categorical variable for the intervals
bins <- cut(values, breaks = c(-Inf, qnorm(p = c(0.2, 0.4, 0.6, 0.8), mean = 22, sd = 7), Inf))

# Tabulate 
table(bins)

## ------------------------------------------------------------------------
chisq.test(c(16, 13, 9, 9, 3), p = c(.2, .2, .2, .2, .2))

