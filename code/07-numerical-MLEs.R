## -----------------------------------------------------------
## Example of using R to find ML estimates using optim
## Math 445, Spring 2016
## Prof. Adam Loy
## -----------------------------------------------------------

## Preliminaries
library(ggplot2)
storm <- read.csv("../data/BostonStorms2013.csv")

## Plot the data
ggplot(data = storm) + 
  geom_histogram(mapping = aes(x = precip), colour = "gray20")

## Calculating the MoM estimates
n <- length(storm$precip) 
xbar <- mean(storm$precip) 
s2 <- var(storm$precip) 
sigma2_hat <- s2 * (n - 1) / n 

lambda_mom <- xbar / sigma2_hat; lambda_mom
alpha_mom <- xbar^2 / sigma2_hat; alpha_mom


## Writing the log-likelihood function
gamma_loglik <- function(theta, x) {
  n <- length(x)
  alpha <- theta[1]
  lambda <- theta[2]
  RES <- n * (alpha * log(lambda) - log(gamma(alpha)) + 
                (alpha - 1) * mean(log(x)) - lambda * mean(x)) 
  return(RES)
}

## Using optim to maximize the log-likelihood
theta <- optim(par = c(alpha_mom, lambda_mom), fn = gamma_loglik, 
               control = list(fnscale = -1), x = storm$precip)
theta

## Alternative way to write the log-likelihood (RECOMMENDED)
gamma_loglik_alt <- function(theta, x) {
  sum(dgamma(x, shape = theta[1], rate = theta[2], log = TRUE))
}

## Using optim to maximize the log-likelihood
theta2 <- optim(par = c(alpha_mom, lambda_mom), fn = gamma_loglik_alt, 
                control = list(fnscale = -1), x = storm$precip)
theta2