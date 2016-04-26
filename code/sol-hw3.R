## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
  tidy=FALSE,     # display code as typed
  size="footnotesize",   # slightly smaller font for code
  fig.width=5,
  fig.height=3
)

library(ggplot2)

## ----5.8A, cache=TRUE, message=FALSE-------------------------------------
# Construct the sampling distribution
N <- 1000
sampling_dsn <- numeric(N)
for(i in 1:N) {
 samp <- rgamma(200, 5, 1/4) 
 sampling_dsn[i] <- mean(samp)
}

# Plot it
ggplot(data = data.frame(mean = sampling_dsn)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20") + 
  ggtitle("Sampling distribution")

# Summarize it
mean_samp_dsn <- mean(sampling_dsn); mean_samp_dsn
sd_samp_dsn <- sd(sampling_dsn); sd_samp_dsn

## ----5.8B, cache=TRUE, message=FALSE-------------------------------------
# Draw one sample of size 200
samp5.8b <- rgamma(200, 5, 1/4) 

# Plot it
ggplot(data = data.frame(obs = samp5.8b)) + 
  geom_histogram(mapping = aes(x = obs), colour = "gray20") + 
  xlab("Observations") + 
  ggtitle("Distribution of one sample of size 200 from Gamma(5,1/4)")


# Summarize it
mean_sample <- mean(samp5.8b); mean_sample
sd_sample <- sd(samp5.8b); sd_sample

## ----5.8C, cache=TRUE, message=FALSE-------------------------------------
# Resample with replacement from the sample
N <- 10000
boot_means <- numeric(N)
for(i in 1:N) {
  boot_means[i] <- mean(sample(samp5.8b, replace = TRUE))
}

# Plot it
ggplot(data = data.frame(mean = boot_means)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20") + 
  ggtitle("Bootstrap distribution")

# Summarize it
mean_boot <- mean(boot_means); mean_boot
sd_boot <- sd(boot_means); sd_boot

## ----echo=FALSE----------------------------------------------------------
library(knitr)
RES <- data.frame(Distribution = c("Popualation", "Sampling distribution", 
                                   "Sample", "Bootstrap distribution"), 
                  Mean = c(20, mean_samp_dsn, mean_sample, mean_boot),
                  SD = c(8.94, sd_samp_dsn, sd_sample, sd_boot)
)
kable(RES, digits = 2)

## ----5.9, cache=TRUE, fig.width=5, fig.height=5, message=FALSE-----------
even_samp_sizes <- c(14, 36, 200, 10^4)
res5.9 <- data.frame(n = NULL, median = NULL) # inefficient, but quick
for(so in even_samp_sizes)
{
  ne <- so     # n even
  no <- so + 1 # n odd
  
  wwe <- rnorm(ne)
  wwo <- rnorm(no)
  
  N <- 10^4
  even.boot <- numeric(N)
  odd.boot <- numeric(N)
  for(i in 1:N) 
  {
    x.even <- sample(wwe, ne, replace = TRUE)
    x.odd <- sample(wwo, no, replace = TRUE)
    even.boot[i] <- median(x.even)
    odd.boot[i] <- median(x.odd)
  }
  res5.9 <- rbind(res5.9, cbind(n = ne, median = even.boot), cbind(n = no, median = odd.boot))
}

# Make the plots
library(dplyr)
ggplot(data = filter(res5.9, n < 200)) + 
  geom_histogram(mapping = aes(x = median), colour = "gray20", binwidth = .1) + 
  facet_wrap(~n, ncol = 2)

ggplot(data = filter(res5.9, n >= 200)) + 
  geom_histogram(mapping = aes(x = median), colour = "gray20", binwidth = .01) + 
  facet_wrap(~n, ncol = 2)

## ----5.10A, message=FALSE, warning=FALSE---------------------------------
bangladesh <-  read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/Bangladesh.csv")

summary(bangladesh$Chlorine)

ggplot(data = bangladesh) + 
  geom_histogram(mapping = aes(x = Chlorine), colour = "gray20")

## ----5.10B, cache=TRUE, message=FALSE------------------------------------
chlorine <- subset(bangladesh, select = Chlorine, subset = !is.na(Chlorine), drop = TRUE)

N <- 10^5
chlorine_boot <- numeric(N)
for(i in 1:N)
{
  x <- sample(chlorine, replace = TRUE)
  chlorine_boot[i] <- mean(x)
}

ggplot(data = data.frame(mean = chlorine_boot)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20")

# boot mean
mean(chlorine_boot)

# boot SE
sd(chlorine_boot)

## ----5.10C---------------------------------------------------------------
ci5.10 <- quantile(chlorine_boot, probs = c(0.025, 0.975))
ci5.10

## ----5.10D---------------------------------------------------------------
# bias
bias5.10 <- mean(chlorine_boot) - mean(chlorine)
bias5.10

# SE
se5.10 <- sd(chlorine_boot)
se5.10

## ----5.11, cache=TRUE, message=FALSE-------------------------------------
N <- 10^5
trim_boot <- numeric(N)
for(i in 1:N)
{
  x <- sample(chlorine, replace = TRUE)
  trim_boot[i] <- mean(x, trim = 0.25)
}

ggplot(data = data.frame(mean = trim_boot)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20")

# bias
bias5.11 <- mean(trim_boot) - mean(chlorine, trim = 0.25)
bias5.11

# SE
se5.11 <- sd(trim_boot)
se5.11

# bias/SE
bias5.11/se5.11

# CI
ci5.11 <- quantile(trim_boot, probs = c(0.025, 0.975))
ci5.11

## ----5.12A, message=FALSE------------------------------------------------
fishmercury <- read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/FishMercury.csv")

ggplot(data = fishmercury) + 
  geom_histogram(mapping = aes(x = Mercury), colour = "gray20", binwidth = 0.05)

## ----5.12B, cache=TRUE, message=FALSE------------------------------------
N <- 10^5
boot5.12b <- numeric(N)
for(i in 1:N)
{
  x <- sample(fishmercury$Mercury, replace = TRUE)
  boot5.12b[i] <- mean(x)
}

ggplot(data = data.frame(mean = boot5.12b)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20")

# bias
bias5.12b <- mean(boot5.12b) - mean(boot5.12b)
bias5.12b

# SE
se5.12b <- sd(boot5.12b)
se5.12b

# bias/SE
bias5.12b/se5.12b

# CI
ci5.12b <- quantile(boot5.12b, probs = c(0.025, 0.975))
ci5.12b

## ----5.12C, cache=TRUE, message=FALSE------------------------------------
N <- 10^5
boot5.12c <- numeric(N)
mercury_no_outlier <- subset(fishmercury, select = Mercury, subset = Mercury < 1.87, drop = TRUE)
for(i in 1:N)
{
  x <- sample(mercury_no_outlier, replace = TRUE)
  boot5.12c[i] <- mean(x)
}

ggplot(data = data.frame(mean = boot5.12c)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20")

# bias
bias5.12c <- mean(boot5.12c) - mean(boot5.12c)
bias5.12c

# SE
se5.12c <- sd(boot5.12c)
se5.12c

# bias/SE
bias5.12c/se5.12c

# CI
ci5.12c <- quantile(boot5.12c, probs = c(0.025, 0.975))
ci5.12c

## ----5.17A, message=FALSE------------------------------------------------
bookprices <- read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/BookPrices.csv")

ggplot(data = bookprices) + 
  geom_boxplot(mapping = aes(x = Area, y = Price)) + 
  coord_flip()

library(dplyr)
bookprices %>%
  group_by(Area) %>%
  summarise(mean = mean(Price), sd = sd(Price))

## ----5.17B, cache=TRUE, message=FALSE------------------------------------
N <- 10^5
math <- subset(bookprices, select = Price, subset = Area == "Math & Science", drop = TRUE)
socsci <- subset(bookprices, select = Price, subset = Area == "Social Sciences", drop = TRUE)
boot_math   <- numeric(N)
boot_socsci <- numeric(N)
for(i in 1:N)
{
  boot_math[i] <- mean(sample(math, replace = TRUE))
  boot_socsci[i] <- mean(sample(socsci, replace = TRUE))
}

ggplot(data = data.frame(mean = boot_math)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20") + 
  ggtitle("Boostrap dsn of avg. price for math & sci.")

ggplot(data = data.frame(mean = boot_socsci)) + 
  geom_histogram(mapping = aes(x = mean), colour = "gray20") + 
  ggtitle("Boostrap dsn of avg. price for social sci.")

# bias
bias_math <- mean(boot_math) - mean(math); bias_math
bias_socsci <- mean(boot_socsci) - mean(socsci); bias_socsci

# SE
se_math <- sd(boot_math); se_math
se_socsci <- sd(boot_socsci); se_socsci

# bias/SE
bias_math / se_math
bias_socsci / se_socsci

## ----5.17C, cache=TRUE, message=FALSE------------------------------------
N <- 10^5
boot_ratio   <- numeric(N)
for(i in 1:N)
{
  boot_ratio[i] <- mean(sample(math, replace = TRUE)) / mean(sample(socsci, replace = TRUE))
}

ggplot(data = data.frame(ratio = boot_ratio)) + 
  geom_histogram(mapping = aes(x = ratio), colour = "gray20") + 
  ggtitle("Boostrap ratio of avg. books prices: math / socsci")

# bias
bias_ratio <- mean(boot_ratio) - mean(math) / mean(socsci); bias_ratio

# SE
se_ratio <- sd(boot_ratio); se_ratio

# bias/SE
bias_ratio / se_ratio

## ----5.17D---------------------------------------------------------------
ci5.17d <- quantile(boot_ratio, probs = c(0.025, 0.975)); ci5.17d

