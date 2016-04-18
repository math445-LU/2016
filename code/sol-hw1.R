## ----include=FALSE-------------------------------------------------------
library(ggplot2)
library(dplyr)
library(knitr)
opts_chunk$set(
  tidy=FALSE,     # display code as typed
  size="small",   # slightly smaller font for code
  fig.width=5,
  fig.height=3
)

## ------------------------------------------------------------------------
gss <- read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/GSS2002.csv")

## ----eval=FALSE, fig.height=2, fig.width = 3.5---------------------------
## library(dplyr)
## gss %>%
##   group_by(DeathPenalty) %>%
##   summarise(count = n())
## 
## library(ggplot2)
## ggplot(data = gss) +
##   geom_bar(mapping = aes(x = DeathPenalty))

## ----echo=FALSE----------------------------------------------------------
gss %>%
  group_by(DeathPenalty) %>%
  summarise(count = n()) %>%
  kable

## ----echo=FALSE,fig.height=2, fig.width=2--------------------------------
ggplot(data = gss) + 
  geom_bar(mapping = aes(x = DeathPenalty))

## ----eval=FALSE----------------------------------------------------------
## library(dplyr)
## gss %>%
##   group_by(OwnGun) %>%
##   summarise(count = n())
## 
## library(ggplot2)
## ggplot(data = gss) +
##   geom_bar(mapping = aes(x = OwnGun))

## ----echo=FALSE----------------------------------------------------------
gss %>%
  group_by(OwnGun) %>%
  summarise(count = n()) %>%
  kable

## ----echo=FALSE,fig.height=2, fig.width=2--------------------------------
ggplot(data = gss) + 
  geom_bar(mapping = aes(x = OwnGun))

## ------------------------------------------------------------------------
views_tbl <- 
  gss %>%
  group_by(DeathPenalty, OwnGun) %>%
  summarise(count = n())
views_tbl

## ----message=FALSE-------------------------------------------------------
library(tidyr)
views_tbl %>%
  na.omit %>%
  spread(DeathPenalty, count)

## ------------------------------------------------------------------------
spruce <- read.csv("https://raw.githubusercontent.com/math445-LU/2016/master/data/Spruce.csv")

## ------------------------------------------------------------------------
summary(spruce$Ht.change)
sd(spruce$Ht.change)
length(spruce$Ht.change)

## ------------------------------------------------------------------------
spruce %>%
  summarise(min = min(Ht.change),
            Q1 = quantile(Ht.change, probs = .25),
            median = median(Ht.change),
            Q3 = quantile(Ht.change, probs = .75),
            mean = mean(Ht.change), 
            sd = sd(Ht.change),
            n = n())

## ------------------------------------------------------------------------
ggplot(data = spruce) +
  geom_histogram(mapping = aes(x = Ht.change), binwidth = 5, colour = "gray")

ggplot(data = spruce) +
  stat_qq(mapping = aes(sample = Ht.change))

## ------------------------------------------------------------------------
spruce %>%
  group_by(Fertilizer) %>%
  summarise(min = min(Ht.change),
            Q1 = quantile(Ht.change, probs = .25),
            median = median(Ht.change),
            Q3 = quantile(Ht.change, probs = .75),
            mean = mean(Ht.change), 
            sd = sd(Ht.change),
            n = n())

## ------------------------------------------------------------------------
ggplot(data = spruce) +
  geom_density(mapping = aes(x = Di.change, fill = Fertilizer), alpha = 0.4)

ggplot(data = spruce) +
  geom_boxplot(mapping = aes(x = Fertilizer, y = Di.change))

ggplot(data = spruce) +
  geom_histogram(mapping = aes(x = Di.change), colour = "gray", binwidth = .5) + 
  facet_wrap(~ Fertilizer, ncol = 1)

## ------------------------------------------------------------------------
spruce %>%
  group_by(Fertilizer, Competition) %>%
  summarise(min = min(Ht.change),
            Q1 = quantile(Ht.change, probs = .25),
            median = median(Ht.change),
            Q3 = quantile(Ht.change, probs = .75),
            mean = mean(Ht.change), 
            sd = sd(Ht.change),
            n = n())

## ------------------------------------------------------------------------
ggplot(data = spruce) +
  geom_density(mapping = aes(x = Di.change, fill = Fertilizer:Competition), alpha = 0.4)

ggplot(data = spruce) +
  geom_boxplot(mapping = aes(x = Fertilizer:Competition, y = Di.change))

ggplot(data = spruce) +
  geom_histogram(mapping = aes(x = Di.change), colour = "gray", binwidth = .5) + 
  facet_grid(Fertilizer ~ Competition)

## ------------------------------------------------------------------------
ggplot(data = spruce) + 
  geom_point(mapping = aes(x = Di.change, y = Ht.change)) +
  geom_smooth(mapping = aes(x = Di.change, y = Ht.change), method = "lm")

