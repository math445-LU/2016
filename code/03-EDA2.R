### Loading ggplot2
library(ggplot2)

### Loading the data set directly from the web
oly12 <- read.table("https://raw.githubusercontent.com/math445-LU/2016/master/data/oly12.csv", sep = ",", header = TRUE)
oly12$Sport <- abbreviate(oly12$Sport, 12)
str(oly12)

### Plotting Code from the slides
ggplot(data = oly12) +
 geom_point(mapping = aes(x = Height, y = Weight))

ggplot(data = oly12) +
 geom_point(mapping = aes(x = Height, y = Weight)) +
 geom_smooth(mapping = aes(x = Height, y = Weight))

ggplot(data = oly12) +
 geom_point(mapping = aes(x = Height, y = Weight, colour = Sex),  alpha = 0.7)

ggplot(data = oly12) +
  geom_boxplot(mapping = aes(x = Sport, y = Age)) +
  coord_flip()

ggplot(data = oly12) +
  geom_boxplot(mapping = aes(x = reorder(Sport, Age, median), y = Age)) +
  coord_flip() +
  xlab("Sport")

ggplot(data = oly12) +
  geom_histogram(mapping = aes(x = Height)) +
  facet_wrap(~ Sport)

ggplot(data = oly12) +
  geom_point(mapping = aes(x = Height, y = Weight), size = 1) +
  facet_wrap(~ Sport)

### Simple summary statistics
mean(oly12$Age)
median(oly12$Age)
sd(oly12$Age)
var(oly12$Age)
quantile(oly12$Age, probs = c(.2, .4, .6, .8))

### Loading dplyr
# install.packages("dplyr") # uncomment if not installed
library(dplyr)

### Obtaining summaries by group
age_sport <- 
  oly12 %>%
  group_by(Sport) %>%
  summarise(avgAge = mean(Age))
head(age_sport)  

medal_count <- 
  oly12 %>%
  group_by(Country) %>%
  summarise(Gold = sum(Gold), Silver = sum(Silver), Bronze = sum(Bronze)) %>%
  arrange(desc(Gold), desc(Silver), desc(Bronze))
head(medal_count)  


### Using filter() to subset the data before summarizing
oly12 %>%
  filter(Country == "United States of America") %>%
  group_by(Sex) %>%
  summarise(avgAge = mean(Age))

oly12 %>%
  filter(Gold > 0 | Silver > 0 | Bronze > 0) %>%
  ggplot() +
  geom_density(mapping = aes(x = Age, group = Sex, fill = Sex), alpha = 0.7)
