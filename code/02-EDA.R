### Load the data set (you'll need to change the file path)
flights <- read.table(file = "../data/hesterberg-chihara-data/FlightDelays.csv", sep = ",", header = TRUE)
## flights <- read.table(file = file.choose(), sep = ",", header = TRUE)

# Take a quick look to see if everything is OK
dim(flights)
nrow(flights)
ncol(flights)
str(flights)
head(flights)

### Load ggplot2 library
# install.packages("ggplot2") # uncomment to install
library(ggplot2)

### Plotting code from the slides
ggplot(data = flights) + 
  geom_bar(mapping = aes(x = Carrier))

ggplot(data = flights) + 
  geom_bar(mapping = aes(x = Carrier, fill = Carrier)) + 
  ggtitle("Bar chart of flights by carrier")

ggplot(data = flights) + 
  geom_histogram(mapping = aes(x = FlightLength)) + 
  xlab("Flight length (min)")

ggplot(data = flights) + 
  geom_histogram(mapping = aes(x = FlightLength), binwidth = 30) + 
  xlab("Flight length (min)")

ggplot(data = flights) + 
  geom_density(mapping = aes(x = FlightLength)) + 
  xlab("Flight length (min)")

ggplot(data = flights) + 
  geom_histogram(mapping = aes(x = FlightLength, y = ..density..), binwidth = 15) +
  geom_density(mapping = aes(x = FlightLength), colour = "orange") + 
  xlab("Flight length (min)")

ggplot(data = flights) +
  geom_boxplot(mapping = aes("var", FlightLength)) +
  xlab("") +
  ylab("Flight time (min)") +
  scale_x_discrete(breaks = NULL) +
  coord_flip()

ggplot(data = flights) +
  geom_point(mapping = aes(sample = Delay), stat = "qq")

ggplot(data = flights) + 
  stat_ecdf(mapping = aes(x = Delay), geom = "step") + 
  xlab("Delay (min)") +
  ylab("F(x)")
