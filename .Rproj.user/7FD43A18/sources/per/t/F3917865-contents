library(tidyverse)
library(janitor)

# Importing fake_energy_data from data folder
# assigning it to the name raw_energy_data 

raw_energy_data <- readr::read_csv("./Data/fake_energy_data.csv")

# We can view the data 
raw_energy_data

# Look at the first 10 observations 

head(raw_energy_data, n = 10)

dplyr::glimpse(raw_energy_data)

# First assign the airquality dataset stored in R 
# to the name airquality_data

airquality_data <- airquality |> 
  clean_names()


# To display the first 5 observation

head(airquality_data,n=5)

# Look at last 10 observations 

tail(airquality_data, n = 10)

glimpse(airquality)


# Random Sampling ---------------------------------------------------------

# We start the list of random numbers at the position 42. 
# The position of the seed is defined by you so it can be anything.

set.seed(42)

# Pick 10 numbers from the sequence 1 to 100. 
# The sample argument 'replace' is default false so we change it to TRUE.

sample(x=seq(1,100), size=10, replace = TRUE)

# exercise

set.seed(653)

sample(x=seq(1,500), size=5, replace = FALSE)

obs <- sample(x = raw_energy_data$rowid, size =10, replace = TRUE)
obs


# Probability Distributions -----------------------------------------------

# d for “density”, the density function (p.d.f)
# p for “probability”, the cumulative distribution function
# q for “quantile”, the inverse c.d.f
# r for “random”, a random variable having the specified distribution

# Picking 8 random values from a sample which follows a binomial distribution, 
# where the total number of observations in the sample is 150 
# and the probability of success is 0.4.

rbinom(n=8,size=150,prob=0.4)

# Probability of getting 26 heads from 51 tosses of a coin
# where the probability of success (probability of getting a head) is 0.5
dbinom(x=26,size=51,prob=0.5)

# Probability of getting up to 26 heads from 51 tosses of a coin.
# where the probability of success is 0.5
# Mathematically it is P(X⩽26) where X~Bin(51,0.5)

pbinom(q=26,size=51,prob=0.5)

# qbinom() function takes the probability value and gives a number whose cumulative value matches the probability value.
# What is the maximum number of heads 
# in the bottom 25% of coin flip experiments when the coin is tossed 51 times?

qbinom(p=0.25,size=51,prob=0.5)

# Binomial exercise
dbinom(x=4,size=12,prob=0.2)

pbinom(q=4,size=12,prob=0.2)


# Normal distribution

# Create a sample of 50 random numbers which are normally distributed.
# using rnorm() function which generates random numbers from 
# normal distributions 
# n=50 because we want 50 random numbers 
# standard normal distribution has mean=0 and standard deviation 1

normal_sample <- rnorm(n=50, mean = 0, sd = 1)

# Plot the histogram for this sample.
# x is the vector of values 
# main gives the title, xlab and ylab gives the title for x-axis and y-axis

hist(x=normal_sample, main = "Normal Distribution",
     xlab = "Sample from normal distribution")

# The default argument is to have lower.tail = TRUE,
# which return the probabilities less than the height specified
# that is P(X⩽157.5)

pnorm(q=157.5, mean = 170, sd = 10, lower.tail = TRUE)

# Normal distribution exercise
pnorm(q=84, mean = 72, sd = 15.2, lower.tail = FALSE)

# Poisson distribution
# Select 15 observations from a poisson distribution
# where the average rate within our window is 10
# n is the size of random numbers we want
# lambda is the mean which is 10 in our case

rpois(n = 15, lambda = 10)

# Let X~Poisson(3) (average of 3 events per unit time: lambda = 3)
# Q. what is probability of 2-4 events happening in that time?
# Since Poisson distribution is discrete distribution 
# the probability at each point does not have to be 0
# So P(2≤X≤4) = P(X≤4)-P(X<2) = P(X≤4)-P(X≤1)
# because P(X<2) = P(X=1)+P(X=0) = P(X≤1)
# Since we are interested in cumulative probability we will use ppois()


ppois(q = 4, lambda = 3, lower.tail = TRUE) - 
  ppois(q = 1, lambda = 3, lower.tail = TRUE)

# Alternative way
# dpois() gives the probability density distribution at each point


dpois(x = 2, lambda = 3) +
  dpois(x = 3, lambda = 3) +
  dpois(x = 4, lambda = 3)

# poisson exercise

#lambda = 10, P(X greq 12)?
ppois(q = 12, lambda = 10, lower.tail = FALSE)
