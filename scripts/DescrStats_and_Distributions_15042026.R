# Lecture script 15.04.2026

# HI

# packages ----
# install.packages(c("DescTools", "e1071"))

library(tidyverse)

# data ----
# Use the penguins data set as example
penguins <- read_csv("data/penguins.csv")

# Part 1 - Descriptive statistics ----

# examine the distribution of penguin body masses
hist(penguins$body_mass, breaks = 20)

## measures of centrality ----
# mean
mean(penguins$body_mass, na.rm = TRUE) 
# remember to exclude NAs otherwise the result will be NA too
# the mean weight is 4201.754

# median
median(penguins$body_mass, na.rm = TRUE)
# the median weight is 4050

# mode
# there is no function in base R for the mode, but in a package called DescTools
DescTools::Mode(penguins$body_mass, na.rm = TRUE)
# the mode is 3800, with 12 entries in the data set
# we can easily verify this or check which is the second most frequent value: 
penguins %>% 
  select(body_mass) %>% 
  group_by(body_mass) %>% 
  count() %>% 
  arrange(desc(n))


## measures of scale ----
# standard deviation
sd(penguins$body_mass, na.rm = TRUE)
# the standard deviation of weight is 801.9545

# range
min(penguins$body_mass, na.rm = TRUE)
max(penguins$body_mass, na.rm = TRUE)

# the weight range is from 2700 to 6300.

# quartiles
quantile(penguins$body_mass, na.rm = TRUE)
# the quantile function automatically calculates Q0 - to Q4

# interquartile range
4750 - 3550
# the interquartile range (Q3 - Q1) is 1200

# percentiles
# we can use the quantiles() function again and specify which percentile we want to calculate
quantile(penguins$body_mass, na.rm = TRUE, probs = 0.4)
# this example calculates the 40% percentile, this means that 40% of the data are
# smaller or equal to a weight of 3800

# boxplots graphically represent the distribution of your data: 
boxplot(penguins$body_mass)

# also the summary function is very helpful, 
# giving you the range, 1st and 3rd quartile and the median all at once:
summary(penguins$body_mass)

## measures of shape ----
# skewness from the package e1071
e1071::skewness(penguins$body_mass, na.rm = TRUE)
# slightly right skewed, see also the histogram

# and kurtosis
e1071::kurtosis(penguins$body_mass, na.rm = TRUE)

# values close to 0 indicate a normal distribution
# but what does close mean? 
# compare to prizes of diamonds, which has a very different distribution
hist(diamonds$price)
e1071::skewness(diamonds$price)
e1071::kurtosis(diamonds$price)

# with this comparison we can now see that the body masses resemble a normal distribution closer than
# the diamond prices.


# Part 2 - Distributions ----
# gaussian / normal
curve(dnorm(x, mean = 165, sd = 15), from = 120, to = 210, ylab = "probability density", xlab = "Body height")

# poisson
values <- 0:11
plot(values, dpois(values, lambda = 3), type = "h", ylab = "probability", xlab = "E-mails per day")

# binomial
values <- 1:20
plot(values, dbinom(values, size = 20, prob = 0.9), type = "h", ylab = "probability", col = "firebrick")

# negative binomial
par(new = TRUE)
plot(values, dnbinom(values, size = 20, prob = 0.9), type = "h", ylab = "probability", col = "dodgerblue", axes = FALSE)

# uniform
curve(dunif(x, min = 10, max = 15), from = 0, to = 20, ylab = "probability density")

# Part 3 - Normality test ----
# use the trees dataset and the variable height
hist(trees$Height)
# what do you think?
shapiro.test(trees$Height)

hist(trees$Height, freq = FALSE) # show the density histogram instead of a frequency histogram
curve(dnorm(x, mean = 76, sd = 6.371813), from = 60, to = 90, add = TRUE, col = "firebrick", lwd = 2)


# use the penguins body mass
hist(penguins$body_mass)
# again what do you think? Is this normally distributed?

shapiro.test(penguins$body_mass)

# Part 4 - exercises ----

## 1. Load the ices_data_new.csv dataset

## 2. Decide for one of the stocks and one variable and make a description
# of the distribution of this variable

## 3. What kind of distribution do you think would fit best? 
# Try to evaluate / compare using maximum log likelihood. 
# See chapter 4 in: https://link.springer.com/book/10.1007/978-3-030-55020-2

