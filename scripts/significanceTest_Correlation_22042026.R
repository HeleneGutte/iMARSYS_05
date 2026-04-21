# Lecture script 22.04.2026


# one sample t-Test ----

# Measured 15 fish length of plaice: 
# data produced using plaice_length <- rnorm(15, 35, sd = 5)

plaice_length <- c(36.67, 28.37, 42.67, 38.63, 33.7, 29.39, 38.32, 38.71, 40.59,
                   38.41, 36.82, 40.52, 43.31, 43.64, 37.48)

# calculate mean and stadard deviation of your data
mean(plaice_length) # 37.8215
sd(plaice_length) # 4.50634

# common length according to fish base are 40cm, so the A in the formula becomes 40
# calculate the t-value
t_plaice <- (37.8215 - 40) / (4.50634 / sqrt(15))
t_plaice # the negative value tells us, that our measured mean is below the expected value.

# is this critical? 

# get the critical levels from the t-distribution
# the only parameter of the t- distribution are the degrees of freedom, which
# are the sample size - 1, so in this case 14.
# we only want to know if it our mean value different from the actual mean and don´t care 
# whether the mean of our sample is lower or higher, therefore we are looking at
# both sides of the distribution and distribute the 5% significance level: 
qt(p = c(0.025, 0.975), df = 14) # -2.144787  2.144787
# these are the "cut-off" values: If our value is lower or higher than those, 
# it is a critical t-value

# additionally we can calculate the p-value for our t-value
# multiply by 2, since we compare to both tails. 
# Therefore we call this type of t-Test two-sided
pt(q = t_plaice, df = 14) * 2 # 0.08219942
# It is not lower than 0.05 thus again not signifcant

# We can also use the build in t-test function to validate this:
# First have a look at the help pages to see how it works: 
?t.test

t.test(x = plaice_length, mu = 40)

#Lets visualize this: 
# The t-Distribution for df=14
# as density distribution
plot(function(x) dt(x, df = 14), -6, 6, ylim = c(0, 0.4),
       main = "Density", ylab = "density f(x)")
abline(v = c(t_plaice, -t_plaice), col = "blue", lty = 2)
abline(v = qt(p = c(0.025, 0.975), df = 14), col = "red", lty = 2)

# and cumulative probability: 
plot(function(x) pt(x, df = 14), -6, 6, ylim = c(0, 1), 
     main = "Distribution", ylab = "Probability F(x)")
abline(v = c(t_plaice, -t_plaice), col = "blue", lty = 2)
abline(v = qt(p = c(0.025, 0.975), df = 14), col = "red", lty = 2)


# Two - sample t-Test ----
# Comparison of polar bear heights: 
polar_bears <- data.frame("male" = c(281, 279, 271, 290, 292, 285, 277, 278, 290, 277, 281, 283,
                                     285, 287, 279, 275, 280, 287, 285, 282),
                          "female" = c(266, 280, 275, 272, 270, 269, 272, 278, 267, 269, 282,
                                       279, 276, 272, 274, 270, 269, 279, 282, 268))
boxplot(polar_bears)

## two-sided ----
# Do female polar bears have a different size than male polar bears? 
# This includes not if they are bigger or smaller, just different, therefor
# this is a two-sided test, where we are looking at both tails of the t-distribution.

# calculate means
mean_males <- mean(polar_bears$male)
mean_males
mean_females <- mean(polar_bears$female)
mean_females

# calculate variances
var_males <- var(polar_bears$male)
var_males
var_females <- var(polar_bears$female)
var_females

nrow(polar_bears)

qt(p = c(0.025, 0.975), df = 38) # 38 because each has 20 rows and for each sample we subtract by 1

# we use here the negative absolute difference, to avoid a miscalculation simply because the t value is very large
pt(q = -abs(t_polars), df = 37.844) * 2 

t_polars <- (mean_males - mean_females) / sqrt( (var_males/20) + (var_females/20) )
t_polars

# for equal variances we can also use pooled variances, 
# this gives us a bit more statistical power: 
t.test(polar_bears$male, polar_bears$female, var.equal = TRUE)


## one-sided ----
# Male polar bears are bigger than female polar bears.
t.test(polar_bears$male, polar_bears$female, alternative = "greater")

t.test(polar_bears$male, polar_bears$female, alternative = "less")

# Paired two-sample t-Test ----
# Compare weights of penguins before and after the breeding season 
penguins <- data.frame("Penguin" = c(1:16),
                       "Lipid_start" = sample(seq(8, 11, by = 0.2), size = 16),
                       "Lipid_end" = sample(seq(1, 4, by = 0.2), size = 16),
                       "Weight_start" = sample(seq(35, 45, by = 0.2), size = 16),
                       "Weight_end" = sample(seq(18, 25, by = 0.2), size = 16))
View(penguins)

boxplot(penguins[ ,4:5])

# Calculating the differences from start to end
difference_weight <- penguins[ ,4] - penguins[ ,5] # this is a vectorized operation, this means
# R automatically calculates the difference for each row individually
difference_weight

# Calculating the variances of the differences
var_diff <- var(difference_weight)


# Calculating t
t_weights <- mean(difference_weight)/(sqrt(var_diff)/sqrt(16))
t_weights 
qt(p = c(0.025, 0.975), df = 15)
# we use here the negative absolute difference, to avoid a miscalculation simply because the t value is very large
pt(q = -abs(t_weights), df = 15) * 2 

#Paired two-sided two sample t-Tests
t.test(x=penguins[,4], y=penguins[,5], paired=T)

# Mann - Whitney - U test ----
octopus <- read_csv("data/kraken.csv")
# data set with intelligence test results of trained and untrained octopus individuals
View(octopus)

ggplot(octopus, aes(y = result, x = group_txt))+
  geom_boxplot()

Ranks <- cbind(octopus, rank(x=octopus$result, na.last=NA,
                                ties.method=c("average")))
View(Ranks)

wilcox.test(result ~ group, data = octopus, alternative = "two.sided")

# Wilcoxon signed rank test ----
# Number of herring eggs from subsampling
herring_eggs <- data.frame("Sample" = c(1:10),
                           "subsample_1" = as.integer(jitter(c(130, 90, 50, 323, 210, 35, 173, 44, 241, 112), amount = 10))
                           )
difference <- sample(-20:20, 10)
herring_eggs$subsample_2 <- herring_eggs$subsample_1 + difference
boxplot(herring_eggs[, c(2:3)])

wilcox.test(x = herring_eggs[,2], y = herring_eggs[,3], alternative = c("two.sided"), paired = T)


# F-Test ----
salmon <- data.frame("Net_1" = rnorm(n = 24, mean = 2898.667, sd = 195.0281),
                     "Net_2" = rnorm(n = 24, mean = 2825.417, sd = 153.6458))
boxplot(salmon)

var.test(x = salmon$Net_1, y = salmon$Net_2, ratio = 1, alternative="two.sided")
var.test(x = salmon$Net_1, y = salmon$Net_2, ratio = 1, alternative = "greater")

# Correlation ----
# Or create a data set with depth and oxygen levels
ctd_data <- data.frame("depth" = round(runif(100, min = 200, max = 500), digits = 0) )
ctd_data$oxygen <- round(8 - 0.01 * ctd_data$depth + rnorm(100, mean = 0, sd = 0.3), 2)
ctd_data$oxygen[ctd_data$oxygen < 0] <- runif(sum(ctd_data$oxygen < 0), 0.1, 0.5)

ggplot(ctd_data, aes(x = depth, y = oxygen))+
  #geom_smooth(method = lm, se = FALSE, colour = "black")+
  geom_point()+
    labs(x = "depth", y = "oxygen")

# covariance
cov(x = ctd_data$depth, y = ctd_data$oxygen)
# correlation  
cor(x = ctd_data$depth, y = ctd_data$oxygen, method = "pearson")

# correlation with significance test
cor.test(x = ctd_data$depth, y = ctd_data$oxygen, method = "pearson")

# Spearman
cor.test(x = ctd_data$depth, y = ctd_data$oxygen, method = "spearman")

# Kendall
cor.test(x = ctd_data$depth, y = ctd_data$oxygen, method = "kendall")


# Association ----
benthos <- data.frame(sediment = c("Sand","Sand","Sand","Mud","Mud","Mud","Gravel","Gravel","Gravel",
               "Sand","Mud","Gravel","Sand","Mud","Gravel"),
               worms_abundance = c(12,18,26, 34,29,23,10,11,15,20,32,13,27,30,14)
)
# Create categorical abundance classes
benthos$abundance_class <- cut(benthos$worms_abundance,
  breaks = c(-Inf, 15, 25, Inf),
  labels = c("Low", "Medium", "High")
)
View(benthos)

ggplot(benthos, aes(x = sediment, y = worms_abundance, fill = abundance_class))+
  geom_bar(stat = "identity", position = "stack", colour = "grey90")

tbl <- table(benthos$sediment, benthos$abundance_class)
tbl <- matrix(c(63, 0, 0, 0, 23, 125, 12, 38, 53), nrow = 3)
tbl
chisq.test(tbl)


# Exercises ----

## significance Tests
# 1. 
# In the data folder you find a csv file with fish sizes sampled in two 
# marine protected ares (MPAs). Investigate wether the MPAs influence fish growth.
# Load and inspect the data set. Write a research question and the associated hypotheses (H1 and H0!).
# Choose and perform the correct t-Test. Interpret the results. 
# Familiarize your self with the topic "Test power", e.g. read the chapter 13.2.2
# in the book "Environmental Data Analysis" and try to apply this to your analysis.

# 2. 
# You conducted an experiment on diatoms using different light treatments, to
# analyze the effect of enhanced light on their growth. In the data set diatoms.csv
# you have the biomass from before and after the treatment. 
# Load and inspect the data set. Write a research question and the associated hypotheses (H1 and H0!).
# Choose and perform the correct Test. Interpret the results. 

# 3. 
# You are working on an aquaculture farm and have treated the females in one of the tanks
# with hormones to enhance the number of eggs. Did it work? 
# Explore graphically, formulate a hypothesis and run the correct test to validate your results.
fish <- data.frame("group_txt" = c(rep("without", 20), rep("with", 25)),
                   "group" = c(rep(1, 20), rep(2, 25)),
                   "result" = c(sample(1200:1400, 20, replace = F), 
                                sample(1250:1450, 15, replace = F), 
                                sample(1400:1500, 10,  replace = F)) )

# 4. 
# You are a fisheries managers and have been asked to analyze the effectiveness 
# of a temporary fisheries closure not on the fish itself but as a trade-off on a
# near-by living seabird colony. For this you have sampled the same nests before 
# and after the closure and counted the number of fish delivered to the chicklings.
# Load the data set "seabirds.csv" from the data folder. 
# Load and inspect the data set. Write a research question and the associated hypotheses (H1 and H0!).
# Choose and perform the correct Test. Interpret the results. 


## F-Test ----
# 5.
# You have dataset with swimming speeds of two tuna species. 
# Graphically explore the data set for the variance within the dataset. 
# Formulate a hypothesis for a F-Test.
# Conduct a F-Test and decide whether variances are homogenuous or not.
tuna <- data.frame("red_tuna" = rnorm(n = 20, mean = 85, sd = 5.454115),
                   "skipjack" = rnorm(n = 20, mean = 25, sd = 5.114222))

## Correlation ----
# 6.
# Use the penguins data set and test for bill length and depth the correlation
# First plot and explore the data. Formulate a hypothesis. 
# Decide which method you want to apply? Are the assumptions for a pearson correlation met? 
# Can you think of another variable influencing your results? Try to add this 
# in your analysis. 




