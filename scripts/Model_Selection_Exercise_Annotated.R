### generate the artificial data ###

x1 <- runif(20, 0, 10) # note that since I'm randomly sampling here without a seed,
# your results will look (slightly) different compared to mine
x2 <- runif(20, 5, 17)
x3 <- runif(20, 3, 25)

itc <- 5.5
a1 <- 3.9
a2 <- 7.6
a3 <- 5.4

y <- itc + a1 * x1 + a2 * x2 # this is the equation underlying the y-values. So 
# technically, the best model should match this equation. However, with the noise 
# added, a simpler model might be a better description of the data given the model-
# selection criteria we discussed. As we discussed, reporting multiple models with 
# similar performance is a good way to address ambiguity in the model-selection process
y <- y + rnorm(length(y), 0, sd(y)/4) # Here, noise is added to make the relationship 
# less obvious

### Fitting the models ###

mod1 <- lm(y ~ x1 + x2 + x3 + x1*x2 + x1*x3 + x2*x3 + x1*x2*x3) # this is the model with 
# the highest complexity possible given the structure of the data: The only possible 
# three-way interaction, all possible two-way interactions and all variables as single / 
# isolated terms
mod2 <- lm(y ~ x1 + x2 + x3 + x1*x2 + x1*x3 + x2*x3) # dropping the three-way interaction
mod3a <- lm(y ~ x1 + x2 + x3 + x1*x2 + x1*x3)
mod3b <- lm(y ~ x1 + x2 + x3 + x1*x2 + x2*x3)
mod3c <- lm(y ~ x1 + x2 + x3 + x1*x2 + x1*x3) # iteratively dropping one two-way interaction
mod4a <- lm(y ~ x1 + x2 + x3 + x1*x2)
mod4b <- lm(y ~ x1 + x2 + x3 + x1*x3)
mod4c <- lm(y ~ x1 + x2 + x3 + x2*x3) # iteratively dropping another two-way interaction
mod4a <- lm(y ~ x1 + x2 + x1*x2)
mod4b <- lm(y ~ x1 + x3 + x1*x3)
mod4c <- lm(y ~ x2 + x3 + x2*x3) # three models, each with one two-way interaction and with the 
# single / isolated variables appearing in that interaction (and without the third one)
mod5 <- lm(y ~ x1 + x2 + x3) # the most complex model without interactions
mod6a <- lm(y ~ x1 + x2)
mod6b <- lm(y ~ x1 + x3)
mod6c <- lm(y ~ x2 + x3) # three models with two variables and without interaction
mod7a <- lm(y ~ x1)
mod7b <- lm(y ~ x2)
mod7c <- lm(y ~ x3) # the three simplest models (only one variable)

### Evaluating / comparing the models ###

## the summary statistics. Look here especially for the adjusted R-squared value (=deviance explained) at the bottom 
# of the summary output. This value lies in the range 0 --> 1. The closer it is to 1, the better 
# (from deviance-explained point-of-view). In ecology, a rule of thumb is that R-squared >= 0.4 is sort of acceptable. 
# If my memory is correct, then adjusted R-squared accounts for the degrees of freedom in your model (the number of degrees 
# depends on the number of data points and the number of model parameters) ##

summary(mod1)
# ...
summary(mod7c)

## the AIC values. The lower the AIC value, the better; however, the difference between AIC values should be >= 2 to make 
# a proper comparison of models. The AIC weighs the predictive skill of the model (R-squared) against the number of model 
# parameters (according to the principle of Occam's razor, the fewer the better - or more precisely, the fewer parameters,
# the less number of assumptions that could be falsified). To compare AIC values between two models, both models must be 
# fitted with exactly the same y-values ##

AIC(mod1)
# ...
AIC(mod7c)

## the diagnostics plots. They enable you to assess the quality of your model in terms of the distribution of residuals. 
# The upper left plot shows the residuals against the predicted y-values. Here, the residuals should be homogenous over 
# the full x-axis in terms of frequency of magnitudes of deviation and frequency of directions (-, +) of deviations. 
# Also, there should be no trend in the residuals, i.e. they should be centered around a y-value of zero over the full 
# x-axis (the red line also gives you a visual help). The plot on the lower left should be interpreted about equally to 
# the upper left one. The upper right plot, if I remember correctly, shows the quantiles of the model residuals against 
# the quantiles of an ideal normal distribution. As we want the residuals to be normally (Gaussian) distributed, the ideal 
# would be that all points lie on the horizontal line. In practice, that is never exactly the case, especially the "tails" 
# tend to diverge from the line. But you can use the plot well to compare different models. The lower right plot shows the 
# impact that each data points has on the model characteristics; i.e. it shows what happens when you remove that data point 
# from the dataset and re-fit your model on the subset. If the impact is strong, then the data point will appear close to or 
# beyond a red dashed line. In that case, that data point might be an outlier, or your model might have a high sensitivity. 
# So if you have such data points, it is a sign that a different model might be appropriate (if you know your experiment 
# really well, you might also consider removing the data point, especially if you know that a measurement was faulty).

# We also look at the histogram of residuals. This should ideally follow a Gaussian (bell-shaped) curve, but with few data 
# points, it is often difficult to assess whether or not it is "Gaussian enough". It is good for comparing models. The information 
# conveyed by that plot will likely match that conveyed by the upper right diagnostics plot.

# Finally we plot the model predictions against the actual y-values and also plot a line with slope 45 °. For a perfect fit, 
# all points should lie on that line. Ideally, the scatter around the line should be homogenous over the full range of x-values. 
# It is essentially a different represnetation of what you can see in the upper left diagnostic plot ##

par(mfrow = c(2,2)) # prepare the plot panel to show four plots on one "slide"

plot(mod1)

hist(resid(mod1))

plot(y, predict(mod1))
abline(0,1)

# ...

plot(mod1)

hist(resid(mod1))

plot(y, predict(mod1))
abline(0,1)

## After accounting for R-squared, AIC and diagnostics plots, also take the principle of Occam's razor into account. 
# If model choice is really ambiguous, i.e. the three metrics really don't look different for different models between 
# which to decide, go for the least complex model (now interactions, fewest parameters / variables). But in such a case 
# you would also report the other models and the fact that it is not really possible to make a model selection without 
# ambiguity ##

### Write the data to a .csv file. This is essentially the data set you got from me  (but see comment on line 3) ###

write.csv(data.frame('x1' = x1, 'x2' = x2, 'x3' = x3, 'y' = y), file = 'Model_Selection.csv', row.names = F)



