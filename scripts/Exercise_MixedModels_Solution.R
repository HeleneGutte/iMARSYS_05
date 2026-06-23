library('lme4')

# fixed intercept
itc_base <- 3.5

# group-wise intercept add-ons
itc_g1 <- 2.3
itc_g2 <- -1.7

# fixed slopes
slp_1 <- -4.6
slp_2 <- 3.5

# group-wise slope add-ons
slp_g1 <- 0.27
slp_g2 <- -0.51

# generate explanatory variables "x1" and "x2"
inds_1 <- sample(seq(1,360), 180)
inds_2 <- sample(seq(1,360), 180) # indices for randomizing the sequences of x values and for introducing
# gaps in the sequences

x1 <- seq(3,60,length.out=360)[inds_1]
x2 <- seq(1.5,25,length.out=360)[inds_2]

# generate y values
noise <- rnorm(n = 180, mean = 0, sd = 1) # vary "sd" to increase / decrease magnitude of noise.
# How well your model will be able to predict the parameters correctly depends on number of data 
# points, magnitude of noise and the complexity of the equation

y_g1 <- ((itc_base + itc_g1) + slp_1 * x1[1:90] + (slp_2 + slp_g1) * x2[1:90]) + noise[1:90]
# the y-values for group #1
y_g2 <- ((itc_base + itc_g2) + slp_1 * x1[91:180] + (slp_2 + slp_g2) * x2[91:180]) + noise[91:180]
# the y-values for group #2
# Please note that there is absolutely no relationship between "x1 / x2" and "g1 / g2"

y <- c(y_g1, y_g2)

g <- c(rep('a', 90), rep('b', 90))  

plot(x1, y)
plot(x2, y) # look at how noisy your data are now

# Now fit a model on our artificial dataset
m1 <- lmer(y ~ (0 + 1 | g) + x1 + x2 + (0 + x2 | g))
# "0 + x2" means that the random intercept and slope are not correlated

# the summary will only show the estimates for the fixed effects
summary(m1)

# the "coef()" function shows the sums of the fixed effects and 
# the group-wise add-ons
coef(m1)

# hence the output of "coef(m1)" should (approximately) match the following:
print(itc_base + itc_g1)
print(itc_base + itc_g2) # these two should match the left-most column
print(slp_2 + slp_g1)
print(slp_2 + slp_g2) # these two should match the right-most column

## plot the coefficient effects

library('ggplot2')
ggplot() + 
  geom_point(aes(x1, y, color = g)) + 
  geom_line(aes(x1[g == 'a'], coef(m1)[['g']][1,1] + coef(m1)[['g']][1,2] * x1[g == 'a'] + coef(m1)[['g']][1,3] * mean(x2[g == 'a'])), 
            color = 'red') + 
  geom_line(aes(x1[g == 'b'], coef(m1)[['g']][2,1] + coef(m1)[['g']][2,2] * x1[g == 'b'] + coef(m1)[['g']][2,3] * mean(x2[g == 'b'])), 
            color = 'green')

## contrast with "wrong" normal LM:

mw <- lm(y ~ x1 + x2)

summary(mw)

par(mfrow = c(2,2))
plot(m1) # the residuals-diagnostics plot for the mixed model
plot(mw) # the diagnostics plots for the normal LM

sum(abs(resid(m1)))
sum(abs(resid(mw))) # sum of absolute differences between obervations and predictions

## contrast with "wrong" normal LM with group as additional variable:

mw <- lm(y ~ x1 + x2 + g)

summary(mw)

par(mfrow = c(2,2))
plot(m1)
plot(mw) # here it's hard to tell if the mixed model is better or if the LM is better

sum(abs(resid(m1)))
sum(abs(resid(mw))) # sum of absolute differences between obervations and predictions

