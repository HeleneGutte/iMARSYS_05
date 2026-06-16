# Day 9 - Non-linear regression and generalized additive models

# packages ----
library(FSAdata)
library(mgcv)
library(tidyverse)


# 1. Example Non-linear regressions ----
data(BluegillLM)

# length and weight data of 100 Bluegills (Lepomis macrochirus) from Lake Mary 
# in Minnesota. 
str(BluegillLM)

idx <- order(BluegillLM$tl)

BluegillLM <- BluegillLM %>%
  select(tl, wght) %>% 
  mutate(across(everything(), na.omit))

plot(x = BluegillLM$tl, y = BluegillLM$wght, 
     pch=20, col="darkgray", main = "Length-Weight",
     xlab = 'Total length', ylab = 'Weight')

# model weight of the Bluegills as a function of total length, with the following function: 
# weight = a * tl^b. Thus the parameters a and be must be estimated
m1_ls <- nls(wght ~ a*(tl^b), data = BluegillLM, start = list(a = 1, b = 3))
summary(m1_ls)
plot(m1_ls)

plot(x = BluegillLM$tl, y = BluegillLM$wght, 
     pch = 20, col="darkgray", main = "Length-Weight",
     xlab = 'Total length', ylab = 'Weight')
lines(sort(BluegillLM$tl), predict(m1_ls)[idx],col="red", lwd=2)


# Evaluate Goodness of fit
# correlation between observed and fitted values
cor(BluegillLM$wght,predict(m1_ls)) 

# Residual sum of squares
(RSS.p <- sum(residuals(m1_ls)^2))

# Total sum of squares
(TSS <- sum((BluegillLM$wght - mean(BluegillLM$wght))^2))

# R-squared measure ~ explained variance
1 - (RSS.p/TSS) 




# 2. Example GAMs ----
squid <- read.table(file = "data/SquidNorway.txt",header = TRUE,dec = ".")

# data set contains data on isotopic ratios (d15N) measured in oceanic squids 
# (Gonatus fabricii) depending on the latitude, depth and their mantle length.
View(squid)

# get a first overview how data are distributed: 
pivot_longer(squid, Lat:ML, names_to = 'Names', values_to = 'Values') %>%
  ggplot(aes(y = d15N, x = Values))+
  geom_point() + 
  geom_smooth(span = 0.8, se=FALSE) + 
  facet_wrap(~Names, scales='free') + 
  theme_bw()

# because we are dealing with variables on different scales, we will first standardize the data.
squid_std <- squid %>% 
  mutate(across(.cols = c("Lat", "ML", "Depth"), scale)) %>%
  mutate(across(.cols = c("Lat", "ML", "Depth"), as.vector))
  
## One predictor - Cubic splines ----
m1_gam <- gam(d15N ~ s(ML, bs = 'cr'), data = squid_std)
summary(m1_gam)
# Intercept indicates the mean, 
# the spline is centered around zero, 
# R2-value is as usual the proportion of variance explained by the model. 
# Scale estimate is the variance of the residual. 
# Also provided is ‘deviance explained’, which in this setting is identical to 
# the unadjusted R2, but for non-Gaussian families would be preferred.
# GCV score for one model not so interesting, but when comparing several e.g. with different splines.

plot(m1_gam, pages=1, resid = TRUE, pch = 16, cex = 0.7, cex.lab = 1.5)

# plot diagnostics
par(mfrow=c(2,2))
gam.check(m1_gam) #check plots
dev.off()


## Multivariate GAM ----
# Cubic splines
m2_gam <- gam(d15N ~ Lat + s(ML, bs = 'cr'), data = squid_std)
summary(m2_gam)
# summary is splited for fixed and smoother terms
plot(m2_gam, pages = 1, resid = TRUE, pch = 16, cex = 0.7, cex.lab = 1.5)

vis.gam(m2_gam, view = c('Lat', 'ML'), color = 'heat') #3d plots useful for interaction terms
visreg2d(m2_gam, xvar = 'Lat', yvar = 'd15N', scale = 'response')
vis.gam(m2_gam, view = c("Lat","ML"), plot.type = "contour", color = "heat")
par(mfrow = c(2,2))
gam.check(m2_gam) #check plots
dev.off()

## Interaction terms
# Interaction with thin plate
# data prep: 
cet <- read.table(file = "data/meantemp_monthly_totals.txt", header = TRUE, dec = ".")
names(cet) <- c("Year", month.abb, "Annual")
cet <- cet[-nrow(cet), ]
rn <- as.numeric(rownames(cet))
Years <- min(cet$Year):max(cet$Year)
cet <- cet[, -1]
cet <- cet[, -ncol(cet)]
## stack the data
cet <- utils::stack(cet)[,2:1]
names(cet) <- c("Month","Temperature")
## add in Year and nMonth for numeric month and a proper Date class
cet <- transform(cet,
                 Year = (Year <- rep(Years, times = 12)),
                 nMonth = rep(1:12, each = length(Years)))
## sort into temporal order
cet <- cet[with(cet, order(Year, Month)), ]
cet

# Without nesting
yearcc_gam <- gam(Temperature ~ s(Year) + s(nMonth), data = cet)
summary(yearcc_gam)
plot(yearcc_gam, page = 1, scale = 0,cex = 1.7)
par(mfrow=c(2,2))
gam.check(yearcc_gam)
dev.off()
vis.gam(yearcc_gam, view = c('Year', 'nMonth'), color = 'heat', theta=135) #3d plots useful for interaction terms

#With nesting
yearnest_gam <- gam(Temperature ~ s(Year, nMonth), data = cet)
summary(yearnest_gam)
plot(yearnest_gam, page = 1, scale = 0,residuals = TRUE,cex = 1.7)
par(mfrow=c(2,2))
gam.check(yearnest_gam)
dev.off()
vis.gam(yearnest_gam, view = c('Year', 'nMonth'), color = 'heat', theta=135)#3d plots useful for interaction terms


# 3. Exercises Block 1 ----

## 3.a. Von Bertalanffy ----
# Fit a non-linear regression, according to the function given in the lecture. 
data(AHerringChile)

# Find out what kind of data you loaded: 
help.search("AHerringChile")

# Plot the data, write a model, evaluate the model fit, graphically and by 
# calculating correlation and R-squared.


## 3.b. GAM for Heger Pierce data set: (bioluminescence) Sources ~ Depth


# 4. Exercises Block 2 ----
# See lecture slides 
