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
squid <- read.table(file = "data/SquidNorway.txt", header = TRUE, dec = ".")

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
plot(x = AHerringChile$age, y = AHerringChile$len)

nls_herring <- nls(len ~ a*(1 - exp(-b*(age - c))), data = AHerringChile, 
                   start = list(a = 17, b = 1, c = 0))
summary(nls_herring)
idx <- order(AHerringChile$age)

plot(x = AHerringChile$age, y = AHerringChile$len, 
     pch = 20, col="darkgray", main = "Age - Length",
     xlab = 'Age', ylab = 'Length')
lines(sort(AHerringChile$age), predict(nls_herring)[idx],col="red", lwd=2)


# Evaluate Goodness of fit
# correlation between observed and fitted values
cor(AHerringChile$len,predict(nls_herring)) 

# Residual sum of squares
(RSS.p <- sum(residuals(nls_herring)^2))

# Total sum of squares
(TSS <- sum((AHerringChile$len - mean(AHerringChile$len))^2))

# R-squared measure ~ explained variance
1 - (RSS.p/TSS) 


## 3.b. GAM for Heger Pierce data set: (bioluminescence) Sources ~ Depth
#Load the data
BL <- read.table(file = "data/HegerPierce.txt", header = TRUE)
str(BL)
names(BL)
unique(BL$Station)
plot(x = BL$Depth,  y = BL$Sources, xlab = "Depth",ylab ="Sources", 
     col = BL$Station)

BL_58 <- BL %>%
  filter(Station == 58)


bl_gam <- gam(Sources ~ s(Depth, k = 5, bs = "cr"), data = BL_58)
summary(bl_gam)

plot(bl_gam, pages = 1, resid = TRUE, pch = 16, cex = 0.7, cex.lab = 1.5)

par(mfrow = c(2,2))
gam.check(bl_gam)
# some patterns in the residuals, normality not really fulfilled
# patterns are probably due to uneven sampling: many low bioluminescence value
# and not so many high values were measured. 

bl_gam_pois <- gam(Sources ~ s(Depth, k = 5, bs = "cr"), data = BL_58, family = poisson())
summary(bl_gam_pois)
gam.check(bl_gam_pois)
# looks better



# 4. Exercises Block 2 ----
# See lecture slides 

# add Station as a fixed factor term to the GAM
BL <- BL %>%
  mutate(station_factor = as.factor(Station))

# first visualize the differences between the stations to see what we are analyzing:
ggplot(BL, aes(x = station_factor, y = Sources))+
  geom_boxplot()

bl_fix_station_gam <- gam(Sources ~ station_factor + s(Depth),
                          data = BL)
summary(bl_fix_station_gam)
# note that the summary output includes now for every factor level an individual 
# intercept in relation to the first factor level (Station 52). That means the
# intercept of the model is adjusted for each Station individually, while the 
# smoothing function of Depth is applied to all data points at once. 

# get the fitted values and plot them with the observed values to check the model
# fit. Use expand.grid() to get a data frame with every combination of the supplied
# vectors:
new_data <- expand.grid(Depth = seq(min(BL$Depth), max(BL$Depth), length.out = 200),
                       station_factor = levels(BL$station_factor))

# predict values for the new data frame:
new_data$fit <- predict.gam(bl_fix_station_gam, newdata = new_data)

ggplot(BL, aes(x = Depth, y = Sources))+
  geom_point(aes(colour = station_factor), alpha = 0.5)+
  geom_line(data = new_data, aes(y = fit, colour = station_factor))+
  facet_wrap(~station_factor)

# Also visually we can see that the smoothed line is the same for each of the 
# stations (try also turning the facetting on and off, to better see this), while
# the intercept changes for each station. 

# inspect the diagnostics: 
par(mfrow = c(2,2))
gam.check(bl_fix_station_gam)

# normality not fulfilled, use again cubic splines and a poission error distribution
# to improve the distribution of residuals: 
bl_fix_station_gam_pois <- gam(Sources ~ station_factor + s(Depth, bs = "cr"), 
                               family = poisson, data = BL)
summary(bl_fix_station_gam_pois)
# improved explained deviance
gam.check(bl_fix_station_gam_pois)
# no improved situation regarding the normality

bl_fix_station_gam_negbino <- gam(Sources ~ station_factor + s(Depth, bs = "cr"), 
                               family = nb(), data = BL)
summary(bl_fix_station_gam_negbino)
# improved explained deviance
gam.check(bl_fix_station_gam_negbino)
# better, still skewed, but I will leve it for now. 

# Investigate the other variables and decide which ones are interesting to 
# include in the analysis. 
pivot_longer(BL, Eddy:Longitude, names_to = 'Names', values_to = 'Values') %>%
  ggplot(aes(y = Sources, x = Values))+
  geom_point() + 
  geom_smooth(se=FALSE) + 
  facet_wrap(~Names, scales='free') + 
  theme_bw()

# the very high fluorescence value looks like a potential outlier --> remove 
# from analysis and then add fluorescence to the last GAM.
BL <- BL %>%
  filter(flcugl < 0.03)%>%
  mutate(flu_scale = as.vector(scale(flcugl)), 
         depth_scale = as.vector(scale(Depth)),
         sources_scale = as.vector(scale(Sources)))

# unnested GAM with three variables: 
bl_fixstat_dept_fl_unnest_gam <- gam(sources_scale ~ station_factor + s(depth_scale, bs = "cr") +
                                       s(flu_scale, bs = "cr"), data = BL, family = nb())
summary(bl_fixstat_dept_fl_unnest_gam)

plot(bl_fixstat_dept_fl_unnest_gam,
     pages = 1,
     residuals = TRUE,
     shade = TRUE)

# 3d plots with the two continuous variables
vis.gam(bl_fixstat_dept_fl_unnest_gam, view = c("flu_scale", "depth_scale"), 
        color = 'heat', theta=135)

par(mfrow = c(2,2))
gam.check(bl_fixstat_dept_fl_unnest_gam)
dev.off()
# similar to previous model

# Now try a nested interaction between Depth and fluorescence: 
bl_fixstat_dept_fl_nested_gam <- gam(sources_scale ~ station_factor + 
                                       s(depth_scale, flu_scale), 
                                     data = BL)

plot(bl_fixstat_dept_fl_nested_gam,
     pages = 1,
     residuals = TRUE,
     shade = TRUE)

# with this isoline plot we can see where higher and lower values of Sources will 
# likely be observed. In this case highest values are expected at medium fluorescence and 
# at shallower stations (check the 2.5 and 2 isoline at the left of the plot).