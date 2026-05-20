
install.packages("ggpubr")
library(dplyr)
library("ggpubr")

data <- PlantGrowth

###re-order the data based on group

data$group <- ordered(data$group, levels = c("ctrl", "trt1", "trt2"))

##stats summary

group_by(data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

## visualise
# Plot weight by group and color by group

ggboxplot(data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

#compute ANOVA
res.aov <- aov(weight ~ group, data = data)
summary(res.aov)

# Tukey multiple pairwise- comparisons
TukeyHSD(res.aov)


#### Penguin data

setwd("/Users/chhaya/Documents/Course/2026_summer")

penguins <- read.csv("penguins.csv")

levels <- c("male", "female")

penguins$sex <- ordered(penguins$sex, levels = c("male", "female"))

penguins_1 = penguins[-1,]

group_by(penguins_1, sex) %>%
  summarise(
    count = n(),
    mean = mean(body_mass_g, na.rm = TRUE),
    sd = sd(body_mass_g, na.rm = TRUE)
  )

ggboxplot(penguins_1, x = "sex", y = "body_mass_g", 
          color = "sex", palette = c("#00AFBB", "#E7B800"),
          order = c("male", "female"),
          ylab = "Body mass (gm)", xlab = "Sex")

#compute ANOVA
res.aov <- aov(body_mass_g ~ sex, data = penguins_1)
summary(res.aov)

TukeyHSD(res.aov)
