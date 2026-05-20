
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
