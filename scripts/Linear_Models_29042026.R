library(ggplot2)

set.seed(40)

#generate a dummy wood density datac(kg/m3)

n <- 100
density <- runif(n, min = 300, max= 900)

##generate timbwe hardness (N)

slope <- 5
intercept <- -500
noise <- rnorm(n, mean =0, sd =200)

hardness <- slope * density +intercept + noise

# create dataframe

data <- data.frame(Density = density, Hardness = hardness)

##fit linear model

model <- lm(Hardness ~ Density, data = data)
summary(model)
# plot data and regression line

ggplot(data, aes(x = Density, y =Hardness)) +
  geom_point() +
  geom_smooth(method ="lm", level = 0.95) +
  labs(title = "Regression with 95% CI",
       x ="Density (kg/m3)",
       y = "Hardness (N)") +
  theme_classic()

par(mfrow = c(2,2))
plot(model)
fitted(model)
fitted_values <- data.frame(model$fitted.values)
