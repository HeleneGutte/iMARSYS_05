x_var <- rnorm(50)

y_var <- 3.7 + -2.2 * x_var + rnorm(length(x_var))
y_var <- exp(y_var)

plot(x_var, y_var)
plot(x_var, log(y_var))

m1 <- lm(log(y_var) ~ x_var)
m1 <- glm(y_var ~ x_var, family = 'poisson')

write.table(data.frame('x' = x_var, 'y' = y_var, 'sample' = 'a'), file = 'dataset_sample_a.csv', row.names = F)



# x_var <- rnorm(50)
# 
# y_var <- 3.7 + -2.2 * x_var + rnorm(length(x_var))
# y_var <- 1 / (1 + exp(-y_var))
# 
# plot(x_var, y_var)
# plot(x_var, log(y_var/(1-y_var)))
# 
# m1 <- lm(log(y_var/(1-y_var)) ~ x_var)
# m1 <- glm(y_var ~ x_var, family = 'binomial')



x_var <- rnorm(50)

y_var <- 0 + -2.2 * x_var + rnorm(length(x_var))
y_var <- 1 / (1 + exp(-y_var))

plot(x_var, y_var)
plot(x_var, log(y_var/(1-y_var)))

m1 <- lm(log(y_var/(1-y_var)) ~ x_var)
m1 <- glm(y_var ~ x_var, family = 'binomial')

write.table(data.frame('x' = x_var, 'y' = y_var, 'sample' = 'b'), file = 'dataset_sample_b.csv', row.names = F)



x_var <- rnorm(50)

y_var <- 0 + -2.2 * x_var + rnorm(length(x_var))
y_var <- 1 / (1 + exp(-y_var))
y_var[y_var <= 0.5] <- 0.000001
y_var[y_var > 0.5] <- 0.999999

plot(x_var, y_var)
plot(x_var, log(y_var/(1-y_var)))

m1 <- lm(log(y_var/(1-y_var)) ~ x_var)
m1 <- glm(y_var ~ x_var, family = 'binomial')

write.table(data.frame('x' = x_var, 'y' = y_var, 'sample' = 'c'), file = 'dataset_sample_c.csv', row.names = F)



x_var <- rnorm(50)

y_var <- 4.5**(x_var + rnorm(length(x_var)))

plot(x_var, y_var)
plot(x_var, log(y_var, base = 4.5))

write.table(data.frame('x' = x_var, 'y' = y_var, 'sample' = 'd'), file = 'dataset_sample_d.csv', row.names = F)



x_var <- rnorm(50)

y_var <- (6.2 + -7.8 * x_var + rnorm(length(x_var), sd = 2))**5

plot(x_var, y_var)
plot(x_var, y_var**(1/9.9))

write.table(data.frame('x' = x_var, 'y' = y_var, 'sample' = 'e'), file = 'dataset_sample_e.csv', row.names = F)

# x_var <- rnorm(50)
# 
# y_var <- (6.2 + -7.8 * x_var + rnorm(length(x_var), sd = 2))**9.9
# 
# plot(x_var, y_var)
# plot(x_var, y_var**(1/9.9))
# 
# write.table(data.frame('x' = x_var, 'y' = y_var, 'sample' = 'e'), file = 'dataset_sample_e.csv', row.names = F)



x_var <- rnorm(50)**4

y_var <- 3.5 + 6.2 * x_var**(1/4) + rnorm(length(x_var))
y_var <- y_var**2.7

plot(x_var, y_var)
plot(x_var**(1/4), y_var**(1/2.7))

write.table(data.frame('x' = x_var, 'y' = y_var, 'sample' = 'extra'), file = 'dataset_sample_extra.csv', row.names = F)







