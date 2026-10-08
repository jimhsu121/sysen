# AnalyzeYourVector.R

library(readr)
library(dplyr)
library(ggplot2)
library(MASS)

# Assumes your working directory is the project root (sysen/),
# same as 02_workshop.R
la = read_csv("workshops/la_parishes.csv")

# Look first
glimpse(la)
la

# My vector: percent of housing damage per parish
pcd <- la$pc_damage

# Step 1
hist(pcd)

pcd_n <- length(pcd)
pcd_mean <- mean(pcd)
pcd_median <- median(pcd)
pcd_min <- min(pcd)
pcd_max <- max(pcd)
pcd_range <- pcd_max - pcd_min

c(n = pcd_n, mean = pcd_mean, median = pcd_median,
  min = pcd_min, max = pcd_max, range = pcd_range)

# On average, about 50% of each parish is damaged, but the actual demage level varies a lot.


# Step 2
pcd_sd <- sd(pcd)
pcd_var <- var(pcd)
pcd_cv <- pcd_sd / pcd_mean
pcd_se <- pcd_sd / sqrt(pcd_n)

c(sd = pcd_sd, var = pcd_var, cv = pcd_cv, se = pcd_se)

# The standard deviation is about 40% of the average, indicating wide variation. The standard error is 4.56, which means our confidence that the real mean sits within the range of mean +-4.56 is 68%.

# Step 3
pcd_skewness <- sum((pcd - pcd_mean)^3) / ((pcd_n - 1) * pcd_sd^3)
pcd_kurtosis <- sum((pcd - pcd_mean)^4) / ((pcd_n - 1) * pcd_sd^4)

c(skewness = pcd_skewness, kurtosis = pcd_kurtosis)

# The skewness is 0.29 and the kurtosis is 1.84, meaning the distribution drifts slightly toward values smaller than the average, and the data points are relatively sparse.

# Step 4:
pcd_shape_gamma <- pcd_mean^2 / pcd_var
pcd_rate_gamma <- pcd_mean / pcd_var
pcd_rate_exp <- 1 / pcd_mean

pcd_sim <- bind_rows(
  data.frame(x = pcd, type = "Observed"),
  data.frame(x = rnorm(100, mean = pcd_mean, sd = pcd_sd), type = "Normal"),
  data.frame(x = rpois(100, lambda = pcd_mean), type = "Poisson"),
  data.frame(x = rgamma(100, shape = pcd_shape_gamma, rate = pcd_rate_gamma), type = "Gamma"),
  data.frame(x = rexp(100, rate = pcd_rate_exp), type = "Exponential"))

ggplot(data = pcd_sim, mapping = aes(x = x, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Percent Housing Damage", y = "Density (Frequency)")

# Normal distribition matches our observation the best.