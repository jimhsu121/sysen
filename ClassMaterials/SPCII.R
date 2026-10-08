library(dplyr)
library(readr)
library(tibble)
library(ggplot2)
library(viridis)
source("functions/functions_process_control.R")

# Let's import our samples of bathwater over time!
water = read_csv("workshops/onsen.csv")
# Take a peek!
water %>% glimpse()

#LC1
lower = 2 - 0.05
upper = 2 + 0.05
sigma = 0.02
mu = 2.01

# Cp = |2.05 - 1.95| / (6 * 0.02) = 0.8333
cp(sigma_s = sigma, upper = upper, lower = lower)

# Cpk = min(2.05 - 2.01, 2.01 - 1.95) / (3 * 0.02) = 0.6667
cpk(mu = mu, sigma_s = sigma, lower = lower, upper = upper)
