library(dplyr)
library(readr)

# Our onsen data: 8 monthly subgroups (time) of 20 samples each
water = read_csv("workshops/onsen.csv")

# Nudge: an "Extra Hot" onsen has spec limits 42 and 50.
# Nudge: build Cp and Cpk from the chapter; look at which one needs the mean.
library(dplyr)
library(readr)

water <- read_csv("workshops/onsen.csv")

LSL <- 42
USL <- 50

# Within-subgroup sigma: pooled SD across the 8 time subgroups
sigma_within <- water %>%
  group_by(time) %>%
  summarise(v = var(temp)) %>%
  summarise(s = sqrt(mean(v))) %>%
  pull(s)

sigma_overall <- sd(water$temp)
mu <- mean(water$temp)

# Cp, Pp
Cp <- (USL - LSL) / (6 * sigma_within)
Pp <- (USL - LSL) / (6 * sigma_overall)

# Cpk, Ppk
Cpk <- min((USL - mu) / (3 * sigma_within), (mu - LSL) / (3 * sigma_within))
Ppk <- min((USL - mu) / (3 * sigma_overall), (mu - LSL) / (3 * sigma_overall))

round(c(Cp = Cp, Cpk = Cpk, Pp = Pp, Ppk = Ppk), 2)

