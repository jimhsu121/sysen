library(dplyr)
library(readr)

water = read_csv("workshops/onsen.csv")

set.seed(12345)

sbar_original = water %>%
  group_by(time) %>%
  summarize(s = sd(temp)) %>%
  summarize(sbar = mean(s))

sbar_original

myboot = tibble(reps = 1:1000) %>%
  group_by(reps) %>%
  reframe(water) %>%
  group_by(reps) %>%
  sample_n(size = n(), replace = TRUE)


mybootstat = myboot %>%
  group_by(reps, time) %>%
  summarize(s = sd(temp), .groups = "drop") %>%
  group_by(reps) %>%
  summarize(sbar = mean(s), .groups = "drop")

results = mybootstat %>%
  summarize(
    estimate = sbar_original$sbar,
    lower = quantile(sbar, probs = 0.025),
    upper = quantile(sbar, probs = 0.975),
    se = sd(sbar)
  )

results