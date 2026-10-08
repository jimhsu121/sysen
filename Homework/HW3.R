library(tidyverse)
# Download data to your workshops folder
# then read it in!
products = read_csv("workshops/products.csv") %>%
  # Now turn it into a tidy data.frame we can compute statistics of quickly
  group_by(subgroup) %>%
  summarize(grams = c(grams1, grams2, grams3, grams4, grams5)) %>%
  ungroup()
