# Load dplyr and tibble for data wrangling
library(dplyr)
library(tibble)
# Load DiagrammeR, which we'll use to make diagrams today!
library(DiagrammeR)


f = tibble(
  # Make a vector of routes to failure
  failure_mode = c(
    "freezer --> fail_break",
    "loading --> fail_time",
    "loading --> fail_eat",
    "transport --> fail_time",
    "transport --> fail_eat")
)

  # Worker 2 could leave ice cream out while loading
  # Worker 2 could eat the ice cream while loading
  # Worker 3 could leave the ice cream out in transit
  # Worker 3 could eat the ice cream in transit

  
f1 = f %>%
  mutate(severity = c(8, 5, 1, 5, 1))
# Check out the contents!
f1
