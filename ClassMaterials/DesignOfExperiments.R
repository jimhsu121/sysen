# Load packages, to get our dplyr, ggplot, tibble, and readr functions
library(dplyr)
library(ggplot2)
library(tibble)
library(readr)
library(broom) # get our tidy() function

# Read data!
donuts = read_csv("workshops/donuts.csv")
# Having trouble reading in your data?
# You can also use this code:
donuts = read_csv("https://raw.githubusercontent.com/timothyfraser/sysen/main/workshops/donuts.csv")


# Check it out!
donuts %>% glimpse()

diff = donuts %>%
  group_by(type) %>%
  summarize(xbar = mean(weight))
diff

stat = diff %>%
  summarize(xbar_a = xbar[type == "a"],
            xbar_b = xbar[type == "b"],
            dbar = xbar_b - xbar_a)
stat
donuts %>%
  ggplot(mapping = aes(x = type, y = weight)) +
  geom_jitter(height = 0, width = 0.1, size = 3, alpha = 0.5,
              # Let's make a a bunch of donuts, using shape = 21 with white fill
              shape = 21, fill = "white", color = "goldenrod",
              # and increase the width of the outline with 'stroke'
              stroke = 5) +
  theme_classic(base_size = 14)

#LC1
diff_life = donuts %>%
  group_by(type) %>%
  summarize(xbar = mean(lifespan))

stat_life = diff_life %>%
  summarize(xbar_a = xbar[type == "a"],
            xbar_b = xbar[type == "b"],
            dbar = xbar_b - xbar_a)

stat_life

perm = tibble(rep = 1:1000) %>%
  group_by(rep) %>%
  summarize(donuts) %>%
  group_by(rep) %>%
  mutate(type = sample(type, size = n(), replace = FALSE)) %>%
  group_by(rep, type) %>%
  summarize(xbar = mean(weight)) %>%
  group_by(rep) %>%
  summarize(xbar_a = xbar[type == "a"],
            xbar_b = xbar[type == "b"],
            dbar = xbar_b - xbar_a)
