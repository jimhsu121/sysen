library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)

# Our onsen data: 8 monthly subgroups of 20 samples each
water = read_csv("workshops/onsen.csv")

g1 = ggplot(water, aes(x = time, y = temp, group = time)) + geom_boxplot()
g2 = ggplot(water, aes(x = temp)) +
  geom_histogram(bins = 15, color = "white", fill = "grey") +
  theme_void() + coord_flip()

# YOUR TURN: explain each argument, then try other widths.
p1 = ggarrange(g1, g2, widths = c(5, 1), align = "h")
ap1
