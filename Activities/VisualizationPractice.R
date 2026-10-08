library(ggplot2)
library(dplyr)

diamonds = ggplot2::diamonds %>% sample_n(1000)

ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = color)) + geom_point()
ggplot(data = diamonds, mapping = aes(x = clarity, y = depth)) + geom_boxplot(color = 'red')


