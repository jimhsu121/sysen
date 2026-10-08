library(ggplot2)
library(dplyr)
mypois = rpois(1000, lambda = 459)
data.frame(x = sort(mypois), p = (1:1000) / 1000) %>% ggplot(aes(x = x, y = p)) + geom_line() + xlim(0, 1000)
c(sum(mypois > 400), sum(mypois > 450), sum(mypois > 500)) / 1000 * 400
