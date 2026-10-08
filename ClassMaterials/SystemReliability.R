library(dplyr)
library(tibble)
library(ggplot2)
library(broom)
library(DiagrammeR)

# Let's imagine a normally distributed lifespan for these cars...
lifespan = rnorm(100, mean = 5, sd = 1)
# We can build ourself the PDF of our lifetime distribution here
dlife = lifespan %>% density() %>% tidy() %>% approxfun

# And we can build the CDF here
plife = lifespan %>% density() %>% tidy() %>%
  mutate(y = cumsum(y) / sum(y)) %>% approxfun()

mycars = tibble(
  time = seq(min(lifespan), max(lifespan), by = 0.1),
  # Get probability of failing at time time
  prob = time %>% dlife(),
  # Get probability of failing at or before time t
  prob_cumulative = time %>% plife(),
  # Get probability of surving past time t 
  # (NOT failing at or before time t) 
  prob_survival = 1 - time %>% plife())

ggplot() +
  # Make one area plot for Cumulative Probability (CDF)
  geom_area(data = mycars,
            mapping = aes(x = time, y = prob_cumulative,
                          fill = "Cumulative Probability"), alpha = 0.5) +
  # Make one area plot for Relibability
  geom_area(data = mycars,
            mapping = aes(x = time, y = prob_survival,
                          fill = "Reliability (Survival)"), alpha = 0.5) +
  # Make one area plot for Probability (PDF)
  geom_area(data = mycars,
            mapping = aes(x = time, y = prob,
                          fill = "Probability"), alpha = 0.5) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Lifespan of Car", y = "Probability",
       subtitle = "Example Life Distributions")


# LC1
days <- 500
units <- 5
m <- days / units
# Check it!
m
fexplode = function(days){ 1 - exp(-1*days*0.01) }
curve(fexplode(x), from = 0, to = 365.25)
1 - fexplode(365.25 / 2) # Survival rate of the phones after 6 months, which returns 0.161.
1 - fexplode(365.25) # Survival rate of the phones after 1 year, which returns 0.026.

# LC2
ramen = c(163, 309, 215, 211, 246, 198, 281, 180, 317, 291,
           238, 281, 215, 208, 212, 300, 231, 240, 285, 232,
           252, 261, 310, 226, 282, 140, 208, 280, 237, 270,
           185, 409, 293, 164, 231, 237, 269, 233, 246, 287,
           187, 232, 180, 227, 215, 260, 236, 229, 263, 220)
# Get failure function f(t) = CDF of ramen failure
framen <- ramen %>% density() %>% tidy() %>%
  # Now compute CDF
  mutate(y = cumsum(y) / sum(y)) %>%
  approxfun()
# Get survival function r(t) = 1 - f(t)
rramen <- function(days){ 1 - framen(days) }
# Get average failure rate from time 1 to time 2
afrramen <- function(days1, days2){
  h1 <- -1*log(rramen(days1))
  h2 <- -1*log(rramen(days2))
  (h2 - h1) / (days2 - days1)
}

framen(240) # Calculates the percentage of the ramen that went bad after 8 months, which returns 0.508
lambda <- afrramen(240, 365)
lambda # Returns the average failure rate between 8 months to 1 year, which is 0.0256
m <- 1 / lambda
m # Returns the mean time to fail between 8 months to 1 year, which is 39 days

# In-class practice
fplane = function(days){1 - exp(-(days/2000)^0.5)}
curve(fplane, from = 0, to = 1000)

lambda = 5/500
mttf = 500/5 # mean time to failure in days
t = seq(0, 400, 5)

# tibble is a smarter function to create data frames where you can do inter-frame calculation.
tibble(
    t = t,
    prob_f = 1 - exp(-lambda*t),
    prob_r = 1 - prob_f
    )

