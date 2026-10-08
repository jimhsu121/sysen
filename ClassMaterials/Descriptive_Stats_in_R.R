library(ggplot2) # for visualization
library(dplyr) # for pipelines!
library(MASS) # for fitting distributions

# You could code it as a vector, save it as an object, then use your functions!
sw = c(4.5, 5, 5.5, 5, 5.5, 6.5, 6.5, 6, 5, 4)
# View it
sw
length(sw)
sw %>% length()

sw %>% mean() 
sw %>% median()
sw %>% table() %>% sort(decreasing = TRUE) # mode
sw %>% sd()
var(sw) == sd(sw)^2 # This should return "TRUE"
sd(sw)/mean(sw) # Coefficient of variation

diff = sw - mean(sw)
# Check it out!
diff


# For example
mymean = sw %>% mean()
mysd = sw %>% sd()

# simulate!
mynorm = rnorm(n = 1000, mean = mymean, sd = mysd)

# Visualize!
mynorm %>% hist()

# LC1
aqi = c(12, 24, 50, 35, 36, 37, 40, 25, 28, 30, 32, 28)
aqi %>% length()
aqi %>% quantile(probs = 0.25)
quantile(aqi, probs = 0.75)

# LC2
cheese = c(5.52, 5.71, 5.06, 5.10, 4.98, 5.50, 4.81, 5.55, 4.74, 5.39)
mean(cheese) # returns 5.236
sd(cheese) # returns 0.3399085, which is how much the data varies on average
CoefficientOfVariation = sd(cheese)/mean(cheese)
CoefficientOfVariation # returns 0.06491759
StandardError = sd(cheese) / sqrt(length(cheese)) 
StandardError # returns 0.1074885

# LC3
seawall <- c(4.15, 4.35, 4.47, 4.74, 4.92, 5.19, 5.23, 5.35, 5.55, 5.70, 5.78, 7.16)
skewness <- sum( (seawall - mean(seawall))^3) / ((length(seawall) - 1) * sd(seawall)^3)
skewness # returns 0.9016585, skews to the right
kurtosis <- sum( (seawall - mean(seawall))^4) / ((length(seawall) - 1) * sd(seawall)^4)
kurtosis # returns 3.520149, quite sharp!

# LC4
# Make distribution of Corgis
corgi <- c(5, 1, 10, 3, 4, 3, 6, 4, 5, 2)

# Compute the mean
corgi_mean <- mean(corgi)
# Compute the standard deviation
corgi_sd <- sd(corgi)
# Compute the shape for gamma distribution
corgi_shape <- mean(corgi)^2 / var(corgi)
# Compute the rate for gamma distribution
corgi_rate <- 1 / (var(corgi) / mean(corgi) )
# Compute teh rate for exponential distribution
corgi_rate_e <- 1 / mean(corgi)

# Estimate the shape and scale parameters for a weibull distribution
corgi_stats <- corgi %>% fitdistr(densfun = "weibull")
# Extract the shape column
corgi_shape_w <- corgi_stats$estimate[1]
# Extract the scale column
corgi_scale_w <- corgi_stats$estimate[2]


# Create the data frame corgisim for visualization
corgisim <- bind_rows(
  data.frame(x = corgi, type = "Observed"),
  data.frame(x = rnorm(1000, mean = corgi_mean, sd = corgi_sd), type = "Normal"),
  data.frame(x = rpois(1000, lambda = corgi_mean), type = "Poisson"),
  data.frame(x = rgamma(1000, shape = corgi_shape, rate = corgi_rate), type = "Gamma"),
  data.frame(x = rexp(1000, rate = corgi_rate_e), type = "Exponential"),
  data.frame(x = rweibull(1000, shape = corgi_shape_w, scale = corgi_scale_w), type = "Weibull"))
# Visualization
ggplot(data = corgisim, mapping = aes(x = x, fill = type)) +
  geom_density(alpha = 0.3) +
  xlim(0,15) +
  labs(x = "Corgi Sightings!", y = "Density (Frequency)")
