library(dplyr)
library(readr)
library(tibble)
library(ggplot2)
library(viridis)
# you'll probably need to install these packages!
# install.packages(c("ggpubr", "moments")) 
library(ggpubr)
library(moments)

# Let's import our samples of bathwater over time!
water = read_csv("workshops/onsen.csv")
# Take a peek!
water %>% glimpse()

# By running theme_set()
theme_set(
  # we tell ggplot to give EVERY plot this theme
  theme_classic(base_size = 14) +
  # With these theme traits, including
  theme(
    # Putting the legend on the bottom, if applicable
    legend.position = "bottom",
    # horizontally justify plot subtitle and caption in center
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    # Getting rid of busy axis ticks
    axis.ticks = element_blank(),
    # Getting rid of busy axis lines
    axis.line = element_blank(),
    # Surrounding the plot in a nice grey border
    panel.border = element_rect(fill = NA, color = "grey"),
    # Remove the right margin, for easy joining of plots
    plot.margin = margin(r = 0)
  )
)

describe = function(x){
  # Put our vector x in a tibble
  tibble(x) %>%
    # Calculate summary statistics
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      # We'll use the moments package for these two
      skew = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE)) %>%
    # Let's add a caption, that compiles  all these statistics
    mutate(
      # We'll paste() the following together
      caption = paste(
        # Listing the name of each stat, then reporting its value and rounding it, then separating with " | "
        "Process Mean: ", mean %>% round(2), " | ",
        "SD: ", sd %>% round(2), " | ",
        "Skewness: ", skew %>% round(2), " | ",
        "Kurtosis: ", kurtosis %>% round(2),
        # Then make sure no extra spaces separate each item
        sep = "")) %>%
    return()
}
# Run descriptives!
tab = water$temp %>% describe()
# Check it out!
tab
# Make the initial boxplot...
g1 = water %>%
  ggplot(mapping = aes(x = time, y = temp, group = time)) +
  # Plot grand mean
  geom_hline(mapping = aes(yintercept = mean(temp)), color = "lightgrey", size = 3) +
  # Plot points and boxplots 
  geom_jitter(height = 0, width = 0.25) +
  geom_boxplot() +
  labs(x = "Time (Subgroup)", y = "Temperature (Celsius)",
       subtitle = "Process Overview",
       # Add our descriptive stats in the caption!
       caption = tab$caption)
# Part 1 of plot
g1
# Make the histogram, but tilt it on its side
g2 = water %>%
  ggplot(mapping = aes(x = temp)) +
  geom_histogram(bins = 15, color = "white", fill = "grey") +
  theme_void() +   # Clear the theme
  coord_flip()   # tilt on its side
# Part 2 of plot
g2
# Then bind them together into 1 plot, 'h'orizontally aligned.
p1 = ggarrange(g1,g2, widths = c(5,1), align = "h")
# Check it out!
p1

# LC1
ggprocess = function(x, y, xlab = "Subgroup", ylab = "Metric"){

  # Get descriptive statistics
  tab = describe(y)

  # Make the initial boxplot...
  g1 = ggplot(mapping = aes(x = x, y = y, group = x)) +
    # Plot grand mean
    geom_hline(mapping = aes(yintercept = mean(y)), color = "lightgrey", size = 3) +
    # Plot points and boxplots 
    geom_jitter(height = 0, width = 0.25) +
    geom_boxplot() +
    labs(x = xlab, y = ylab,
         subtitle = "Process Overview",
         # Add our descriptive stats in the caption!
         caption = tab$caption)

  # Make the histogram, but tilt it on its side
  g2 = ggplot(mapping = aes(x = y)) +
    geom_histogram(bins = 15, color = "white", fill = "grey") +
    theme_void() +   # Clear the them
    coord_flip()   # tilt on its side

  # Then bind them together into 1 plot, 'h'orizontally aligned.
  p1 = ggarrange(g1,g2, widths = c(5,1), align = "h")

  return(p1)
}

# Visualize it!
ggprocess(x = water$time, y = water$ph)

# Back to the topic...
# Calculate short-term statistics within each group
stat_s = water %>%
  # For each timestpe
  group_by(time) %>%
  # Calculate these statistics of interest!
  summarize(
    # within-group mean
    xbar = mean(temp),
    # within-group range
    r = max(temp) - min(temp),
    # within-group standard deviation
    sd = sd(temp),
    # within-group sample size
    nw = n(),
    # Degrees of freedom within groups
    df = nw - 1) %>%
  # Last, we'll calculate sigma_short (within-group variance)
  # We're going to calculate the short-term variation parameter sigma_s (sigma_short)
  # by taking the square root of the average of the standard deviation
  # Essentially, we're weakening the impact of any special cause variation
  # so that our sigma is mostly representative of common cause (within-group) variation
  mutate(
    # these are equivalent
    sigma_s = sqrt( sum(df * sd^2) / sum(df) ),
    sigma_s = sqrt(mean(sd^2)),
    # And get standard error (in a way that retains each subgroup's sample size!)
    se = sigma_s / sqrt(nw),
    # Calculate 6-sigma control limits!
    upper = mean(xbar) + 3*se,
    lower = mean(xbar) - 3*se)

# Check it!
stat_s %>% head(3)


# LC2
# Get the within-group stats for ph!
ph_s = water %>%
  group_by(time) %>%
  summarize(
    xbar = mean(ph),
    r = max(ph) - min(ph),
    sd = sd(ph),
    nw = n(),
    df = nw - 1) %>%
  mutate(
    sigma_s = sqrt(mean(sd^2)),
    se = sigma_s / sqrt(nw),
    upper = mean(xbar) + 3*se,
    lower = mean(xbar) - 3*se)

# Let's extract some labels
labels = ph_s %>%
  reframe(
    time = max(time),
    type = c("xbbar",  "upper", "lower"),
    name = c("mean", "+3 s", "-3 s"),
    value = c(mean(xbar), unique(upper), unique(lower)),
    value = round(value, 2),
    text = paste(name, value, sep = " = "))

# and let's visualize it!
ph_s %>%
  ggplot(mapping = aes(x = time, y = xbar)) +
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  # Plot labels
  geom_label(data = labels, mapping = aes(x = time, y = value, label = text),  hjust = 1)  +
  labs(x = "Time (Subgroups)", y = "Average pH",
       subtitle = "Average and Standard Deviation Chart")


# LC3
# Let's calculate our own d function
dn = function(n, reps = 1e4){
  # For 10,0000 reps
  tibble(rep = 1:reps) %>%
    # For each rep,
    group_by(rep) %>%
    # Simulate the ranges of n values
    summarize(r = rnorm(n = n, mean = 0, sd = 1) %>% range() %>% diff() %>% abs()) %>%
    ungroup() %>%
    # And calculate...
    summarize(
      # Mean range
      d2 = mean(r),
      # standard deviation of ranges
      d3 = sd(r),
      # and constants for obtaining lower and upper ci for rbar
      D3 = 1 - 3*(d3/d2), # sometimes written D3
      D4 = 1 + 3*(d3/d2), # sometimes written D4
      # Sometimes D3 goes negative; we need to bound it at zero
      D3 = if_else(D3 < 0, true = 0, false = D3) ) %>%
    return()
}
# Let's try it, where subgroup size is n = 2
dn(n = 2)
# Let's get the constants we need too.
# Each of our samples has a sample size of 20
d = dn(n = 20)

# Check it!
d
# Let's get within group range for temperature...
stat_w = water %>%
  group_by(time) %>%
  summarize(r = temp %>% range() %>% diff() %>% abs(),
            n_w = n()) # get subgroup size
# Let's get average within group range for temperature...
stat = stat_w %>%
  summarize(rbar = mean(r), # get Rbar
            n_w = unique(n_w)) # assuming constant subgroup size...
# Check it!
stat
# We find that dn() gives us constants D3 and D4...
mydstat = dn(n = stat$n_w)
mydstat
stat %>%
  mutate(rbar_lower = rbar * mydstat$D3,
         rbar_upper = rbar * mydstat$D4) %>%
  select(rbar, rbar_lower, rbar_upper)
# So quick! You could use these values to make a range chart now.

remove(stat, stat_w)
# Let's write a function bn() to calculate our B3 and B4 statistics for any subgroup size n
bn = function(n, reps = 1e4){
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarize(s = rnorm(n, mean = 0, sd = 1) %>% sd()) %>%
    summarize(b2 = mean(s),
              b3 = sd(s),
              C4 = b2, # this is sometimes called C4
              A3 = 3 / (b2 * sqrt( n  )),
              B3 = 1 - 3 * b3/b2,
              B4 = 1 + 3 * b3/b2,
              # bound B3 at 0, since we can't have a standard deviation below 0
              B3 = if_else(B3 < 0, true = 0, false = B3)) %>%
    return()
}
# Let's get within group standard deviation for temperature...
stat_w = water %>%
  group_by(time) %>%
  summarize(s = temp %>% sd(),
            n_w = n()) # get subgroup size

stat_w
# Let's get average within group range for temperature...
stat = stat_w %>%
  summarize(sbar = mean(s), # get Rbar
            n_w = unique(n_w)) # assuming constant subgroup size...
# Check it!
stat
# For a subgroup size of 20...
stat$n_w
# Let's get our B constants!
mybstat = bn(n = stat$n_w)

# Check it out!
mybstat
stat = stat %>%
  # Add our constants to the data.frame...
  mutate(mybstat) %>%
  # Calculate 3 sigma control limits
  mutate(sbar_lower = sbar * B3,
         sbar_upper = sbar * B4)

# Check it out!
stat %>%
  select(sbar, sbar_lower, sbar_upper)
# Let's bind them together!
dx = bind_rows(
  dn(2), dn(3), dn(4), dn(5),
  dn(6), dn(7), dn(8), dn(9), dn(10)) %>%
  mutate(n = 2:10) %>%
  select(n, d2, d3)

# Look at that cool table! 
dx
