# Step 1: load the course helper functions (ggprocess, ggxbar, ggs, ggr, etc.)
source("functions/functions_process_control.R")
set_theme()

# Step 2: load the onsen dataset
water = read_csv("workshops/onsen.csv")

# Step 3: pick a variable of interest -- pH, sampled in 8 subgroups of 20
# readings each (grouped by `time`). We use `time` as the subgroup index
# and `ph` as the metric so each chart reflects within-subgroup variation.
ggprocess(x = water$time, y = water$ph, xlab = "Time (Subgroup)", ylab = "pH")

# Step 4: averages (x-bar) chart -- tracks the subgroup means against
# the grand mean and its +/-3 sigma control limits
p_avg = ggxbar(x = water$time, y = water$ph, xlab = "Time (Subgroup)", ylab = "Average pH")
p_avg

# Step 5: standard deviation (s) chart -- tracks within-subgroup spread
p_sd = ggs(x = water$time, y = water$ph, xlab = "Time (Subgroup)", ylab = "Standard Deviation of pH")
p_sd

# Step 6: range (r) chart -- tracks within-subgroup range (max - min)
p_r = ggr(x = water$time, y = water$ph, xlab = "Time (Subgroup)", ylab = "Range of pH")
p_r
