#' @name functions_process_control.R
#' @title Functions for Statistical Process Control
#' @description
#' Below are several functions that you can use or customize.

library(dplyr)    # for data wrangling
library(readr)    # for reading in data
library(ggplot2)  # for visualization
library(ggpubr)   # for combining plots
library(moments)  # for skewness and kurtosis
# functions_process_control.R
source("functions/functions_process_control.R")
dn()
bn(n = 10)
# dn(), bn()      -> d2/d3/D3/D4 and b2/b3/C4/A3/B3/B4 constants
# ggprocess()     -> process overview chart
# ggs()           -> standard deviation chart
# ggr()           -> range chart
# ggmr()          -> moving range chart
# ggxbar()        -> averages chart

# Exercise 2: pH as the quality metric ----------------------------------

water = read_csv("workshops/onsen.csv")
set_theme()

# Quick look at the raw data and summary stats
ggprocess(x = water$time, y = water$ph, xlab = "Subgroup", ylab = "pH")
describe(x = water$ph)

# Averages chart
ggxbar(x = water$time, y = water$ph, xlab = "Time (Subgroups)", ylab = "Average pH")

# Standard deviation chart
ggs(x = water$time, y = water$ph, xlab = "Time (Subgroups)", ylab = "SD of pH")

# Range chart
ggr(x = water$time, y = water$ph, xlab = "Time (Subgroups)", ylab = "Range of pH")
