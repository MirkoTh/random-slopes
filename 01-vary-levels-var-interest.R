# Load Packages -----------------------------------------------------------

library(tidyverse)
library(BayesFactor)

# home-grown
source("utils.R")

# Parameters --------------------------------------------------------------

N <- 20
n <- 40
n_levels_1 <- c(2, 5, 10)
n_levels_2 <- 2
intercept <- 0
es_1 <- 0
es_2 <- .5
sd <- 1
sigma <- c(.1, .25)

df_design <- crossing(N, n, intercept, n_levels_1, n_levels_2, es_1, es_2, sigma)
df_design$n_expt <- 1:nrow(df_design)

# Simulate Data -----------------------------------------------------------

l_tbl_y <- simulate_y(df_design)

# check simulated data visually
suppressMessages(map(l_tbl_y, plot_results))

# Apply Models ------------------------------------------------------------

l_bfs <- map(l_tbl_y, compare_models_bf, agg = TRUE)

