# Load Packages -----------------------------------------------------------

library(tidyverse)
library(BayesFactor)
library(furrr)

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

tbl_design <- crossing(N, n, intercept, n_levels_1, n_levels_2, es_1, es_2, sigma)
tbl_design$n_expt <- 1:nrow(tbl_design)

# Simulate Data -----------------------------------------------------------

l_tbl_y <- simulate_y(tbl_design)

# check simulated data visually
suppressMessages(map(l_tbl_y, plot_results))

# Apply Models ------------------------------------------------------------

l_bfs <- map(l_tbl_y, compare_models_bf, agg = TRUE)

# Experiment --------------------------------------------------------------

n_expt <- 12
l_expt <- 1:n_expt %>% as.list()

# processing setup
n_cores_available <- future::availableCores()
n_cores_used <- n_cores_available - 2
plan(multisession, workers = n_cores_used)

# run workers
l_tbl_results <- future_map(
  l_expt, run_experiments, tbl_design = tbl_design,
  .progress = TRUE
  )

# analyze results
tbl_results <- l_tbl_results %>%
  reduce(rbind)
