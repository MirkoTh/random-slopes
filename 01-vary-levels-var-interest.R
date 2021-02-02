# Load Packages -----------------------------------------------------------

library(tidyverse)
library(BayesFactor)
library(furrr)
library(ggrepel)

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
sigma <- seq(.2, 1, by = .2)

tbl_design <- crossing(N, n, intercept, n_levels_1, n_levels_2, es_1, es_2, sigma)
tbl_design$n_expt <- 1:nrow(tbl_design)



#### Single Run Experiment ####


# Simulate Data -----------------------------------------------------------

l_simulation <- simulate_y(tbl_design)
l_tbl_y <- l_simulation[["y"]]

# check simulated data visually
suppressMessages(map(l_tbl_y, plot_results, tbl_info = tbl_design))

# Apply Models ------------------------------------------------------------

l_m <- map(l_tbl_y, compare_models_bf, agg = TRUE)
l_bfs <- map(l_m, 1)



#### Iterate Over Several Experiments ####

n_expt <- 10
l_expt <- 1:n_expt %>% as.list()

# processing setup
n_cores_available <- future::availableCores()
n_cores_used <- n_cores_available - 2
plan(multisession, workers = n_cores_used)

# run workers
suppressMessages(
  ls <- future_map(
    l_expt, run_experiments, tbl_design = tbl_design,
    .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
)

l_tbl_results <- map(ls, 1)
l_tbl_b1 <- map(ls, 2)

tbl_results <- l_tbl_results %>%
  reduce(rbind)
tbl_results$b1_sample_avg <- l_tbl_b1 %>% reduce(c)

td <- lubridate::today()
save(tbl_results, file = str_c(td, "-results-expt-sigma", length(sigma), "l.Rda"))


# Analyze Experiments -----------------------------------------------------

load("2021-02-02-results-expt-sigma5l.Rda")

# BFs of FAs against Nr. Levels X1
bfs_larger_equal_3(tbl_results)

# log10(BF) against Sample Effect Size
bfs_against_sample_es(tbl_results)

# measurement error & effect x1-1
measure_error_vs_x1(tbl_results)
