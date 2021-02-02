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

# Simulate Data -----------------------------------------------------------

l_simulation <- simulate_y(tbl_design)
l_tbl_y <- l_simulation[["y"]]

# check simulated data visually
suppressMessages(map(l_tbl_y, plot_results, tbl_info = tbl_design))

# Apply Models ------------------------------------------------------------

l_bfs <- map(l_tbl_y, compare_models_bf, agg = TRUE)

# Run Experiments ---------------------------------------------------------

n_expt <- 100
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

save(tbl_results, file = str_c("results-expt-sigma", length(sigma), "l.Rda"))


# Analyze Experiments -----------------------------------------------------

load("2021-02-01-results-expt-sigma5l.Rda")

# BFs of FAs against Nr. Levels X1
bfs_larger_equal_3(tbl_results)

# log10(BF) against Sample Effect Size
bfs_against_sample_es(tbl_results)


p_2 <- compare_models_bf2(l_tbl_y[[5]]) %>% as_tibble()
p_10 <- compare_models_bf2(l_tbl_y[[15]]) %>% as_tibble()
p_2 %>% 
  select("x1-1", "sig2") %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value") %>%
  ggplot(aes(Value)) +
  geom_density() +
  facet_wrap(~ Parameter) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(0, 10))

p_10 %>% 
  select("x1-1", "sig2") %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Value") %>%
  ggplot(aes(Value)) +
  geom_density() +
  facet_wrap(~ Parameter) +
  theme_bw() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(0, 10))



