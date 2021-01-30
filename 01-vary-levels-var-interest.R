# Load Packages -----------------------------------------------------------

library(tidyverse)

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

# Simulate Data -----------------------------------------------------------

b1 <- df_design[, c("N", "es_1", "sigma")] %>% 
  rename(c("n" = "N", "mean" = "es_1", "sd" = "sigma")) %>%
  pmap(rnorm)
b2 <- df_design[, c("N", "es_2", "sigma")] %>%
  rename(c("n" = "N", "mean" = "es_2", "sd" = "sigma")) %>%
  pmap(rnorm)
mu <- df_design[, c("N", "intercept", "sd" = "sigma")] %>%
  rename(c("n" = "N", "mean" = "intercept", "sd" = "sigma")) %>%
  pmap(rnorm)
m_cond <- pmap(list(
  mu, b1, b2, df_design$n_levels_1, df_design$n_levels_2
  ), .f=condition_means)
y <- map(m_cond, rnorm, sd = sd)


# Apply Models ------------------------------------------------------------


