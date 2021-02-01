condition_means <- function(mu, b1, b2, n_levels_1, n_levels_2){
  N <- length(mu)
  C1 <- scale(seq(1, n_levels_1), center = TRUE, scale = TRUE) %>% 
    as.matrix() %>% t()
  C2 <- scale(seq(1, n_levels_2), center = TRUE, scale = TRUE) %>%
    as.matrix() %>% t()
  efs_1 <- array(rep(b1 %*% C1, n_levels_2), dim = c(N, n_levels_1, n_levels_2))
  efs_2 <- array(rep(b2 %*% C2, n_levels_1), dim = c(N, n_levels_2, n_levels_1))
  efs_2 <- aperm(efs_2, c(1, 3, 2))
  return(mu + efs_1 + efs_2)
}


cond_means_tbl <- function(x){
  dims <- dim(x)
  lvl_1 <- seq(1, dims[2])
  lvl_2 <- seq(1, dims[3])
  nm_cols <- merge(lvl_1, lvl_2, all = TRUE)
  m <- x %>% 
    matrix(dims[1], dims[2]*dims[3])
  colnames(m) <- str_c(nm_cols$x, "_", nm_cols$y)
  return(as_tibble(m))
}


sample_y <- function(ms, n_per_cond, n_expt){
  i <- 1:nrow(ms)
  l_ms <- split(ms, seq(nrow(ms)))
  l_tbl_i <- map(l_ms, .f = sample_entity, n = n_per_cond)
  N <- length(l_tbl_i)
  n_trials_tot <- nrow(l_tbl_i[[1]])
  n_cond <- n_trials_tot / n_per_cond
  l_tbl_i %>% 
    reduce(rbind) %>%
    mutate(
      n_expt = n_expt,
      i = rep(i, each = n_trials_tot),
      trial = rep(1:n_trials_tot, N),
      x1 = str_extract(Condition, "^(.+)_"),
      x1 = str_replace(x1, "_", ""),
      x2 = str_extract(Condition, "_(.+)$"),
      x2 = str_replace(x2, "_", "")
    ) %>%
    select(n_expt, i, trial, x1, x2, y)
}


sample_entity <- function(ms, n){
  tbl <- as_tibble(map(ms, rnorm, n = n)) %>%
    pivot_longer(
      cols = names(ms), names_to = "Condition", values_to = "y"
    )
  return(tbl)
}


plot_results <- function(x){
  n_expt <- x$n_expt[1]
  x_agg_i <- aggregate_i(x)
  x_agg_cond <- aggregate_cond(x_agg_i)
  ggplot(x_agg_i, aes(x1, y_mn_i)) +
    geom_line(aes(group = i, color = i)) +
    geom_line(
      data=x_agg_cond, aes(x1, y_mn_cond, group = 1), color = "black", size = 1
    ) +
    geom_point(
      data=x_agg_cond, aes(x1, y_mn_cond, group = 1), color = "white", size = 4
    ) +
    geom_point(
      data=x_agg_cond, aes(x1, y_mn_cond, group = 1), color = "black", size = 2,
      shape = 1
    ) +
    scale_color_viridis_c(guide = FALSE) +
    facet_wrap(~ x2) +
    theme_bw() +
    labs(
      title = str_c("Experiment ", n_expt),
      x = "X1",
      y = "Y"
    )
}


simulate_y <- function(tbl_design){
  b1 <- tbl_design[, c("N", "es_1", "sigma")] %>% 
    rename(c("n" = "N", "mean" = "es_1", "sd" = "sigma")) %>%
    pmap(rnorm)
  b2 <- tbl_design[, c("N", "es_2", "sigma")] %>%
    rename(c("n" = "N", "mean" = "es_2", "sd" = "sigma")) %>%
    pmap(rnorm)
  mu <- tbl_design[, c("N", "intercept", "sd" = "sigma")] %>%
    rename(c("n" = "N", "mean" = "intercept", "sd" = "sigma")) %>%
    pmap(rnorm)
  m_cond <- pmap(list(
    mu, b1, b2, tbl_design$n_levels_1, tbl_design$n_levels_2
  ), .f=condition_means)
  l_tbl_m_cond <- map(m_cond, cond_means_tbl)
  y <- pmap(
    list(
      l_tbl_m_cond, n_per_cond = tbl_design$n, n_expt = tbl_design$n_expt
    ), .f = sample_y
  )
  list_out <- list(
    b1 = b1,
    b2 = b2,
    mu = mu,
    l_tbl_m_cond = l_tbl_m_cond,
    y = y
  )
  return(list_out)
}


aggregate_i <- function(x, f = FALSE){
  out <- x %>%
    group_by(i, x1, x2) %>%
    summarize(
      n_trials = length(y),
      y_mn_i = mean(y),
      y_sem_i = sd(y)/sqrt(n_trials)
    ) %>%
    ungroup()
  if (f == TRUE){
    out$i <- as.factor(out$i)
  }
  return(out)
}


aggregate_cond <- function(x_agg_i){
  x_agg_i %>%
    group_by(x1, x2) %>%
    summarize(
      N = length(unique(i)),
      y_mn_cond = mean(y_mn_i),
      y_sem_cond = sd(y_mn_i)/sqrt(N)
    ) %>%
    ungroup() %>%
    mutate(i = max(x_agg_i$i) + 1)
}


compare_models_bf <- function(tbl, agg){
  if (agg){
    tbl <- aggregate_i(tbl, f = TRUE)
    names(tbl) <- c("i", "x1", "x2", "n_trials", "y", "y_sem")
  } else {
    tbl <- tbl %>% select(i, x1, x2, y)
    tbl$i <- as.factor(tbl$i)
  }
  m_bf_eff <- lmBF(
    formula = y ~ x1 + x2 + i + i:x2, 
    data = tbl %>% as.data.frame(),
    whichRandom = "i"
  )
  m_bf_no_eff <- lmBF(
    formula = y ~ x2 + i + i:x2, 
    data = tbl %>% as.data.frame(),
    whichRandom = "i"
  )
  m_comp <- m_bf_eff / m_bf_no_eff
  return (exp(m_comp@bayesFactor$bf))
}


run_experiments <- function(l, tbl_design){
  l_simulation <- simulate_y(tbl_design)
  l_tbl_y <- l_simulation[["y"]]
  l_bfs <- map(l_tbl_y, compare_models_bf, agg = TRUE)
  tbl_results <- tbl_design %>%
    select(-n_expt)
  tbl_results$bf <- as_vector(l_bfs)
  return(tbl_results)
}
