condition_means <- function(mu, b1, b2, n_levels_1, n_levels_2){
  N <- length(mu)
  # C1 <- scale(seq(1, n_levels_1), center = TRUE, scale = TRUE) %>%
  #   as.matrix() %>% t()
  # C2 <- scale(seq(1, n_levels_2), center = TRUE, scale = TRUE) %>%
  #   as.matrix() %>% t()
  
  C1 <- rep(c(-.5, .5), n_levels_1)[1:n_levels_1] %>%
    as.matrix() %>% t()
  C2 <- rep(c(-.5, .5), n_levels_2)[1:n_levels_2] %>%
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


plot_results <- function(x, tbl_info){
  n_expt_ <- x$n_expt[1]
  tbl_info <- tbl_info %>% filter(n_expt == n_expt_)
  x_agg_i <- aggregate_i(x)
  x_agg_cond <- aggregate_cond(x_agg_i)
  x_agg_i <- x_agg_i %>%
    mutate(x1 = as.numeric(as.character(x1)),
           x2 = as.numeric(as.character(x2)))
  x_agg_cond <- x_agg_cond %>%
    mutate(x1 = as.numeric(as.character(x1)),
           x2 = as.numeric(as.character(x2)))
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
      title = str_c("Experiment ", n_expt_),
      x = "X1",
      y = "Y",
      caption = str_c(
        "B1 = ", tbl_info$es_1, ", B2 = ", tbl_info$es_2,
        ", sigma = ", tbl_info$sigma
      )
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
  bf <- exp(m_comp@bayesFactor$bf)
  p <- posterior(m_bf_eff, iterations = 1000)
  l_out <- list(bf = bf, p = p)
  return (l_out)
}

posterior_samples <- function(tbl){
  tbl <- aggregate_i(tbl, f = TRUE)
  names(tbl) <- c("i", "x1", "x2", "n_trials", "y", "y_sem")
  m_bf_eff <- lmBF(
    formula = y ~ x1 + x2 + i + i:x2, 
    data = tbl %>% as.data.frame(),
    whichRandom = "i"
  )
  p <- posterior(m_bf_eff, iterations = 5000)
  return (p)
}


run_experiments <- function(l, tbl_design){
  l_simulation <- simulate_y(tbl_design)
  l_tbl_y <- l_simulation[["y"]]
  tbl_b1 <- map(l_simulation[["b1"]], mean) %>% as_vector()
  l_m <- map(l_tbl_y, compare_models_bf, agg = TRUE)
  l_bfs <- map(l_m, 1)
  l_post <- map(l_m, 2)
  tbl_results <- tbl_design %>%
    select(-n_expt)
  tbl_results$bf <- as_vector(l_bfs)
  tbl_post <- map(
    l_post, function(x) {
      as_tibble(x) %>% 
      select("x1-1", "sig2") %>% 
      summarize(
        x1_1_mn = mean(`x1-1`),
        x1_1_sd = sd(`x1-1`),
        sig2_mn = mean(sig2),
        sig2_sd = sd(sig2))
      }
    ) %>% reduce(rbind)
  tbl_results <- cbind(tbl_results, tbl_post)
  list_out <- list(
    tbl_results = tbl_results,
    tbl_b1 = tbl_b1
    )
  return(list_out)
}


bfs_larger_equal_3 <- function(tbl_results){
  tbl_results %>%
    filter(bf >= 3) %>%
    group_by(n_levels_1, sigma) %>%
    count() %>%
    ggplot(aes(n_levels_1, n/n_expt, group = sigma)) +
    geom_line(aes(color = sigma)) +
    geom_point(size = 3, color = "white") +
    geom_point(aes(color = sigma)) +
    scale_color_viridis_c() +
    theme_bw() +
    labs(
      x = "Nr. Levels X1",
      y = "Proportion Bayes factors >= 3"
    )
}


bfs_against_sample_es <- function(tbl_results){
  ggplot(tbl_results, aes(b1_sample_avg, log10(bf))) +
    geom_point(aes(color = n_levels_1), shape = 1) +
    coord_cartesian(ylim = c(-3.5, 3.5)) +
    facet_grid(sigma ~ n_levels_1) +
    scale_color_viridis_c() +
    theme_bw() +
    theme(legend.title = element_blank()) +
    labs(
      x = "Sample Effect Size",
      y = "log10(BF)"
    )
}


measure_error_vs_x1 <- function(tbl_results){
  tbl_results %>%
    select(n_levels_1, sigma, sig2_mn, sig2_sd, x1_1_mn, x1_1_sd) %>%
    pivot_longer(
      cols = c(sig2_mn, sig2_sd, x1_1_mn, x1_1_sd), 
      names_to = "Parameter", values_to = "Value"
    ) %>%
    mutate(
      Parameter = factor(
        Parameter, labels = c(
          "Measurement Error", "Precision Measurement Error", 
          "Effect X1-1", "Precision Effect X1-1"
        )
      )
    ) %>%
    group_by(n_levels_1, sigma, Parameter) %>%
    summarize(mn = mean(Value)) %>%
    ggplot(aes(n_levels_1, mn, group = sigma)) +
    geom_line(aes(color = sigma)) +
    facet_wrap(~ Parameter, ncol = 2, scales = "free_y") +
    scale_color_viridis_c() +
    theme_bw() +
    theme(legend.title = element_blank()) +
    labs(
      x = "Nr. Levels X1",
      y = "Effect"
    )
  
}