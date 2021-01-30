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
