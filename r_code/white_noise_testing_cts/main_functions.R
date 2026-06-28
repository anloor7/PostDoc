
# This is the main function to perform the omnibus test
# Input parameters:
# X: circular time series of angles in [0, 2 * pi)
# probs: vector of quantile levels
# radii: vector of radii
# lag: lag l at which serial dependence is assessed
# lambda: shrinkage parameter for estimating the covariance matrix (according to the method of Schäfer and Strimmer)

# Output:
# rho_hat: vector of CQA estimates
# rho_array: array of CQA estimates, with dimensions P (number of centers) x P (number of centers) x R (number of radii)
# qstat: statistic Q_T(l)
# Sigma_G: covariance matrix Sigma_G (see Theorems 3 and 6 in the paper)
# centers: circular quantiles corresponding to probs
# combs: data frame with the considered combinations (i, j, k) for (\tau_i, \tau_j, r_k)
# W: realization of the process W_t (see the proof of Theorem 3 in the paper)


estimate_cqa_structure <- function(X, probs, radii, lag = 1, lambda = 0) {
  
  series_length <- length(X)
  P <- length(probs) * 2
  R <- length(radii)
  
  # Arc centers from circular quantiles (two antipodal points per prob)
  
  centers <- c(sapply(probs, circular_quantile_lsv, cts = X)[1:2, ])
  
  # All combinations (center_i, center_j, radius_k)
  
  combs <- expand.grid(i = 1:P, j = 1:P, k = 1:R)
  D     <- nrow(combs)     # D = P^2 * R
  
  # Indicator array
  
  Z <- array(0, dim = c(series_length, P, R))
  
  for (i in 1:P) {

    for (k in 1:R) {

      Z[, i, k] <- as.numeric(in_arc(X, centers[i], radii[k]))

    }

  }
  
  # Vector of CQA estimates
  
  rho_hat <- numeric(D)
  
  # Matrix of empirical W_t vectors
  
  W <- matrix(0, nrow = series_length - lag, ncol = D)
  
  for (d in 1:D) {
    
    i <- combs$i[d]
    j <- combs$j[d]
    k <- combs$k[d]
    
    z_i <- Z[, i, k]
    z_j <- Z[, j, k]
    
    p_i_hat <- mean(z_i)
    p_j_hat <- mean(z_j)
    
    p_ij_hat <- mean(
      z_i[1:(series_length - lag)] *
        z_j[(lag + 1):series_length]
    )
    
    sigma_hat <- sqrt(
      p_i_hat * (1 - p_i_hat) *
        p_j_hat * (1 - p_j_hat)
    )
    
    rho_hat[d] <- (p_ij_hat - p_i_hat * p_j_hat)/sigma_hat
    
    W[, d] <-
      ((z_i[1:(series_length - lag)] - p_i_hat) *
         (z_j[(lag + 1):series_length] - p_j_hat))/sigma_hat
  }
  
  # Arrange rho_hat as a P x P x R array
  
  rho_array <- array(NA_real_, dim = c(P, P, R))
  
  for (d in 1:D) {

    rho_array[combs$i[d], combs$j[d], combs$k[d]] <- rho_hat[d]

  }
  
  # Omnibus statistic Q_T
  
  Q_T <- sum(apply(abs(rho_array), c(1, 2), max))
  
  # Covariance matrix Sigma_G
  # Theorem 3: Sigma_G = Cov(W_0)
  
  W[!is.finite(W)] <- 0
  Sigma_G_prev <- corpcor::cov.shrink(W, lambda = lambda, lambda.var = 0, verbose = F)
  Sigma_G <- unclass(Sigma_G_prev)
  Sigma_G <- as.matrix(Sigma_G)
  diag(Sigma_G) <- 1
  
  return(list(
    rho_hat   = rho_hat,
    rho_array = rho_array,
    qstat       = Q_T,
    Sigma_G   = Sigma_G,
    centers   = centers,
    combs     = combs,
    W         = W
  ))
  
}


# This is a function to obtain the critical value of the omnibus test
# Input parameters:
# Sigma_G: output covariance matrix of the above function
# combs: output combs of the above function
# alpha: significance level
# B: number of Monte Carlo replications

# Output:
# critical_value: approximated critical value


cqa_critical <- function(Sigma_G,
                         combs,
                         alpha = 0.05,
                         B = 10000) {
  
  D <- nrow(Sigma_G)
  Sigma_G[!is.finite(Sigma_G)] <- 0
  
  pair_ids <- unique(combs[, c("i", "j")])
  
  idx_by_pair <- vector("list", length = nrow(pair_ids))
  
  for (m in seq_len(nrow(pair_ids))) {
      
    idx_by_pair[[m]] <- which(
      combs$i == pair_ids$i[m] &
        combs$j == pair_ids$j[m]
    )
  }
  
  Gmat <- mvtnorm::rmvnorm(n = B, mean = rep(0, D), sigma = Sigma_G)
  
  absGmat <- abs(Gmat)
  
  Q_sim <- numeric(B)
  
  for (m in seq_along(idx_by_pair)) {
      
    idx <- idx_by_pair[[m]]
    Q_sim <- Q_sim + apply(absGmat[, idx, drop = FALSE], 1, max)
      
  }
  
  critical_value <- quantile(Q_sim, probs = 1 - alpha)
  
  return(critical_value)
    
}


# This is a function to obtain the  p-value of the omnibus test
# Input parameters:
# Sigma_G: output covariance matrix of the above function
# combs: output combs of the above function
# alpha: significance level
# B: number of Monte Carlo replications

# Output:
# critical_value: approximated critical value


cqa_critical_new <- function(Sigma_G,
                         combs,
                         alpha = 0.05,
                         B = 10000) {
  
  D <- nrow(Sigma_G)
  Sigma_G[!is.finite(Sigma_G)] <- 0
  
  pair_ids <- unique(combs[, c("i", "j")])
  
  idx_by_pair <- vector("list", length = nrow(pair_ids))
  
  for (m in seq_len(nrow(pair_ids))) {
      
    idx_by_pair[[m]] <- which(
      combs$i == pair_ids$i[m] &
        combs$j == pair_ids$j[m]
    )
  }
  
  Gmat <- mvtnorm::rmvnorm(n = B, mean = rep(0, D), sigma = Sigma_G)
  
  absGmat <- abs(Gmat)
  
  Q_sim <- numeric(B)
  
  for (m in seq_along(idx_by_pair)) {
      
    idx <- idx_by_pair[[m]]
    Q_sim <- Q_sim + apply(absGmat[, idx, drop = FALSE], 1, max)
      
  }
  
  critical_value <- quantile(Q_sim, probs = 1 - alpha)
  
  return(critical_value)
    
}
