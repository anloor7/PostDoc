
# This is the main function to perform the omnibus test
# Input parameters:
# X: circular time series of angles in [0, 2 * pi)
# probs: vector of quantile levels
# radii: vector of radii
# lag: lag l at which serial dependence is assessed

# Output:
# cqa: vector of CQA estimates
# covariance: corresponding covariance matrix 
# SigmaW: covariance matrix of the process W_t
# qstat: statistic Q_T(l)
# combs: data frame with the considered combinations (i, j, k) for (\tau_i, \tau_j, r_k)
# centers: circular quantiles corresponding to probs
# p1: vector of probabilities p_{\tau_i, r_k}
# p2: vector of probabilities p_{\tau_j, r_k}
# p12: vector of probabilities p_{\tau_i, \tau_j, r_k}(l)


estimate_cqa_structure <- function(X, probs, radii, lag = 1) {
  
  series_length <- length(X)
  P <- length(probs) * 2
  R <- length(radii)
  
  # Arc centers from circular quantiles (two antipodal points per prob)
  
  centers <- c(sapply(probs, circular_quantile_lsv, cts = X)[1:2, ])
  
  # All combinations (center_i, center_j, radius_k)
  
  combs <- expand.grid(i = 1:P, j = 1:P, k = 1:R)
  D     <- nrow(combs)     # D = P^2 * R
  
  
  # Storage
  
  cqa_estimates <- numeric(D)
  Zt_list       <- vector("list", D)
  Ztl_list      <- vector("list", D)
  Zprod_list    <- vector("list", D)
  
  p1_vec  <- numeric(D)
  p2_vec  <- numeric(D)
  p12_vec <- numeric(D)
  
  # 1) Build CQA components + W_t components (aligned length = T-lag)
  
  for (dd in 1:D) {
    
    i <- combs$i[dd]
    j <- combs$j[dd]
    k <- combs$k[dd]
    
    ci <- centers[i]
    cj <- centers[j]
    rk <- radii[k]
    
    # Aligned indicators for pairs (t, t+lag)
    
    A_i <- in_arc(X[1:(series_length - lag)], center = ci, radius = rk)
    A_j <- in_arc(X[(1 + lag):series_length], center = cj, radius = rk)
    
    Zt         <- as.numeric(A_i)          # Length T-lag
    Ztl_lagged <- as.numeric(A_j)          # Length T-lag
    Zprod      <- Zt * Ztl_lagged          # Length T-lag
    
    # Empirical probabilities computed on the same aligned sample
    
    p1  <- mean(Zt)
    p2  <- mean(Ztl_lagged)
    p12 <- mean(Zprod)
    
    p1_vec[dd]  <- p1
    p2_vec[dd]  <- p2
    p12_vec[dd] <- p12
    
    denom <- sqrt(p1 * (1 - p1) * p2 * (1 - p2))
    cqa_estimates[dd] <- if (denom > 0) (p12 - p1 * p2)/denom else 0
    
    Zt_list[[dd]]    <- Zt
    Ztl_list[[dd]]   <- Ztl_lagged
    Zprod_list[[dd]] <- Zprod
    
  }
  
  # 2) Stack W_t = (Zt, Ztl, Zt*Ztl) for all parameter combinations
  
  input_matrix <- do.call(
    cbind,
    mapply(function(Zt, Ztl, Zprod) cbind(Zt, Ztl, Zprod),
           Zt_list, Ztl_list, Zprod_list, SIMPLIFY = FALSE)
  )
  
  # 3) Center
  
  centered <- scale(input_matrix, center = TRUE, scale = FALSE)
  
  # The matrix centered has n = T-lag rows (W_t index runs over aligned pairs)
  
  n <- nrow(centered)
  d <- ncol(centered)
  
  # 4) HAC estimate using 0 and ±lag (weight 1 at 0 and weight 0.5 at ±lag)
  
  covsum <- matrix(0, d, d)
  
  for (h in c(-lag, 0L, lag)) {
    
    if (h == 0) {
      
      w_h <- 1
      
    } else {
      
      w_h <- 0.50
      
    }
    
    # Valid indices
    
    if (h < 0) {
      
      i1 <- 1:(n + h)
      i2 <- (1 - h):n
      
    } else if (h > 0) {
      
      i1 <- (1 + h):n
      i2 <- 1:(n - h)
      
    } else {
      
      i1 <- 1:n
      i2 <- 1:n
      
    }
    
    W1c <- centered[i1, , drop = FALSE]
    W2c <- centered[i2, , drop = FALSE]
    
    cov_h <- t(W1c) %*% W2c/length(i1)
    
    covsum <- covsum + w_h * cov_h
    
  }
  
  SigmaW <- covsum
  
  # 5) Jacobian of g for each CQA component
  
  grad_g_list <- vector("list", D)
  
  for (kk in 1:D) {
    
    p1  <- p1_vec[kk]
    p2  <- p2_vec[kk]
    p12 <- p12_vec[kk]
    
    gamma_hat <- p12 - p1 * p2
    Dden <- sqrt(p1 * (1 - p1) * p2 * (1 - p2))
    
    if (Dden > 0) {
      
      d1 <- (-p2 / Dden) - ((gamma_hat / (2 * Dden^3)) * (1 - 2 * p1) * p2 * (1 - p2))
      d2 <- (-p1 / Dden) - ((gamma_hat / (2 * Dden^3)) * (1 - 2 * p2) * p1 * (1 - p1))
      d3 <- 1 / Dden
      grad_g_list[[kk]] <- c(d1, d2, d3)
      
    } else {
      
      grad_g_list[[kk]] <- c(0, 0, 0)
      
    }
  }
  
  grad_g_matrix <- matrix(0, nrow = D, ncol = 3 * D)
  
  for (kk in 1:D) {
    
    grad_g_matrix[kk, ((kk - 1) * 3 + 1):(kk * 3)] <- grad_g_list[[kk]]
    
  }
  
  # 6) Delta method covariance for the CQA vector
  
  Sigma_cqa <- grad_g_matrix %*% SigmaW %*% t(grad_g_matrix)
  
  # 7) Omnibus max-type statistic
  
  abs_vector <- abs(cqa_estimates)
  MT_matrix <- matrix(0, nrow = P, ncol = P)
  
  for (ii in 1:P) {
    
    for (jj in 1:P) {
      
      idx_ij <- which(combs$i == ii & combs$j == jj)
      MT_matrix[ii, jj] <- max(abs_vector[idx_ij])
      
    }
    
  }
  
  qstat <- sum(MT_matrix)
  
  
  return(list(
    cqa       = cqa_estimates,
    covariance = Sigma_cqa,
    SigmaW    = SigmaW,
    qstat     = qstat,
    combs     = combs,
    centers = centers,
    p1        = p1_vec,
    p2        = p2_vec,
    p12       = p12_vec
  ))
  
}


# This is a function to obtain the critical value of the omnibus test
# Input parameters:
# Sigma_cqa: output covariance of the above function
# combs: output combs of the above function
# n_sim: number of Monte Carlo replications
# alpha: significance level

# Output:
# crit: approximated critical value


cqa_critical <- function(Sigma_cqa, combs, n_sim = 10000, alpha = 0.05) {
  
  D <- nrow(Sigma_cqa)
  P <- max(combs$i)
  R <- max(combs$k)
  
  ## Index sets for each (i,j)
  
  idx_by_ij <- vector("list", length = P * P)
  
  for (i in 1:P) {
    
    for (j in 1:P) {
      
      m <- (i - 1) * P + j
      idx_by_ij[[m]] <- which(combs$i == i & combs$j == j)
      
    }
    
  }
  
  Gmat <- mvtnorm::rmvnorm(n_sim, mean = rep(0, D), sigma = Sigma_cqa)
  absGmat <- abs(Gmat)   # same dimensions
  
  # Compute Q_T for each simulation
  
  QT_sim <- numeric(n_sim)
  
  for (b in 1:n_sim) {
    
    g_b <- absGmat[b, ]     
    MT  <- matrix(0, nrow = P, ncol = P)
    
    for (i in 1:P) {
      
      for (j in 1:P) {
        
        m <- (i - 1) * P + j
        idx_ij <- idx_by_ij[[m]]
        MT[i, j] <- max(g_b[idx_ij])
        
      }
      
    }
    
    QT_sim[b] <- sum(MT)
  }
  
  crit <- as.numeric(stats::quantile(QT_sim, probs = 1 - alpha))
  
  return(crit)
  
}