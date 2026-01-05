
# This is one of the main functions to perform the omnibus test
# Input parameters:
# X: functional time series (each row is a curve)
# probs: vector of quantile levels (and thresholds)
# lag: lag l at which serial dependence is assessed

# Output:
# fqa: vector of FQA estimates (the omnibus statistic is computed as: series_length * sum(fqa^2)
# covariance: corresponding covariance matrix 


estimate_fqa_vector_barlett_reduced <- function(X, probs, lag) {
  
  series_length <- nrow(X)
  n_points      <- ncol(X)
  P             <- length(probs)
  
  ## 1. Estimate pointwise quantiles
  
  q_hat <- sapply(probs, function(tau) apply(X, 2, quantile, probs = tau))
  names(probs) <- NULL
  
  ## 2. Excursion set indicators: I{X_t(u) <= q_tau(u)} for each tau
  
  excursion_indicators <- lapply(1:P, function(i) {
    sweep(X, 2, q_hat[, i], FUN = "<=") * 1
  })
  
  ## 3. Excursion set proportions: approximate L(A_tau^t)
  
  excursion_props <- lapply(excursion_indicators, function(ind) rowMeans(ind))
  
  ## 4. All (tau_i, tau_j) combinations
  
  combs <- expand.grid(i = 1:P, j = 1:P)
  K     <- nrow(combs)
  
  ## 5. FQA estimates and intermediate vectors
  
  fqa_estimates <- numeric(K)
  Zt_list       <- vector("list", K)
  Ztl_list      <- vector("list", K)
  Zprod_list    <- vector("list", K)
  
  for (k in 1:K) {
    i <- combs$i[k]
    j <- combs$j[k]
    
    Zt  <- as.numeric(excursion_props[[i]] <= probs[i])
    Ztl <- as.numeric(excursion_props[[j]] <= probs[j])
    
    valid_idx  <- 1:(series_length - lag)
    Zt         <- Zt[valid_idx]
    Ztl_lagged <- Ztl[valid_idx + lag]
    
    p1  <- mean(Zt)
    p2  <- mean(Ztl_lagged)
    p12 <- mean(Zt * Ztl_lagged)
    
    denom <- sqrt(p1 * (1 - p1) * p2 * (1 - p2))
    fqa_estimates[k] <- if (denom > 0) (p12 - p1 * p2) / denom else 0
    
    Zt_list[[k]]    <- Zt
    Ztl_list[[k]]   <- Ztl_lagged
    Zprod_list[[k]] <- Zt * Ztl_lagged
  }
  
  ## 6. Stack W_t = (Zt, Ztl, Zt*Ztl_lagged) for all parameter combinations
  
  input_matrix <- do.call(
    cbind,
    mapply(function(Zt, Ztl, Zprod) cbind(Zt, Ztl, Zprod),
           Zt_list, Ztl_list, Zprod_list, SIMPLIFY = FALSE)
  )
  
  ## 7. Center
  centered <- scale(input_matrix, center = TRUE, scale = FALSE)
  
  ## 8. HAC with custom weights
  
  lmax <- 1L                      
  n    <- nrow(centered)
  d    <- ncol(centered)
  covsum <- matrix(0, d, d)
  
  for (h in -lmax:lmax) {
    
    if (h == 0) {
      w_h <- 1
    } else if (abs(h) == 1) {
      w_h <- 0.2
    } else {
      w_h <- 0
    }
    
    if (w_h <= 0) next
    
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
    cov_k <- t(W1c) %*% W2c / length(i1)
    
    covsum <- covsum + w_h * cov_k
  }
  
  Sigma <- covsum
  
  ## 9. Jacobian of g evaluated at empirical means
  
  grad_g_list <- vector("list", K)
  
  for (k in 1:K) {
    Zt    <- Zt_list[[k]]
    Ztl   <- Ztl_list[[k]]
    Zprod <- Zprod_list[[k]]
    
    p1  <- mean(Zt)
    p2  <- mean(Ztl)
    p12 <- mean(Zprod)
    gamma_hat <- p12 - p1 * p2
    D <- sqrt(p1 * (1 - p1) * p2 * (1 - p2))
    
    if (D > 0) {
      d1 <- (-p2 / D) - ((gamma_hat / (2 * D^3)) * (1 - 2 * p1) * p2 * (1 - p2))
      d2 <- (-p1 / D) - ((gamma_hat / (2 * D^3)) * (1 - 2 * p2) * p1 * (1 - p1))
      d3 <- 1 / D
      grad_g_list[[k]] <- c(d1, d2, d3)
    } else {
      grad_g_list[[k]] <- c(0, 0, 0)
    }
  }
  
  ## 10. Jacobian matrix ∇g: K × (3K)
  
  grad_g_matrix <- matrix(0, nrow = K, ncol = 3 * K)
  for (k in 1:K) {
    grad_g_matrix[k, ((k - 1) * 3 + 1):(k * 3)] <- grad_g_list[[k]]
  }
  
  ## 11. Covariance of FQA vector via delta method
  
  Sigma_fqa <- grad_g_matrix %*% Sigma %*% t(grad_g_matrix)
  
  return(list(fqa = fqa_estimates, covariance = Sigma_fqa))
  
}


# This is a function to obtain the critical value of the omnibus test
# Input parameters:
# lambda: vector of (nonnegative) eigenvalues of the covariance matrix obtained as output of the previous function
# prob: desired CDF level
# B : number of Monte Carlo replications

# Output:
# Approximated critical value


simulate_q_linear_comb_chisq <- function(lambda, prob, B = 10000) {
  
  # Enforce nonnegative weights and drop zeros
  
  lambda <- pmax(lambda, 0)
  lambda <- lambda[lambda > 0]
  if (length(lambda) == 0) return(0)
  
  sims <- replicate(B, {
    z <- rnorm(length(lambda))
    sum(lambda * z^2)
  })
  
  return(quantile(sims, prob, names = FALSE))
  
}
