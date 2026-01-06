
series_length   <- 100
trials          <- 500
quantile_levels <- seq(0.05, 0.95, 0.05)
n_points        <- 500
alphas          <- c(0.05, 0.01)
lag_test        <- 1


methods <- c("FQA", "FPC", "FACF", "SDO", "FSACF")
rejections <- array(0L,
                    dim = c(trials, length(alphas), length(methods)),
                    dimnames = list(NULL,
                                    paste0("alpha_", alphas),
                                    methods))


for (i in 1 : trials) {
  
  series <- simulate_iid_cauchy_fts(n_points = n_points,
                             series_length = series_length)
  grid <- attr(series, "grid")
  
  aux <- estimate_fqa_vector_barlett_reduced(series, quantile_levels, lag_test)
  whole_statistic <- series_length * sum(aux$fqa^2)
  
  cov_mat <- aux$covariance
  eig     <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
  lambda  <- pmax(eig, 0)
  lambda  <- lambda[lambda > 0]

  
  f_data <- t(series)
  
  for (j in seq_along(alphas)) {
    
    alpha <- alphas[j]
    
    # FQA
    
    crit_fqa <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    rejections[i, j, "FQA"] <- (whole_statistic > crit_fqa)
    
    # FPC
    
    ind_res <- independence_test(f_data = f_data,
                                 components = 3,
                                 lag = lag_test,
                                 alpha = alpha)
     rejections[i, j, "FPC"] <- (ind_res$p_value < alpha)
    
    # FACF
    
    facf_res <- fACF_test(f_data = f_data,
                          H = lag_test,
                          iid = TRUE,      # strong white-noise null
                          M = NULL,
                          alpha = alpha)
    rejections[i, j, "FACF"] <- (facf_res$p_value < alpha)
    
    # SDO
    
    sdo_res <- spectral_test(f_data,
                             kernel    = "Parzen",
                             bandwidth = "adaptive",
                             alpha     = alpha)
    rejections[i, j, "SDO"] <- (sdo_res$p_value < alpha)
    
    # FSACF
    
    fsacf_res <- fSACF_test(f_data = f_data,
                            H       = lag_test,
                            alpha   = alpha,
                            pplot   = FALSE,
                            suppress_raw_output   = FALSE,
                            suppress_print_output = FALSE)
    rejections[i, j, "FSACF"] <- (fsacf_res$p_value < alpha)
    
  }
  
  cat("Replication", i, "done\n")
  
}

apply(rejections, c(2, 3), mean)
