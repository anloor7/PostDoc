
series_length   <- 200
trials          <- 2000
quantile_levels <- seq(0.05, 0.95, 0.05)
n_points        <- 500
alpha           <- 0.05
lag_test        <- 1

sum_abs_values        <- c(0.2, 0.4, 0.6, 0.8, 1) # Varying serial dependence strength

methods <- c("FQA", "FPC", "FACF", "SDO", "FSACF")

power_results <- array(NA_real_,
                       dim = c(length(sum_abs_values), length(methods)),
                       dimnames = list(paste0("sum_abs_val=", sum_abs_values), methods))



for (k in seq_along(sum_abs_values)) {
  
  sum_abs_val <- sum_abs_values[k]
  cat("Computing power for sum_abs_val =", sum_abs_val, "\n")
   
  rejections <- array(0L, dim = c(trials, length(methods)),
                      dimnames = list(NULL, methods))
  
  for (i in 1:trials) {
    
    auxiliary <- generate_c1_c2(sum_abs_val)
    c1 <- auxiliary[1]
    c2 <- auxiliary[2]
    phi_1_mat <- make_phi_mat_gaussian(n_points, c1)
    phi_2_mat <- make_phi_mat_gaussian(n_points, c2)
    
    series <- simulate_threshold_far1_bm(series_length = series_length, 
                                            n_points = n_points, 
                                            phi1_mat = phi_1_mat,
                                            phi2_mat = phi_2_mat,
                                            r_fun = function(x_vec) {mean(x_vec)}, s_thresh = 0)
    f_data <- t(series)
    
    # FQA
    
    aux <- estimate_fqa_vector_barlett_reduced(series, quantile_levels, lag_test)
    whole_statistic <- series_length * sum(aux$fqa^2)
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    crit_fqa <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    rejections[i, "FQA"] <- (whole_statistic > crit_fqa)
    
    # FPC
    
    ind_res <- independence_test(f_data = f_data, components = 3, lag = lag_test, alpha = alpha)
    rejections[i, "FPC"] <- (ind_res$p_value < alpha)

    # FACF
    
    facf_res <- fACF_test(f_data = f_data, H = lag_test, iid = TRUE, M = NULL, alpha = alpha)
    rejections[i, "FACF"] <- (facf_res$p_value < alpha) 

    # SDO
    
    sdo_res <- spectral_test(f_data, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
    rejections[i, "SDO"] <- (sdo_res$p_value < alpha) 

    # FSACF
    
    fsacf_res <- fSACF_test(f_data = f_data, H = lag_test, pplot = FALSE, alpha = alpha)
    rejections[i, "FSACF"] <- (fsacf_res$p_value < alpha) 
    
    if (i %% 200 == 0) cat("  Trial", i, "of", trials, "done\n")
    print(i)
    
  }
  
  power_results[k, ] <- apply(rejections, 2, mean)
  cat("sum_abs_val =", sum_abs_val, "Power (α=0.05):", round(power_results[k, ], 3), "\n\n")
  
}

print("Power results (T=200, α=0.05, varying sum_abs_val):")
print(round(power_results, 3))


