
library(wwntests) # independence_test
library(FTSgof)       # fACF_test, fSACF_test
# library(... )       # package providing spectral_test(), if not already loaded

series_length   <- 200
trials          <- 500
quantile_levels <- seq(0.05, 0.95, 0.05)
n_points        <- 500
alphas          <- c(0.05, 0.01)
lag_test        <- 1

## ---- Rejection indicators: rows = trials, cols = alpha, 5 methods ----

methods <- c("Proposed", "FPC", "FACF", "SDO", "FSACF")
rejections <- array(0L,
                    dim = c(trials, length(alphas), length(methods)),
                    dimnames = list(NULL,
                                    paste0("alpha_", alphas),
                                    methods))

## ---- main loop: size comparison, Scenario 1 (i.i.d. functional white noise) ----

for (i in 1:trials) {
  
  ## generate functional time series under Scenario 1 (null)
  series <- simulate_quadratic_iid_fts(n_points = n_points,
                             series_length = series_length)
  grid <- attr(series, "grid")
  
  ## Proposed FQA-based omnibus test at lag 1
  # aux <- estimate_fqa_vector_barlett_reduced(series, quantile_levels, lag_test)
  # whole_statistic <- series_length * sum(aux$fqa^2)
  
  # cov_mat <- aux$covariance
  # eig     <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
  # lambda  <- pmax(eig, 0)
  # lambda  <- lambda[lambda > 0]
  
  ## competitors: data as J x N (rows = grid, cols = time)
  f_data <- t(series)
  
  for (j in seq_along(alphas)) {
    
    alpha <- alphas[j]
    
    ## Proposed FQA test
    #  crit_fqa <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    #  rejections[i, j, "Proposed"] <- (whole_statistic > crit_fqa)
    
    ## FPC-based independence test (Gabrys & Kokoszka), 3 FPCs
    # ind_res <- independence_test(f_data = f_data,
    #                             components = 3,
    #                             lag = lag_test,
    #                             alpha = alpha)
    # rejections[i, j, "FPC"] <- (ind_res$p_value < alpha)
    
    ## FACF test (autocovariance-based)
    # facf_res <- fACF_test(f_data = f_data,
    #                      H = lag_test,
    #                      iid = TRUE,      # strong white-noise null
    #                      M = NULL,
    #                      alpha = alpha)
    # rejections[i, j, "FACF"] <- (facf_res$p_value < alpha)
    
    ## SDO test via spectral_test()
    # sdo_res <- spectral_test(f_data,
    #                         kernel    = "Parzen",
    #                         bandwidth = "adaptive",
    #                         alpha     = alpha)
    #rejections[i, j, "SDO"] <- (sdo_res$p_value < alpha)
    
    ## FSACF test (functional spherical autocorrelation)
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

       ## ---- empirical rejection rates (Scenario 1: size) ----

apply(rejections, c(2, 3), mean)
