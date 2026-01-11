# Set a suitable working directory
# Load the data
load('list_fts.RData')

# Analyses

quantile_levels <- seq(0.05, 0.95, 0.05)
alpha <- 0.05
lags_test <- 1 : 10
series_length <- 756 

# Series 1 (GOOGL)

fts_1 <- list_fts$fts_1
fdata_1 <- t(fts_1)

stats_fqa_1 <- numeric()
crit_fqa_1 <- numeric()
p_values_fqa_1 <- numeric() # Vector of p-values for FQA

for (i in 1 : 10) {
    
    aux <- estimate_fqa_vector_barlett_reduced(fts_1, quantile_levels, lags_test[i])
    whole_statistic <- series_length * sum(aux$fqa^2)
    stats_fqa_1[i] <- whole_statistic
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    
    crit_fqa_1[i] <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    p_values_fqa_1[i] <- simulate_p_value_linear_comb(whole_statistic, lambda)
    print(i)
    
}

p_values_fpc_1 <- numeric() # Vector of p-values for FPC

for (i in 1 : 10) {
    
    ind_res <- independence_test(f_data = fdata_1, components = 3, lag = lags_test[i], alpha = alpha)
    p_values_fpc_1[i] <- ind_res$p_value
    print(i)
    
}

p_values_facf_1 <- numeric() # Vector of p-values for FACF

for (i in 1 : 10) {
    
    facf_res <- fACF_test(f_data = fdata_1, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
    p_values_facf_1[i] <- facf_res$p_value
    print(i)
    
}

sdo_res <- spectral_test(fdata_1, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_1 <- rep(sdo_res$p_value, 10) # Vector of p-values for SDO

p_values_fsacf_1 <- numeric() # Vector of p-values for FSACF

for (i in 1 : 10) {
    
    fsacf_res <- fSACF_test(f_data = fdata_1, H = lags_test[i], pplot = T, alpha = alpha)
    p_values_fsacf_1[i] <- fsacf_res$p_value
    print(i)
    
}


# Series 2 (T)

fts_2 <- list_fts$fts_2
fdata_2 <- t(fts_2)

stats_fqa_2 <- numeric()
crit_fqa_2 <- numeric()
p_values_fqa_2 <- numeric() # Vector of p-values for FQA

for (i in 1 : 10) {
    
    aux <- estimate_fqa_vector_barlett_reduced(fts_2, quantile_levels, lags_test[i])
    whole_statistic <- series_length * sum(aux$fqa^2)
    stats_fqa_2[i] <- whole_statistic
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    
    crit_fqa_2[i] <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    p_values_fqa_2[i] <- simulate_p_value_linear_comb(whole_statistic, lambda)
    print(i)
    
}

p_values_fpc_2 <- numeric() # Vector of p-values for FPC 

for (i in 1 : 10) {
    
    ind_res <- independence_test(f_data = fdata_2, components = 3, lag = lags_test[i], alpha = alpha)
    p_values_fpc_2[i] <- ind_res$p_value
    print(i)
    
}

p_values_facf_2 <- numeric() # Vector of p-values for FACF

for (i in 1 : 10) {
    
    facf_res <- fACF_test(f_data = fdata_2, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
    p_values_facf_2[i] <- facf_res$p_value
    print(i)
    
}

sdo_res <- spectral_test(fdata_2, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_2 <- rep(sdo_res$p_value, 10) # Vector of p-values for SDO

p_values_fsacf_2 <- numeric() # Vector of p-values for FSACF

for (i in 1 : 10) {
    
    fsacf_res <- fSACF_test(f_data = fdata_2, H = lags_test[i], pplot = T, alpha = alpha)
    p_values_fsacf_2[i] <- fsacf_res$p_value
    print(i)
    
}


# Series 3 (CSCO)

fts_3 <- list_fts$fts_3
fdata_3 <- t(fts_3)

stats_fqa_3 <- numeric()
crit_fqa_3 <- numeric()
p_values_fqa_3 <- numeric() # Vector of p-values for FQA

for (i in 1 : 10) {
    
    aux <- estimate_fqa_vector_barlett_reduced(fts_3, quantile_levels, lags_test[i])
    whole_statistic <- series_length * sum(aux$fqa^2)
    stats_fqa_3[i] <- whole_statistic
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    
    crit_fqa_3[i] <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    p_values_fqa_3[i] <- simulate_p_value_linear_comb(whole_statistic, lambda)
    print(i)
    
}

p_values_fpc_3 <- numeric() # Vector of p-values for FPC

for (i in 1 : 10) {
    
    ind_res <- independence_test(f_data = fdata_3, components = 3, lag = lags_test[i], alpha = alpha)
    p_values_fpc_3[i] <- ind_res$p_value
    print(i)
    
}

p_values_facf_3 <- numeric() # Vector of p-values for FACF

for (i in 1 : 10) {
    
    facf_res <- fACF_test(f_data = fdata_3, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
    p_values_facf_3[i] <- facf_res$p_value
    print(i)
    
}

sdo_res <- spectral_test(fdata_3, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_3 <- rep(sdo_res$p_value, 10) # Vector of p-values for SDO

p_values_fsacf_3 <- numeric() # Vector of p-values for FSACF

for (i in 1 : 10) {
    
    fsacf_res <- fSACF_test(f_data = fdata_3, H = lags_test[i], pplot = T, alpha = alpha)
    p_values_fsacf_3[i] <- fsacf_res$p_value
    print(i)
    
}


# Series 4 (CMCSA)

fts_4 <- list_fts$fts_4
fdata_4 <- t(fts_4)

stats_fqa_4 <- numeric()
crit_fqa_4 <- numeric()
p_values_fqa_4 <- numeric() # Vector of p-values for FQA

for (i in 1 : 10) {
    
    aux <- estimate_fqa_vector_barlett_reduced(fts_4, quantile_levels, lags_test[i])
    whole_statistic <- series_length * sum(aux$fqa^2)
    stats_fqa_4[i] <- whole_statistic
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    
    crit_fqa_4[i] <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    p_values_fqa_4[i] <- simulate_p_value_linear_comb(whole_statistic, lambda)
    print(i)
    
}

p_values_fpc_4 <- numeric() # Vector of p-values for FPC

for (i in 1 : 10) {
    
    ind_res <- independence_test(f_data = fdata_4, components = 3, lag = lags_test[i], alpha = alpha)
    p_values_fpc_4[i] <- ind_res$p_value
    print(i)
    
}

p_values_facf_4 <- numeric() # Vector of p-values for FACF

for (i in 1 : 10) {
    
    facf_res <- fACF_test(f_data = fdata_4, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
    p_values_facf_4[i] <- facf_res$p_value
    print(i)
    
}

sdo_res <- spectral_test(fdata_4, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_4 <- rep(sdo_res$p_value, 10) # Vector of p-values for SDO

p_values_fsacf_4 <- numeric() # Vector of p-values for FSACF

for (i in 1 : 10) {
    
    fsacf_res <- fSACF_test(f_data = fdata_4, H = lags_test[i], pplot = T, alpha = alpha)
    p_values_fsacf_4[i] <- fsacf_res$p_value
    print(i)
    
}


# Series 5 (EA)

fts_5 <- list_fts$fts_5
fdata_5 <- t(fts_5)

stats_fqa_5 <- numeric()
crit_fqa_5 <- numeric()
p_values_fqa_5 <- numeric() # Vector of p-values for FQA

for (i in 1 : 10) {
    
    aux <- estimate_fqa_vector_barlett_reduced(fts_5, quantile_levels, lags_test[i])
    whole_statistic <- series_length * sum(aux$fqa^2)
    stats_fqa_5[i] <- whole_statistic
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    
    crit_fqa_5[i] <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    p_values_fqa_5[i] <- simulate_p_value_linear_comb(whole_statistic, lambda)
    print(i)
    
}

p_values_fpc_5 <- numeric() # Vector of p-values for FPC

for (i in 1 : 10) {
    
    ind_res <- independence_test(f_data = fdata_5, components = 3, lag = lags_test[i], alpha = alpha)
    p_values_fpc_5[i] <- ind_res$p_value
    print(i)
    
}

p_values_facf_5 <- numeric() # Vector of p-values for FACF

for (i in 1 : 10) {
    
    facf_res <- fACF_test(f_data = fdata_5, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
    p_values_facf_5[i] <- facf_res$p_value
    print(i)
    
}

sdo_res <- spectral_test(fdata_5, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_5 <- rep(sdo_res$p_value, 10) # Vector of p-values for SDO

p_values_fsacf_5 <- numeric() # Vector of p-values for FSACF

for (i in 1 : 10) {
    
    fsacf_res <- fSACF_test(f_data = fdata_5, H = lags_test[i], pplot = T, alpha = alpha)
    p_values_fsacf_5[i] <- fsacf_res$p_value
    print(i)
    
}


# Series 6 (META)

fts_6 <- list_fts$fts_6
fdata_6 <- t(fts_6)

stats_fqa_6 <- numeric()
crit_fqa_6 <- numeric()
p_values_fqa_6 <- numeric() # Vector of p-values for FQA

for (i in 1 : 10) {
    
    aux <- estimate_fqa_vector_barlett_reduced(fts_6, quantile_levels, lags_test[i])
    whole_statistic <- series_length * sum(aux$fqa^2)
    stats_fqa_6[i] <- whole_statistic
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    
    crit_fqa_6[i] <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    p_values_fqa_6[i] <- simulate_p_value_linear_comb(whole_statistic, lambda)
    print(i)
    
}

p_values_fpc_6 <- numeric() # Vector of p-values for FPC

for (i in 1 : 10) {
    
    ind_res <- independence_test(f_data = fdata_6, components = 3, lag = lags_test[i], alpha = alpha)
    p_values_fpc_6[i] <- ind_res$p_value
    print(i)
    
}

p_values_facf_6 <- numeric() # Vector of p-values for FACF

for (i in 1 : 10) {
    
    facf_res <- fACF_test(f_data = fdata_6, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
    p_values_facf_6[i] <- facf_res$p_value
    print(i)
    
}

sdo_res <- spectral_test(fdata_6, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_6 <- rep(sdo_res$p_value, 10) # Vector of p-values for SDO

p_values_fsacf_6 <- numeric() # Vector of p-values for FSACF

for (i in 1 : 10) {
    
    fsacf_res <- fSACF_test(f_data = fdata_6, H = lags_test[i], pplot = T, alpha = alpha)
    p_values_fsacf_6[i] <- fsacf_res$p_value
    print(i)
    
}
