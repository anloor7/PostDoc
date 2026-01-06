
setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_functional/data/finance')


# Loading the data

first_data <- read.csv('first_hanlin.csv')[c(1, 4, 7, 8)]


# Indexes of companies in communication services sector and corresponding dataset

indexes_f_1 <- which(first_data$X.RIC == 'GOOGL.OQ'); df_f_1 <- first_data[indexes_f_1,] 
indexes_f_2 <- which(first_data$X.RIC == 'GOOG.OQ'); df_f_2 <- first_data[indexes_f_2,] 
indexes_f_3 <- which(first_data$X.RIC == 'T.N'); df_f_3 <- first_data[indexes_f_3,]
indexes_f_4 <- which(first_data$X.RIC == 'CHTR.OQ'); df_f_4 <- first_data[indexes_f_4,]
indexes_f_5 <- which(first_data$X.RIC == 'CSCO.OQ'); df_f_5 <- first_data[indexes_f_5,] 
indexes_f_6 <- which(first_data$X.RIC == 'CMCSA.OQ'); df_f_6 <- first_data[indexes_f_6,] 
indexes_f_7 <- which(first_data$X.RIC == 'EA.OQ'); df_f_7 <- first_data[indexes_f_7,] 
indexes_f_8 <- which(first_data$X.RIC == 'FOXA.OQ'); df_f_8 <- first_data[indexes_f_8,] 
indexes_f_9 <- which(first_data$X.RIC == 'FOX.OQ'); df_f_9 <- first_data[indexes_f_9,] 
indexes_f_10 <- which(first_data$X.RIC == 'IPG.N'); df_f_10 <- first_data[indexes_f_10,] 
indexes_f_11 <- which(first_data$X.RIC == 'LYV.N'); df_f_11 <- first_data[indexes_f_11,] 
indexes_f_12 <- which(first_data$X.RIC == 'FB.OQ'); df_f_12 <- first_data[indexes_f_12,] 
indexes_f_13 <- which(first_data$X.RIC == 'NFLX.OQ'); df_f_13 <- first_data[indexes_f_13,] 
indexes_f_14 <- which(first_data$X.RIC == 'NWSA.OQ'); df_f_14 <- first_data[indexes_f_14,] 
indexes_f_15 <- which(first_data$X.RIC == 'NWS.OQ'); df_f_15 <- first_data[indexes_f_15,] 
indexes_f_16 <- which(first_data$X.RIC == 'OMC.N'); df_f_16 <- first_data[indexes_f_16,] 
indexes_f_17 <- which(first_data$X.RIC == 'TMUS.OQ'); df_f_17 <- first_data[indexes_f_17,] 
indexes_f_18 <- which(first_data$X.RIC == 'TTWO.OQ'); df_f_18 <- first_data[indexes_f_18,] 
indexes_f_19 <- which(first_data$X.RIC == 'VZ.N'); df_f_19 <- first_data[indexes_f_19,] 
indexes_f_20 <- which(first_data$X.RIC == 'DIS.N'); df_f_20 <- first_data[indexes_f_20,] 




# Indexes of companies in energy sector and corresponding dataset

indexes_u_1 <- which(first_data$X.RIC == 'APA.N'); df_u_1 <- first_data[indexes_u_1,] 
indexes_u_2 <- which(first_data$X.RIC == 'BKR.N'); df_u_2 <- first_data[indexes_u_2,] 
indexes_u_3 <- which(first_data$X.RIC == 'CVX.N'); df_u_3 <- first_data[indexes_u_3,] 
indexes_u_4 <- which(first_data$X.RIC == 'COP.N'); df_u_4 <- first_data[indexes_u_4,]
indexes_u_5 <- which(first_data$X.RIC == 'DVN.N'); df_u_5 <- first_data[indexes_u_5,] 
indexes_u_6 <- which(first_data$X.RIC == 'FANG.OQ'); df_u_6 <- first_data[indexes_u_6,] 
indexes_u_7 <- which(first_data$X.RIC == 'EOG.N'); df_u_7 <- first_data[indexes_u_7,] 
indexes_u_8 <- which(first_data$X.RIC == 'EQT.N'); df_u_8 <- first_data[indexes_u_8,] 
indexes_u_9 <- which(first_data$X.RIC == 'XOM.N'); df_u_9 <- first_data[indexes_u_9,] 
indexes_u_10 <- which(first_data$X.RIC == 'HAL.N'); df_u_10 <- first_data[indexes_u_10,] 
indexes_u_11 <- which(first_data$X.RIC == 'HES.N'); df_u_11 <- first_data[indexes_u_11,] 
indexes_u_12 <- which(first_data$X.RIC == 'KMI.N'); df_u_12 <- first_data[indexes_u_12,] 
indexes_u_13 <- which(first_data$X.RIC == 'MRO.N'); df_u_13 <- first_data[indexes_u_13,] 
indexes_u_14 <- which(first_data$X.RIC == 'MPC.N'); df_u_14 <- first_data[indexes_u_14,] 
indexes_u_15 <- which(first_data$X.RIC == 'OXY.N'); df_u_15 <- first_data[indexes_u_15,] 
indexes_u_16 <- which(first_data$X.RIC == 'OKE.N'); df_u_16 <- first_data[indexes_u_16,] 
indexes_u_17 <- which(first_data$X.RIC == 'PSX.N'); df_u_17 <- first_data[indexes_u_17,] 
indexes_u_18 <- which(first_data$X.RIC == 'PXD.N'); df_u_18 <- first_data[indexes_u_18,] 
indexes_u_19 <- which(first_data$X.RIC == 'SLB.N'); df_u_19 <- first_data[indexes_u_19,] 
indexes_u_20 <- which(first_data$X.RIC == 'VLO.N'); df_u_20 <- first_data[indexes_u_20,] 


# Creating the corresponding FTS

fts_f_1 <- create_sp_fts(df_f_1); fts_f_1[is.na(fts_f_1)] <- 0
fts_f_2 <- create_sp_fts(df_f_2); fts_f_2[is.na(fts_f_2)] <- 0
fts_f_3 <- create_sp_fts(df_f_3); fts_f_3[is.na(fts_f_3)] <- 0
fts_f_4 <- create_sp_fts(df_f_4); fts_f_4[is.na(fts_f_4)] <- 0
fts_f_5 <- create_sp_fts(df_f_5); fts_f_5[is.na(fts_f_5)] <- 0
fts_f_6 <- create_sp_fts(df_f_6); fts_f_6[is.na(fts_f_6)] <- 0
fts_f_7 <- create_sp_fts(df_f_7); fts_f_7[is.na(fts_f_7)] <- 0
fts_f_8 <- create_sp_fts(df_f_8); fts_f_8[is.na(fts_f_8)] <- 0
fts_f_9 <- create_sp_fts(df_f_9); fts_f_9[is.na(fts_f_9)] <- 0
fts_f_10 <- create_sp_fts(df_f_10); fts_f_10[is.na(fts_f_10)] <- 0
fts_f_11 <- create_sp_fts(df_f_11); fts_f_11[is.na(fts_f_11)] <- 0
fts_f_12 <- create_sp_fts(df_f_12); fts_f_12[is.na(fts_f_12)] <- 0
fts_f_13 <- create_sp_fts(df_f_13); fts_f_13[is.na(fts_f_13)] <- 0
fts_f_14 <- create_sp_fts(df_f_14); fts_f_14[is.na(fts_f_14)] <- 0
fts_f_15 <- create_sp_fts(df_f_15); fts_f_15[is.na(fts_f_15)] <- 0
fts_f_16 <- create_sp_fts(df_f_16); fts_f_16[is.na(fts_f_16)] <- 0
fts_f_17 <- create_sp_fts(df_f_17); fts_f_17[is.na(fts_f_17)] <- 0
fts_f_18 <- create_sp_fts(df_f_18); fts_f_18[is.na(fts_f_18)] <- 0
fts_f_19 <- create_sp_fts(df_f_19); fts_f_19[is.na(fts_f_19)] <- 0
fts_f_20 <- create_sp_fts(df_f_20); fts_f_20[is.na(fts_f_20)] <- 0



fts_u_1 <- create_sp_fts(df_u_1); fts_u_1[is.na(fts_u_1)] <- 0
fts_u_2 <- create_sp_fts(df_u_2); fts_u_2[is.na(fts_u_2)] <- 0
fts_u_3 <- create_sp_fts(df_u_3); fts_u_3[is.na(fts_u_3)] <- 0
fts_u_4 <- create_sp_fts(df_u_4); fts_u_4[is.na(fts_u_4)] <- 0
fts_u_5 <- create_sp_fts(df_u_5); fts_u_5[is.na(fts_u_5)] <- 0
fts_u_6 <- create_sp_fts(df_u_6); fts_u_6[is.na(fts_u_6)] <- 0
fts_u_7 <- create_sp_fts(df_u_7); fts_u_7[is.na(fts_u_7)] <- 0
fts_u_8 <- create_sp_fts(df_u_8); fts_u_8[is.na(fts_u_8)] <- 0
fts_u_9 <- create_sp_fts(df_u_9); fts_u_9[is.na(fts_u_9)] <- 0
fts_u_10 <- create_sp_fts(df_u_10); fts_u_10[is.na(fts_u_10)] <- 0
fts_u_11 <- create_sp_fts(df_u_11); fts_u_11[is.na(fts_u_11)] <- 0
fts_u_12 <- create_sp_fts(df_u_12); fts_u_12[is.na(fts_u_12)] <- 0
fts_u_13 <- create_sp_fts(df_u_13); fts_u_13[is.na(fts_u_13)] <- 0
fts_u_14 <- create_sp_fts(df_u_14); fts_u_14[is.na(fts_u_14)] <- 0
fts_u_15 <- create_sp_fts(df_u_15); fts_u_15[is.na(fts_u_15)] <- 0
fts_u_16 <- create_sp_fts(df_u_16); fts_u_16[is.na(fts_u_16)] <- 0
fts_u_17 <- create_sp_fts(df_u_17); fts_u_17[is.na(fts_u_17)] <- 0
fts_u_18 <- create_sp_fts(df_u_18); fts_u_18[is.na(fts_u_18)] <- 0
fts_u_19 <- create_sp_fts(df_u_19); fts_u_19[is.na(fts_u_19)] <- 0
fts_u_20 <- create_sp_fts(df_u_20); fts_u_20[is.na(fts_u_20)] <- 0


# Analyses

quantile_levels <- seq(0.05, 0.95, 0.05)
alpha <- 0.05
lags_test <- 1 : 10

# Series 1

fts_1 <- fts_f_1
fdata_1 <- t(fts_1)

stats_fqa_1 <- numeric()
crit_fqa_1 <- numeric()
p_values_fqa_1 <- numeric()

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

p_values_fpc_1 <- numeric()

for (i in 1 : 10) {
  
ind_res <- independence_test(f_data = fdata_1, components = 3, lag = lags_test[i], alpha = alpha)
p_values_fpc_1[i] <- ind_res$p_value
print(i)

}

p_values_facf_1 <- numeric()

for (i in 1 : 10) {

facf_res <- fACF_test(f_data = fdata_1, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
p_values_facf_1[i] <- facf_res$p_value
print(i)

}

sdo_res <- spectral_test(fdata_1, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_1 <- rep(sdo_res$p_value, 10)

p_values_fsacf_1 <- numeric()

for (i in 1 : 10) {

fsacf_res <- fSACF_test(f_data = fdata_1, H = lags_test[i], pplot = T, alpha = alpha)
p_values_fsacf_1[i] <- fsacf_res$p_value
print(i)

}


# Series 2

fts_2 <- fts_f_3
fdata_2 <- t(fts_2)

stats_fqa_2 <- numeric()
crit_fqa_2 <- numeric()
p_values_fqa_2 <- numeric()

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

p_values_fpc_2 <- numeric()

for (i in 1 : 10) {
  
  ind_res <- independence_test(f_data = fdata_2, components = 3, lag = lags_test[i], alpha = alpha)
  p_values_fpc_2[i] <- ind_res$p_value
  print(i)
  
}

p_values_facf_2 <- numeric()

for (i in 1 : 10) {
  
  facf_res <- fACF_test(f_data = fdata_2, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
  p_values_facf_2[i] <- facf_res$p_value
  print(i)
  
}

sdo_res <- spectral_test(fdata_2, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_2 <- rep(sdo_res$p_value, 10)

p_values_fsacf_2 <- numeric()

for (i in 1 : 10) {
  
  fsacf_res <- fSACF_test(f_data = fdata_2, H = lags_test[i], pplot = T, alpha = alpha)
  p_values_fsacf_2[i] <- fsacf_res$p_value
  print(i)
  
}


# Series 3

fts_3 <- fts_f_5
fdata_3 <- t(fts_3)

stats_fqa_3 <- numeric()
crit_fqa_3 <- numeric()
p_values_fqa_3 <- numeric()

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

p_values_fpc_3 <- numeric()

for (i in 1 : 10) {
  
  ind_res <- independence_test(f_data = fdata_3, components = 3, lag = lags_test[i], alpha = alpha)
  p_values_fpc_3[i] <- ind_res$p_value
  print(i)
  
}

p_values_facf_3 <- numeric()

for (i in 1 : 10) {
  
  facf_res <- fACF_test(f_data = fdata_3, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
  p_values_facf_3[i] <- facf_res$p_value
  print(i)
  
}

sdo_res <- spectral_test(fdata_3, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_3 <- rep(sdo_res$p_value, 10)

p_values_fsacf_3 <- numeric()

for (i in 1 : 10) {
  
  fsacf_res <- fSACF_test(f_data = fdata_3, H = lags_test[i], pplot = T, alpha = alpha)
  p_values_fsacf_3[i] <- fsacf_res$p_value
  print(i)
  
}


# Series 4

fts_4 <- fts_f_6
fdata_4 <- t(fts_4)

stats_fqa_4 <- numeric()
crit_fqa_4 <- numeric()
p_values_fqa_4 <- numeric()

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

p_values_fpc_4 <- numeric()

for (i in 1 : 10) {
  
  ind_res <- independence_test(f_data = fdata_4, components = 3, lag = lags_test[i], alpha = alpha)
  p_values_fpc_4[i] <- ind_res$p_value
  print(i)
  
}

p_values_facf_4 <- numeric()

for (i in 1 : 10) {
  
  facf_res <- fACF_test(f_data = fdata_4, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
  p_values_facf_4[i] <- facf_res$p_value
  print(i)
  
}

sdo_res <- spectral_test(fdata_4, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_4 <- rep(sdo_res$p_value, 10)

p_values_fsacf_4 <- numeric()

for (i in 1 : 10) {
  
  fsacf_res <- fSACF_test(f_data = fdata_4, H = lags_test[i], pplot = T, alpha = alpha)
  p_values_fsacf_4[i] <- fsacf_res$p_value
  print(i)
  
}


# Series 5

fts_5 <- fts_f_7
fdata_5 <- t(fts_5)

stats_fqa_5 <- numeric()
crit_fqa_5 <- numeric()
p_values_fqa_5 <- numeric()

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

p_values_fpc_5 <- numeric()

for (i in 1 : 10) {
  
  ind_res <- independence_test(f_data = fdata_5, components = 3, lag = lags_test[i], alpha = alpha)
  p_values_fpc_5[i] <- ind_res$p_value
  print(i)
  
}

p_values_facf_5 <- numeric()

for (i in 1 : 10) {
  
  facf_res <- fACF_test(f_data = fdata_5, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
  p_values_facf_5[i] <- facf_res$p_value
  print(i)
  
}

sdo_res <- spectral_test(fdata_5, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_5 <- rep(sdo_res$p_value, 10)

p_values_fsacf_5 <- numeric()

for (i in 1 : 10) {
  
  fsacf_res <- fSACF_test(f_data = fdata_5, H = lags_test[i], pplot = T, alpha = alpha)
  p_values_fsacf_5[i] <- fsacf_res$p_value
  print(i)
  
}


# Series 6

fts_6 <- fts_f_12
fdata_6 <- t(fts_6)

stats_fqa_6 <- numeric()
crit_fqa_6 <- numeric()
p_values_fqa_6 <- numeric()

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

p_values_fpc_6 <- numeric()

for (i in 1 : 10) {
  
  ind_res <- independence_test(f_data = fdata_6, components = 3, lag = lags_test[i], alpha = alpha)
  p_values_fpc_6[i] <- ind_res$p_value
  print(i)
  
}

p_values_facf_6 <- numeric()

for (i in 1 : 10) {
  
  facf_res <- fACF_test(f_data = fdata_6, H = lags_test[i], iid = TRUE, M = NULL, alpha = alpha, pplot = T)
  p_values_facf_6[i] <- facf_res$p_value
  print(i)
  
}

sdo_res <- spectral_test(fdata_6, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
p_values_sdo_6 <- rep(sdo_res$p_value, 10)

p_values_fsacf_6 <- numeric()

for (i in 1 : 10) {
  
  fsacf_res <- fSACF_test(f_data = fdata_6, H = lags_test[i], pplot = T, alpha = alpha)
  p_values_fsacf_6[i] <- fsacf_res$p_value
  print(i)
  
}


# Plots series

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/postdoc/papers/tests_fqa/application')

hours <- seq(9.5, 16, by = 1/12)
par(mfrow = c(1,1))

# 1

series_1_smoothed <- t(apply(fts_1[1 : 100,], 1, c))
plot(fdata(series_1_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-return', main = 'GOOGL',
     ylim = c(-0.011, 0.011))

savefig('series_1_smoothed', width = 12, height = 10, type = 'png', toplines = 0.5)
plot(fdata(series_1_smoothed, argvals = hours[-c(1, 2)]), 
     xlab = '', ylab = 'Log-return', main = 'GOOGL',
     ylim = c(-0.011, 0.011),
     cex.lab = 1.4,      # Axis label size
     cex.axis = 1.3,     # Tick label/number size  
     cex.main = 1.5,     # Main title size
     cex = 1.2)          # Overall scaling
dev.off()

# 2

series_2_smoothed <- t(apply(fts_2[1 : 100,], 1, c))
plot(fdata(series_2_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-return', main = 'T',
     ylim = c(-0.011, 0.011))

savefig('series_2_smoothed', width = 12, height = 10, type = 'png', toplines = 0.5)
plot(fdata(series_2_smoothed, argvals = hours[-c(1, 2)]), 
     xlab = '', ylab = '', main = 'T',
     ylim = c(-0.011, 0.011),
     cex.lab = 1.4,      # Axis label size
     cex.axis = 1.3,     # Tick label/number size  
     cex.main = 1.5,     # Main title size
     cex = 1.2)   
dev.off()

# 3

series_3_smoothed <- t(apply(fts_3[1 : 100,], 1, c))
plot(fdata(series_3_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-return', main = 'CSCO',
     ylim = c(-0.011, 0.011))

savefig('series_3_smoothed', width = 12, height = 10, type = 'png', toplines = 0.5)
plot(fdata(series_3_smoothed, argvals = hours[-c(1, 2)]), 
     xlab = '', ylab = 'Log-return', main = 'CSCO',
     ylim = c(-0.011, 0.011),
     cex.lab = 1.4,      # Axis label size
     cex.axis = 1.3,     # Tick label/number size  
     cex.main = 1.5,     # Main title size
     cex = 1.2)   
dev.off()

# 4

series_4_smoothed <- t(apply(fts_4[1 : 100,], 1, c))
plot(fdata(series_4_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-return', main = 'CMCSA',
     ylim = c(-0.011, 0.011))

savefig('series_4_smoothed', width = 12, height = 10, type = 'png', toplines = 0.5)
plot(fdata(series_4_smoothed, argvals = hours[-c(1, 2)]), 
     xlab = '', ylab = '', main = 'CMCSA',
     ylim = c(-0.011, 0.011),
     cex.lab = 1.4,      # Axis label size
     cex.axis = 1.3,     # Tick label/number size  
     cex.main = 1.5,     # Main title size
     cex = 1.2)   
dev.off()

# 5

series_5_smoothed <- t(apply(fts_5[1 : 100,], 1, c))
plot(fdata(series_5_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-return', main = 'EA',
     ylim = c(-0.011, 0.011))

savefig('series_5_smoothed', width = 12, height = 10, type = 'png', toplines = 0.5)
plot(fdata(series_5_smoothed, argvals = hours[-c(1, 2)]), 
     xlab = 'Trading time', ylab = 'Log-return', main = 'EA',
     ylim = c(-0.011, 0.011),
     cex.lab = 1.4,      # Axis label size
     cex.axis = 1.3,     # Tick label/number size  
     cex.main = 1.5,     # Main title size
     cex = 1.2)   
dev.off()

# 6

series_6_smoothed <- t(apply(fts_6[1 : 100,], 1, c))
plot(fdata(series_6_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-return', main = 'META',
     ylim = c(-0.011, 0.011))

savefig('series_6_smoothed', width = 12, height = 10, type = 'png', toplines = 0.5)
plot(fdata(series_6_smoothed, argvals = hours[-c(1, 2)]), 
     xlab = 'Trading time', ylab = '', main = 'META',
     ylim = c(-0.011, 0.011),
     cex.lab = 1.4,      # Axis label size
     cex.axis = 1.3,     # Tick label/number size  
     cex.main = 1.5,     # Main title size
     cex = 1.2)   
dev.off()


# Plots p-values

library(ggplot2)
library(dplyr)
library(tidyr)

# 1

p_all <- data.frame(
  Lag = 1:10,
  FQA = p_values_fqa_1,
  FPC = p_values_fpc_1,
  FACF = p_values_facf_1,
  FSACF = p_values_fsacf_1,
  SDO = p_values_sdo_1
) %>%
  pivot_longer(-Lag, names_to = "Method", values_to = "p_value") %>%
  mutate(Method = factor(Method, levels = c("FQA", "FPC", "FACF", "FSACF", "SDO")))

ggplot(p_all, aes(x = Lag, y = p_value, color = Method, linetype = Method)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 2.5, alpha = 0.9) +
  scale_y_continuous("p-value", limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_color_manual(values = c("FQA" = "#E31A1C", "FPC" = "#1F78B4", 
                                "FACF" = "#33A02C", "FSACF" = "#FF7F00", 
                                "SDO" = "#6A3D9A")) +
  scale_linetype_manual(values = c("FQA" = "solid", "FPC" = "22", 
                                   "FACF" = "dotted", "FSACF" = "41", "SDO" = "13")) +
  labs(x = "Lag $h$", title = "Serial Dependence Tests: p-values vs.\ Lag") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 1, y = 0.06, label = "5% level", size = 4, hjust = 0) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

