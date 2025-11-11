
# Script for the simulations in Scenario 2 of the paper

series_1 <- list() # Time series in the first cluster
series_2 <- list() # Time series in the second cluster
series_3 <- list() # Time series in the third cluster
series_length <- 200 # Series length
lags <- c(1) # Lags
n_series <- 5 # Number of time series per cluster
levels <- c(0.1, 0.5, 0.9)  # Quantile levels
K <- 3 # Number of clusters
m_values <- seq(series_length/20, series_length, series_length/20) # Grid of values for m
ground_truth <- rep(1 : K, each = n_series) # True clustering partition
ari_1 <- numeric() # ARI for LSQAF
selected_m <- numeric()
iters <- numeric()
ari_2 <- numeric() # ARI for QAF
ari_3 <- numeric() # ARI for LSMODWT2
ari_4 <- numeric() # ARI for LSMODWT4
ari_5 <- numeric() # ARI for EWS
j_1 <- numeric() # JI for LSQAF
j_2 <- numeric() # JI for QAF
j_3 <- numeric() # JI for LSMODWT2
j_4 <- numeric() # JI for LSMODWT4
j_5 <- numeric() # JI for EWS
trials <- 200 # Number of simulation trials
# set.seed(123) # We can set a seed

for (j in 1 : trials) {
  
  
  for (i in 1 : n_series) {
    
    series_1[[i]] <- simulate_tqvar_1(series_length, 1.9, -0.95, -1.9, 0.95)
    series_2[[i]] <- simulate_tqvar_1(series_length, 1.3, -0.95, 0, 0)
    series_3[[i]] <- simulate_tqvar_1(series_length, 0.7, -0.95, -0.7, 0.95)
    
  }
  
  cluster <- c(series_1, series_2, series_3)
  
  a1 <- clustering_ls_acf(list_series = cluster, m_values = m_values, K = K, levels = levels, lags = lags, init = c(1, 6, 11))
  
  features_qacf <- lapply(cluster, qafs, levels = levels, lags = lags)
  features_w1 <- lapply(cluster, features_wavelet_block, bs = 2)
  features_w2 <- lapply(cluster, features_wavelet_block, bs = 4)
  features_ews <- lapply(cluster, ews_vector)
  
  p2 <- generic_k_medoids(features_qacf, K = K, init = c(1, 6, 11))$Partition
  p3 <- generic_k_medoids(features_w1, K = K, init = c(1, 6, 11))$Partition
  p4 <- generic_k_medoids(features_w2, K = K, init = c(1, 6, 11))$Partition
  p5 <- generic_k_medoids(features_ews, K = K, init = c(1, 6, 11))$Partition
  
  ari_1[j] <- external_validation(a1$Partition, ground_truth)
  selected_m[j] <- a1$m
  iters[j] <- a1$Iter
  ari_2[j] <- external_validation(p2, ground_truth)
  ari_3[j] <- external_validation(p3, ground_truth)
  ari_4[j] <- external_validation(p4, ground_truth)
  ari_5[j] <- external_validation(p5, ground_truth)
  j_1[j] <- external_validation(a1$Partition, ground_truth, method = 'jaccard_index')
  j_2[j] <- external_validation(p2, ground_truth, method = 'jaccard_index')
  j_3[j] <- external_validation(p3, ground_truth, method = 'jaccard_index')
  j_4[j] <- external_validation(p4, ground_truth, method = 'jaccard_index')
  j_5[j] <- external_validation(p5, ground_truth, method = 'jaccard_index')
  
  print(j)
  
}

boxplot(ari_1, ari_2, ari_3, ari_4, ari_5)
boxplot(j_1, j_2, j_3, j_4, j_5)

