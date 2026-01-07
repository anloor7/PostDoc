
# Script for the simulations in Scenario 1 of the paper: original EWS method of Hargreaves et al. (2018), with dimensionality reduction

series_1 <- list()
series_2 <- list()
series_3 <- list()
series_length <- 200
n_series <- 5
K <- 3
ground_truth <- rep(1 : K, each = n_series)
ari <- numeric()
trials <- 200
ncomp <- 1 # Number of principal components



for (j in 1 : trials) {
  
  for (i in 1 : n_series) {
    
    series_1[[i]] <- simulate_tvar_2(series_length, 1, 1.5, 0)
    series_2[[i]] <- simulate_tvar_2(series_length, -1, 1.5, 0)
    series_3[[i]] <- simulate_tvar_2(series_length, -1, 0.8, 0)
    
  }
  
  cluster <- c(series_1, series_2, series_3)
  
  features_wavelets <- lapply(cluster, ews_vector)
  feature_matrix <- list_to_matrix(features_wavelets)
  pca_result <- prcomp(feature_matrix, center = TRUE, scale = TRUE)
  pc_scores <- pca_result$x[, 1:ncomp]
  dist_matrix <- dist(pc_scores, method = "euclidean")^2
  partition <- pam(dist_matrix, k = K)$clustering
  ari[j] <- external_validation(partition, ground_truth)
  
  
  print(j)
  
}

mean(ari)


