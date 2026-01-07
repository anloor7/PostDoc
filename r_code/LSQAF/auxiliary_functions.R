
# install.packages(c("mlmts", "wavethresh", "TSdist", "proxy", "Rfast", "ClusterR", "refund"))
library(mlmts)
library(wavethresh)
library(TSdist)
library(proxy)
library(Rfast)
library(ClusterR)
library(refund)

# Function to simulate the processes in Scenario 1

simulate_tvar_2 <- function(series_length, coef_1, coef_2, coef_3) {
  
  points <- 1 : series_length
  coefs_lag_1 <- coef_1 * cos(coef_2 - cos(2 * pi * points/series_length))
  coefs_lag_2 <- rep(coef_3, series_length)
  
  series <- numeric()
  series[1] <- rnorm(1)
  series[2] <- rnorm(1)
  
  for (i in 3 : series_length) {
    
    series[i] <- coefs_lag_1[i] * series[i - 1] + coefs_lag_2[i] * series[i - 2] + rnorm(1)
    
  }
  
  return(series)
  
  
}

# Function to simulate the processes in Scenario 2

simulate_tqvar_1 <- function(series_length, coef_1, coef_2, coef_3, coef_4) {
  
  points <- (1 : series_length)/series_length
  
  series <- numeric()
  series[1] <- runif(1)
  
  for (i in 2 : series_length) {
    
    udata <- runif(1)
    series[i] <- ((coef_1 * udata + coef_2) * points[i] + (coef_3 * udata + coef_4) * (1 - points[i])) * series[i - 1] + udata - 1/2 
    
  }
  
  return(series)
  
  
}

# Function to simulate the processes in Scenario 3

simulate_tsetar_2 <- function(series_length, coef_1, coef_2, coef_3, coef_4, 
                              coef_5, coef_6, coef_7, coef_8, threshold) {
  
  points <- 1 : series_length
  coefs_1_1 <- coef_1 + coef_2 * points/series_length
  coefs_1_2 <- coef_3 + coef_4 * points/series_length
  coefs_1_3 <- coef_5 + coef_6 * points/series_length
  coefs_1_4 <- coef_7 + coef_8 * points/series_length
  
  series <- numeric()
  series[1] <- rnorm(1)
  series[2] <- rnorm(1)
  
  for (i in 3 : series_length) {
    
    if (series[i-2] <= threshold) {
      
      series[i] <- coefs_1_1[i] * series[i - 1] + coefs_1_2[i] * series[i - 2] + rnorm(1)
      
    } else {
      
      series[i] <- coefs_1_3[i] * series[i - 1] + coefs_1_4[i] * series[i - 2] + rnorm(1)
      
    }
    
  }
  
  return(series)
  
  
}


# Functions to extract the QAF features (under stationarity)

qaf <- function(series, tau_1 = 0.5, tau_2 = 0.5, lag = 1) {
  
  l_series <- length(series)
  series_1 <- series[1 : (l_series - lag)]
  series_2 <- series[(lag + 1) : l_series]
  q_tau_1 <- as.numeric(quantile(series, probs = tau_1))
  q_tau_2 <- as.numeric(quantile(series, probs = tau_2))
  
  indicators_1 <- as.numeric(series_1 <= q_tau_1)
  indicators_2 <- as.numeric(series_2 <= q_tau_2)
  
  return(cor(indicators_1, indicators_2))
  
}


qafs <- function(series, levels, lags) {
  
  l_levels <- length(levels)
  l_lags <- length(lags)
  array_features <- array(0, dim = c(l_levels, l_levels, l_lags))
  
  
  for (i in 1 : l_levels) {
    
    for (j in 1 : l_levels) {
      
      for (k in 1 : l_lags) {
        
        array_features[i, j , k] <- qaf(series, tau_1 = levels[i], tau_2 = levels[j], 
                                                                lag = lags[k])
        
      }
      
    }
    
  }
  
  return(c(array_features))
  
}


# Functions to extract the LSMODWT features

features_wavelet_block <- function(series, bs = 2) {
  
  series_length <- length(series)
  x <- 1 : series_length
  chunk_size <- series_length/bs
  split_vector <- split(x, ceiling(seq_along(x)/chunk_size))
  list_features <- list()
  
  for (i in 1 : bs) {
    
    list_features[[i]] <- mlmts::dis_modwt(list(as.matrix(series[split_vector[[i]]])), features = T)
    
  }
  
  return(unlist(list_features))
  
}


# Functions to extract the EWS features

truncate_to_dyadic <- function(x) {
  n <- length(x)
  # Find the largest power of 2 less than or equal to n
  dyadic_length <- 2^(floor(log2(n)))
  # Truncate the vector to that length
  truncated_x <- x[1:dyadic_length]
  return(truncated_x)
}


ews_vector <- function(x) {
  
  truncated_series <- truncate_to_dyadic(x)
  X <- wavethresh::ewspec(truncated_series)
  feature_vector <- X$S$D
  
  return(feature_vector)
  
}


# Functions to execute a generic K-medoids algorithm given a feature matrix

list_to_matrix <- function(l){
  n <- length(l)
  s <- length(l[[1]])
  m <- base::matrix(nrow = n, ncol = s)
  for (i in 1 : n) {
    m[i,] <- l[[i]]
  }
  m
}


update_medoids_generic <- function(list_features) {
  
  n_series <- length(list_features)
  
  
  matrix_features <- list_to_matrix(list_features)
  
  dis_features <- as.matrix(proxy::dist(matrix_features))^2
  cs <- colsums(dis_features)
  index_series <- which.min(cs)
  
  return(list_features[[index_series]])
  
}


index_medoids_generic <- function(list_features, new_medoids) {
  
  K <- length(new_medoids)
  matrix_features <- apply(list_to_matrix(list_features), 1, function(x) {sum(x^2)})
  matrix_medoids <- apply(list_to_matrix(new_medoids), 1, function(x) {sum(x^2)})
  indexes <- numeric()
  
  for (i in 1 : K) {
    
    indexes[[i]] <- which(matrix_medoids[[i]] == matrix_features)
    
  }
  
  return(indexes)
  
}




generic_k_medoids <- function(list_features, K = 2, max_iter = 100, init = NULL) {
  
  n_series <- length(list_features)
  
  
  # Picking initial medoids
  
  if (is.null(init)) {
    
    initial_medoids <- sample(n_series, K)
    
  } else {
    
    initial_medoids <- init
    
  }
  
  
  index_medoids <- initial_medoids
  medoids <- list_features[index_medoids]
  old_medoids <- index_medoids + 1
  
  
  # Iterative procedure
  
  iter = 0
  dis_matrix <- matrix(0, n_series, K)
  
  while (iter < max_iter & all(old_medoids != index_medoids)) {
    
    old_medoids  <- index_medoids
    
    
    # Updating the clustering partition for given medoids and value of m
    
    for (i in 1 : n_series) {
      
      for (j in 1 : K) {
        
        dis_matrix[i, j] <- TSdist::EuclideanDistance(list_features[[i]], medoids[[j]])^2
        
      } 
      
    }
    
    dis_matrix[is.na(dis_matrix)] <- runif(1)
    partition <- apply(dis_matrix, 1, which.min)
    
    u_cluster_vector <- unique(partition)
    
    if (length(u_cluster_vector) != K) {
      n_clusters <- length(unique(partition))
      
      for (i in 1 : n_series) {
        
        partition[i] <- which(partition[i] == 
                                u_cluster_vector)
      }
      
      K <- n_clusters
      
    }
    
    
    # Updating the set of medoids for a given clustering partition and value of
    # m
    
    new_medoids <- list()
    
    
    for (i in 1 : K) {
      
      indexes_i <- which(partition == i)
      new_medoids[[i]] <- update_medoids_generic(list_features[indexes_i])
      
    }
    
    index_medoids <- index_medoids_generic(list_features, new_medoids)
    medoids <- list_features[index_medoids]
    iter <- iter + 1
    
  }
  
  return_list <- list('Partition' = partition, 
                      'Medoids' = index_medoids,
                      'Iter' = iter,
                      'Of' = sum(apply(dis_matrix, 1, min)))
  
  return(return_list)
  
  
}

