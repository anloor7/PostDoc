
# Necessary packages
#install.packages(c("proxy", "Rfast"))
library(proxy)
library(Rfast)

# This is the main function to execute the approach LSQAF
# Input parameters
# list_series: A list of time series (numeric vectors)
# m_values: Vector of values for the radius of the neighborhood for the local estimates of QAF
# K: Number of clusters
# levels: Vector of quantile levels
# lags: Vector of lags
# max_iter: Maximum number of iterations
# init: Vector with indices of initial medoids (default is random)

clustering_ls_acf <- function(list_series, m_values, K = 2, levels = c(0.1, 0.5, 0.9), lags = 1, max_iter = 100, init = NULL) {
  
  n_series <- length(list_series)
  
  
  # Picking initial medoids
  
  if (is.null(init)) {
    
    initial_medoids <- sample(n_series, K)
    
  } else {
    
    initial_medoids <- init
    
  }
  
  
  index_medoids <- initial_medoids
  medoids <- list_series[index_medoids]
  old_medoids <- index_medoids + 1
  
  # Picking initial value for m
  
  m_value <- sample(m_values, 1)
  
  
  # Iterative procedure
  
  iter = 0
  dis_matrix <- matrix(0, n_series, K)
  
  while (iter < max_iter & all(old_medoids != index_medoids)) {
    
    old_medoids  <- index_medoids
    
    
    # Updating the clustering partition for given medoids and value of m
    
    for (i in 1 : n_series) {
      
      for (j in 1 : K) {
        
        dis_matrix[i, j] <- distance_ls(list_series[[i]], medoids[[j]], m = m_value, levels = levels,
                                        lags = lags)
        
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
    
    
    # Updating the value of m for given medoids and clustering partition
    
    
    new_value_m <- update_m(list_series = list_series, medoids = medoids, partition = partition, m_values = m_values, levels = levels, lags = lags)$m
    
    
    
    
    # Updating the set of medoids for a given clustering partition and value of
    # m
    
    new_medoids <- list()
    
    
    for (i in 1 : K) {
      
      indexes_i <- which(partition == i)
      new_medoids[[i]] <- update_medoids_ls(list_series[indexes_i], new_value_m, levels = levels, lags = lags)
      
    }
    
    index_medoids <- index_medoids_ls(list_series, new_medoids)
    medoids <- list_series[index_medoids]
    iter <- iter + 1
    
  }
  
  return_list <- list('Partition' = partition, 
                      'Medoids' = index_medoids, 'm' = new_value_m, 
                      'Of' = update_m(list_series = list_series, medoids = medoids, partition = partition, m_values = m_values, levels = levels, lags = lags)$of,
                      'Iter' = iter)
  
  return(return_list)
  
  
}


# Functions within clustering_ls_acf()

qaf_local <- function(series, t_0, tau_1 = 0.5, tau_2 = 0.5, lag = 1,
                                              n_1, n_2) {
  
  series_length <- length(series)
  indexes_1 <- which(abs(1 : series_length - t_0) <= n_1) # Indexes for the estimation of the quantiles
  indexes_2 <- which(abs(1 : series_length - t_0) <= n_2) # Indexes for the estimation of QAF
  series_1 <- series[indexes_1]
  series_2 <- series[indexes_2]
  l_2 <- length(series_2)
  
  q_tau_1 <- as.numeric(quantile(series_1, probs = tau_1))
  q_tau_2 <- as.numeric(quantile(series_1, probs = tau_2))
  
  subseries_1 <- series_2[1 : (l_2 - lag)]
  subseries_2 <- series_2[(lag + 1) : l_2]
  
  indicators_1 <- as.numeric(subseries_1 <= q_tau_1)
  indicators_2 <- as.numeric(subseries_2 <= q_tau_2)
  
  return(cor(indicators_1, indicators_2))
  
}

qafs_local <- function(series, levels, lags, n_1 = 10, n_2 = 10) {
  
  l_levels <- length(levels)
  l_lags <- length(lags)
  l_points <- length(series)
  array_features <- array(0, dim = c(l_levels, l_levels, l_lags, l_points))
  
  
  for (i in 1 : l_levels) {
    
    for (j in 1 : l_levels) {
      
      for (k in 1 : l_lags) {
        
        for (l in 1 : l_points) {
          
          array_features[i, j , k, l] <- qaf_local(series = series, t_0 = l, tau_1 = levels[i], 
                                                                           tau_2 = levels[j], 
                                                                           lag = lags[k],
                                                                           n_1 = n_1, n_2 = n_2)
          
        }
        
      }
      
    }
    
  }
  
  return(c(array_features))
  
}


distance_ls <- function(series_1, series_2, m = 10, levels = c(0.1, 0.5, 0.9), lags = c(1)) {
  
  features_1 <- qafs_local(series = series_1, levels = levels, lags = lags, n_1 = m,
                                                   n_2 = m)
  features_2 <- qafs_local(series = series_2, levels = levels, lags = lags, n_1 = m,
                                                   n_2 = m)
  features_1[is.na(features_1)] <- mean(features_1, na.rm = T)
  features_2[is.na(features_2)] <- mean(features_2, na.rm = T)
  
  series_length <- length(series_1)
  
  n_factor <- 1/(4 * series_length * length(lags) * length(levels)^2)
  
  return(n_factor * sum((features_1 - features_2)^2))
  
}


sum_distances_m <- function(list_series, medoids, partition, m = 10, levels = c(0.1, 0.5, 0.9), lags = c(1)) {
  
  K <- length(medoids)
  distance_vector <- numeric()
  
  for (i in 1 : K) {
    
    distances <- numeric()
    indexes_i <- which(partition == i)
    
    for (j in indexes_i) {
      
      distances[j] <- distance_ls(list_series[[j]], medoids[[i]], m = m, levels = levels, lags = lags)
      
    }
    
    distance_vector[i] <- sum(distances, na.rm = T)
    
  }
  
  return(sum(distance_vector))
  
}


update_m <- function(list_series, medoids, partition, m_values, levels = c(0.1, 0.5, 0.9), lags = c(1)) {
  
  sum_distances <- numeric()
  series_length <- length(list_series[[1]])
  l_m <- length(m_values)
  
  for (i in 1 : l_m) {
    
    sum_distances[i] <- sum_distances_m(list_series, medoids, partition, m_values[i], levels = levels, lags = lags)
    
  }
  
  values <- numeric()
  maxmin <- max(sum_distances) - min(sum_distances)
  
  for (i in 1 : l_m) {
    
    values[i] <- sum_distances[i] + m_values[i]/series_length * maxmin
    
  }
  
  
  
  return_list <- list(m = m_values[which.min(values[1 : (l_m)])], of = min(sum_distances[1 : (l_m)]))
  return(return_list)
  
}


list_to_matrix <- function(l){
  n <- length(l)
  s <- length(l[[1]])
  m <- base::matrix(nrow = n, ncol = s)
  for (i in 1 : n) {
    m[i,] <- l[[i]]
  }
  m
}


update_medoids_ls <- function(list_series, m, levels = levels, lags = lags) {
  
  n_series <- length(list_series)
  list_features <- lapply(list_series, qafs_local, levels = levels, lags = lags, n_1 = m, n_2 = m)
  
  matrix_features <- list_to_matrix(list_features)
  
  series_length <- length(list_series[[1]])
  n_factor <- 1/(4 * series_length * length(lags) * length(levels)^2)
  
  dis_features <- n_factor * as.matrix(proxy::dist(matrix_features))^2
  cs <- colsums(dis_features)
  index_series <- which.min(cs)
  
  return(list_series[[index_series]])
  
}

index_medoids_ls <- function(list_series, new_medoids) {
  
  K <- length(new_medoids)
  indexes <- numeric()
  
  for (i in 1 : K) {
    
    indexes[[i]] <- which(sum(new_medoids[[i]]) == unlist(lapply(list_series, sum)))
    
  }
  
  return(indexes)
  
}

