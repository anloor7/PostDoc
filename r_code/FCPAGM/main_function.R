

# Input parameters
# series: a list of time series
# K: number of clusters
# m: fuzziness parameter
# max_lag: model order
# niter: maximum number of iterations
# seed: seed


f_k_means_global_models_in_sample <- function(series, K, m = 2, max_lag = 1, niter = 150, seed = NULL) {
  
  n_series <- length(series)
  series_lengths <- lapply(series, length)
  exponent <- (1)/(m-1)
  
  # Computing the matrices associated to each series (later these matrices will be
  # used to compute the predictions)
  
  prediction_matrices <- lapply(series, prediction_matrix, max_lag = max_lag)
  
  # Initializing the membership matrix
  
  set.seed(seed)
  U <- matrix(0, nrow = n_series, ncol = K)
  
  for (i in 1 : n_series) {
    
    U[i,] <- rchisq(K, df = 3)
    U[i,] <- U[i,]/sum(U[i,])
    
  }
  
  set.seed(NULL)
  
  
  # Initializing the global models
  
  initial_models <- list()
  
  for (i in 1 : K) {
    
    initial_models[[i]] <- f_global_model_function(series, U[,i], max_lag = max_lag, m = m)
    
  }
  
  
  # Repeat
  
  current_U <- 0
  old_U <- 1
  models <- initial_models
  iter <- 0
  objective_function <- numeric()
  error_list <- list()
  clustering_solution_list <- list()
  centroids_list <- list()
  mae_iter <- numeric()
  mae_vector_list <- list()
  
  while (iter < niter & (sum(current_U == old_U) != (n_series * K)) & if(iter > 7) {objective_function[iter - 6] > min(c(objective_function[iter - 5], 
                                                                                                                         objective_function[iter - 4],
                                                                                                                         objective_function[iter - 3],
                                                                                                                         objective_function[iter - 2],
                                                                                                                         objective_function[iter - 1]))} else {T}) {
    
    
    # Recomputing the fuzzy partition
    
    old_U = current_U
    current_U <- matrix(0, nrow = n_series, ncol = K)
    matrix_errors <- matrix(0, nrow = n_series, ncol = K)
    
    
    # Computing the matrix of prediction errors and the membership matrix
    
    for (j1 in 1 : n_series){
      
      distances <- numeric()
      
      for(j2 in 1 : K){
        
        vector_coefficients <- models[[j2]]$coeff[-1]
        
        if (length(vector_coefficients) != max_lag) {
          
          vector_coefficients <- c(vector_coefficients, 
                                   rep(0, max_lag - length(models[[j2]]$coeff[-1])))
        }
        
        distances[j2] <- prediction_distance_in_sample(series[[j1]][-(1 : max_lag)],
                                                       prediction_matrices[[j1]],
                                                       vector_coefficients,
                                                       rep(models[[j2]]$coeff[1], series_lengths[[j1]] - max_lag))
        
      }
      
      matrix_errors[j1,] <- distances
      
    }
    
    
    for (i in 1 : n_series) {
      
      for (k in 1 : K) {
        
        current_U[i, k] <- sum((matrix_errors[i, k]/matrix_errors[i,])^exponent)^(-1)
        
      }
      
    }
    
    
    # Recomputation of global models
    
    
    for (p in 1 : K) {
      
      models[[p]] <- f_global_model_function(series, current_U[,p], max_lag = max_lag, m = m)
      
    }
    
    
    iter <- iter + 1
    
    objective_function[iter] <- sum(current_U^m*matrix_errors)
    error_list[[iter]] <- matrix_errors
    clustering_solution_list[[iter]] <- current_U
    centroids_list[[iter]] <- models
    mae_iter[iter] <- sum(current_U^m*matrix_errors)
    mae_vector_list[[iter]] <- sum(current_U^m*matrix_errors)
    
  }
  
  sample_numbers <- sample(1 : K)
  clustering_solution_list <- clustering_solution_list[-iter]
  clustering_solution_list <- c(list(sample_numbers), clustering_solution_list)
  
  
  # Computing the value of the objective function
  
  
  # distances_min <- numeric()
  # for (j1 in 1 : n_series){
  
  #  index <- indexes_assignment[j_1]
  #  distances_min[j1] <- prediction_distance(training_series[[j1]], test_series[[j1]],
  #                                         models[[index]], max_lag = max_lag)
  
  
  # }
  
  if (iter > 7) {
    
    if (objective_function[iter - 6] <= min(c(objective_function[iter - 5], 
                                              objective_function[iter - 4],
                                              objective_function[iter - 3],
                                              objective_function[iter - 2],
                                              objective_function[iter - 1]))) {
      
      
      
      list_return <- list(clustering = clustering_solution_list[[iter - 6]],
                          centroids = centroids_list[[iter - 6]],
                          niter = iter,
                          mae = mae_iter[iter - 6],
                          mae_vector = mae_vector_list[[iter - 6]],
                          objective_function = objective_function,
                          error_list = error_list,
                          clustering_solution_list = clustering_solution_list,
                          centroids_list = centroids_list)
      
    } else { 
      
      list_return <- list(clustering = current_U,
                          centroids = models,
                          niter = iter,
                          mae =  sum(current_U^m*matrix_errors),
                          mae_vector =  sum(current_U^m*matrix_errors),
                          objective_function = objective_function,
                          error_list = error_list,
                          clustering_solution_list = clustering_solution_list,
                          centroids_list = centroids_list)
      
    }
    
  } else {
    
    list_return <- list(clustering = current_U,
                        centroids = models,
                        niter = iter,
                        mae = sum(current_U^m*matrix_errors),
                        mae_vector = sum(current_U^m*matrix_errors),
                        objective_function = objective_function,
                        error_list = error_list,
                        clustering_solution_list = clustering_solution_list,
                        centroids_list = centroids_list)
    
  }
  
  
  return(list_return)
  
  
  
  
}

