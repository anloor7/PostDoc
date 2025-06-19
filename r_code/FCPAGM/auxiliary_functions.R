


prediction_matrix <- function(series, max_lag){
  
  series_length <- length(series)
  
  # Creating a dataset which contains, in each row, the vectors subjected
  # to be predicted
  
  dataset_predictions <- matrix(0, nrow = series_length - max_lag, ncol = max_lag)
  starting_vector <- 1 : max_lag
  
  for (i in (1 : (series_length - max_lag))) {
    
    dataset_predictions[i,] <- series[starting_vector + (i - 1)]
    
  }
  
  return(dataset_predictions)
  
}


prediction_distance_in_sample <- function(series_test, series_matrix, vector_coefs, mean_vector) {
  
  predictions <- series_matrix %*% t(t(vector_coefs)) + t(t(mean_vector))
  return(mean(abs(series_test - predictions)))
  
}





f_global_model_function <- function(series, u, max_lag = 1, m = 2, ...) {
  
  
  # Creating the vector of weights
  
  lengths <- unlist(lapply(series, length))
  vector_weights_prev <- rep(u^m, lengths - max_lag)
  vector_weights <- vector_weights_prev/sum(vector_weights_prev)
  
  # Creating the lag-embedded matrix
  
  l <- length(series)
  
  list_matrices <- list()
  
  for (i in 1 : l) {
    series_length <- length(series[[i]])
    list_matrices[[i]] <- matrix(0, nrow = series_length - max_lag, ncol = max_lag + 1)
    
    for (j in 1 : max_lag) {
      
      list_matrices[[i]][,j] <- series[[i]][j : (j + series_length -  max_lag - 1)]
      
    }
    
    list_matrices[[i]][, (max_lag + 1)] <- series[[i]][-c(1 : max_lag)]
    
  }
  
  matrix_total <- do.call('rbind', list_matrices)
  
  
  # Constructing a weighted least squares regression model over the lag-embedded matrix
  # the lag-embedded matrix
  
  last_col <- ncol(matrix_total)
  train_explanatory <- data.frame(matrix_total[, 1 : (last_col - 1)])
  train_response <- matrix_total[, last_col]
  if (ncol(train_explanatory) == 1) {colnames(train_explanatory) <- 'X1'}
  
  df_wls <- data.frame(cbind(train_explanatory, train_response))
  
  model <- lm(train_response~., data = df_wls, weights = vector_weights)
  model
  
}