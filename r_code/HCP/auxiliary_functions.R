

library(caret)
library(MLmetrics)


# Auxiliary functions for HCP method with linear models (regression)

rmse_function <- function(x, y, n_series, n_out) {
  
  x_new <- split_vector_chunks(x, n_series, n_out)
  y_new <- split_vector_chunks(y, n_series, n_out)
  lxn <- length(x_new)
  
  errors <- numeric()
  
  for (i in 1 : lxn) {
    
    errors[i] <- RMSE(x_new[[i]], y_new[[i]])
    
  }
  
  sum(errors)
  
}



distance_global <- function(list_1, list_2, n_out = 5) {

  n_predictors <- ncol(list_1[[1]]) - 1
  n_1 <- length(list_1)
  n_2 <- length(list_2)
  n_row_1 <- numeric()
  n_row_2 <- numeric()

  for (i in 1 : n_1) {

    n_row_1[i] <- nrow(list_1[[i]])

  }

  for (i in 1 : n_2) {

    n_row_2[i] <- nrow(list_2[[i]])

  }

  for (i in 1 : n_1) {

    colnames(list_1[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')

  }

  for (i in 1 : n_2) {

    colnames(list_2[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')

  }


  list_1_train <- list()
  list_2_train <- list()
  list_1_test <- list()
  list_2_test <- list()

  for (i in 1 : n_1) {

    list_1_train[[i]] <- data.frame(list_1[[i]][1 : (n_row_1[i] - n_out),])
    list_1_test[[i]] <- data.frame(list_1[[i]][(n_row_1[i] - n_out + 1) : (n_row_1[i]),])

  }

  for (i in 1 : n_2) {

    list_2_train[[i]] <- data.frame(list_2[[i]][1 : (n_row_2[i] - n_out),])
    list_2_test[[i]] <- data.frame(list_2[[i]][(n_row_2[i] - n_out + 1) : (n_row_2[i]),])

  }


  data_1_train <- do.call(rbind, list_1_train)
  data_2_train <- do.call(rbind, list_2_train)
  data_1_test <- do.call(rbind, list_1_test)
  data_2_test <- do.call(rbind, list_2_test)

  data_total_train <- do.call(rbind, c(list_1_train, list_2_train))
  data_total_test <- do.call(rbind, c(list_1_test, list_2_test))

  # Fitting local models and computing local RSS

  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))

  model_1 <- lm(fmla, data = data_1_train)
  model_2 <- lm(fmla, data = data_2_train)

  pred_1 <- predict(model_1, newdata = data_1_test)
  pred_2 <- predict(model_2, newdata = data_2_test)
  pred_1[is.na(pred_1)] <- 0
  pred_2[is.na(pred_2)] <- 0

  error_1 <- rmse_function(pred_1, data_1_test[, (n_predictors + 1)], n_1, n_out)
  error_2 <- rmse_function(pred_2, data_2_test[, (n_predictors + 1)], n_2, n_out)


  # Fitting a global model and computing global RSS

  global_model <- lm(fmla, data = data_total_train)
  pred_global_1 <- predict(global_model, newdata = data_1_test)
  pred_global_2 <- predict(global_model, newdata = data_2_test)
  pred_global_1[is.na(pred_global_1)] <- 0
  pred_global_2[is.na(pred_global_2)] <- 0
  error_global_1 <- rmse_function(pred_global_1, data_1_test[, (n_predictors + 1)], n_1, n_out)
  error_global_2 <- rmse_function(pred_global_2, data_2_test[, (n_predictors + 1)], n_2, n_out)

  local_error <- error_1 + error_2
  global_error <- error_global_1 + error_global_2


  return(1/(local_error - global_error))

}



dis_matrix_global <- function(partition, df_list, n_out = 5) {
  
  n_clusters <- length(unique(partition))
  new_list_df <- list()
  
  for (i in 1 : n_clusters) {
    
    indexes <- which(partition == i)
    new_list_df[[i]] <- df_list[indexes]
    
  }
  
  distance_matrix <- matrix(Inf, n_clusters, n_clusters)
  
  
  for (i in 1 : n_clusters) {
    
    for (j in 1 : n_clusters) {
      
      if (i < j) {
        
        distance_matrix[i, j] <- distance_global(new_list_df[[i]], new_list_df[[j]], n_out = n_out)
        
      }
      
    }
    
  }
  
  return(distance_matrix)
  
}


of <- function(partition, df_list, n_out = 5) {
  
  n_row <- numeric()
  n_data <- length(df_list)
  n_clust <- length(unique(partition))
  n_predictors <- ncol(df_list[[1]]) - 1
  
  for (i in 1 : n_data) {
    
    n_row[i] <- nrow(df_list[[i]])
    
  }
  
  for (i in 1 : n_data) {
    
    colnames(df_list[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
  
  df_list_train <- list()
  df_list_test <- list()
  
  for (i in 1 : n_data) {
    
    df_list_train[[i]] <- data.frame(df_list[[i]][1 : (n_row[i] - n_out),])
    df_list_test[[i]] <- data.frame(df_list[[i]][(n_row[i] - n_out + 1) : (n_row[i]),])
    
  }
  
  gm_list <- list()
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    list_i <- df_list_train[indexes]
    dataset_i <- do.call(rbind, list_i)
    gm_list[[i]] <- lm(fmla, data = dataset_i)
    
  }
  
  
  error_i <- numeric()
  
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    l_i <- length(indexes)
    list_test_i <- df_list_test[indexes]
    dataset_test_i <- do.call(rbind, list_test_i)
    pred_i <- predict(gm_list[[i]], newdata = dataset_test_i)
    pred_i[is.na(pred_i)] <- 0
    error_i[i] <- rmse_function(pred_i, dataset_test_i[, (n_predictors + 1)], l_i, n_out)
    
  }
  
  sum(error_i)
  
  
}


update_partition <- function(partition, distance_matrix) {
  
  min_index <- which(distance_matrix == min(distance_matrix), arr.ind = TRUE)
  index_1 <- which(partition == min_index[1])
  index_2 <- which(partition == min_index[2])
  partition[c(index_1, index_2)] <- min(partition[c(index_1, index_2)])
  indexes <- which(partition >  min_index[2])
  partition[indexes] <- partition[indexes] - 1
  
  return(partition)
  
}



# Auxiliary functions for HCP method with knn models (regression)




distance_global_knn <- function(list_1, list_2, n_out = 5, k = 5) {
  
  n_predictors <- ncol(list_1[[1]]) - 1
  n_1 <- length(list_1)
  n_2 <- length(list_2)
  n_row_1 <- numeric()
  n_row_2 <- numeric()
  
  for (i in 1 : n_1) {
    
    n_row_1[i] <- nrow(list_1[[i]])
    
  }
  
  for (i in 1 : n_2) {
    
    n_row_2[i] <- nrow(list_2[[i]])
    
  }
  
  for (i in 1 : n_1) {
    
    colnames(list_1[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  for (i in 1 : n_2) {
    
    colnames(list_2[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  
  list_1_train <- list()
  list_2_train <- list()
  list_1_test <- list()
  list_2_test <- list()
  
  for (i in 1 : n_1) {
    
    list_1_train[[i]] <- data.frame(list_1[[i]][1 : (n_row_1[i] - n_out),])
    list_1_test[[i]] <- data.frame(list_1[[i]][(n_row_1[i] - n_out + 1) : (n_row_1[i]),])
    
  }
  
  for (i in 1 : n_2) {
    
    list_2_train[[i]] <- data.frame(list_2[[i]][1 : (n_row_2[i] - n_out),])
    list_2_test[[i]] <- data.frame(list_2[[i]][(n_row_2[i] - n_out + 1) : (n_row_2[i]),])
    
  }
  
  
  data_1_train <- do.call(rbind, list_1_train)
  data_2_train <- do.call(rbind, list_2_train)
  data_1_test <- do.call(rbind, list_1_test)
  data_2_test <- do.call(rbind, list_2_test)
  
  data_total_train <- do.call(rbind, c(list_1_train, list_2_train))
  data_total_test <- do.call(rbind, c(list_1_test, list_2_test))
  
  # Fitting local models and computing local RSS
  
  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
  
  trc <- trainControl(method = 'none')
  tg = expand.grid(k = k)
  model_1 <- train(fmla, data_1_train, method = 'knn', trControl = trc, tuneGrid = tg)
  model_2 <- train(fmla, data_2_train, method = 'knn', trControl = trc, tuneGrid = tg)
  
  pred_1 <- predict(model_1, newdata = data_1_test)
  pred_2 <- predict(model_2, newdata = data_2_test)
  pred_1[is.na(pred_1)] <- 0
  pred_2[is.na(pred_2)] <- 0
  
  error_1 <- rmse_function(pred_1, data_1_test[, (n_predictors + 1)], n_1, n_out)
  error_2 <- rmse_function(pred_2, data_2_test[, (n_predictors + 1)], n_2, n_out)
  
  
  # Fitting a global model and computing global RSS
  
  global_model <- train(fmla, data_total_train, method = 'knn', trControl = trc, tuneGrid = tg)
  pred_global_1 <- predict(global_model, newdata = data_1_test)
  pred_global_2 <- predict(global_model, newdata = data_2_test)
  pred_global_1[is.na(pred_global_1)] <- 0
  pred_global_2[is.na(pred_global_2)] <- 0
  error_global_1 <- rmse_function(pred_global_1, data_1_test[, (n_predictors + 1)], n_1, n_out)
  error_global_2 <- rmse_function(pred_global_2, data_2_test[, (n_predictors + 1)], n_2, n_out)
  
  local_error <- error_1 + error_2
  global_error <- error_global_1 + error_global_2
  
  
  return(1/(local_error - global_error))
  
}



dis_matrix_global_knn <- function(partition, df_list, n_out = 5, k = 5) {
  
  n_clusters <- length(unique(partition))
  new_list_df <- list()
  
  for (i in 1 : n_clusters) {
    
    indexes <- which(partition == i)
    new_list_df[[i]] <- df_list[indexes]
    
  }
  
  distance_matrix <- matrix(Inf, n_clusters, n_clusters)
  
  
  for (i in 1 : n_clusters) {
    
    for (j in 1 : n_clusters) {
      
      if (i < j) {
        
        distance_matrix[i, j] <- distance_global_knn(new_list_df[[i]], new_list_df[[j]], n_out = n_out, k = k)
        
      }
      
    }
    
  }
  
  return(distance_matrix)
  
}


of_knn <- function(partition, df_list, n_out = 5, k = 5) {
  
  n_row <- numeric()
  n_data <- length(df_list)
  n_clust <- length(unique(partition))
  n_predictors <- ncol(df_list[[1]]) - 1
  tg = expand.grid(k = k)
  trc <- trainControl(method = 'none')
  
  for (i in 1 : n_data) {
    
    n_row[i] <- nrow(df_list[[i]])
    
  }
  
  for (i in 1 : n_data) {
    
    colnames(df_list[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
  
  df_list_train <- list()
  df_list_test <- list()
  
  for (i in 1 : n_data) {
    
    df_list_train[[i]] <- data.frame(df_list[[i]][1 : (n_row[i] - n_out),])
    df_list_test[[i]] <- data.frame(df_list[[i]][(n_row[i] - n_out + 1) : (n_row[i]),])
    
  }
  
  gm_list <- list()
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    list_i <- df_list_train[indexes]
    dataset_i <- do.call(rbind, list_i)
    gm_list[[i]] <- train(fmla, data = dataset_i, method = 'knn', trControl = trc, tuneGrid = tg)
    
  }
  
  
  error_i <- numeric()
  
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    l_i <- length(indexes)
    list_test_i <- df_list_test[indexes]
    dataset_test_i <- do.call(rbind, list_test_i)
    pred_i <- predict(gm_list[[i]], newdata = dataset_test_i)
    pred_i[is.na(pred_i)] <- 0
    error_i[i] <- rmse_function(pred_i, dataset_test_i[, (n_predictors + 1)], l_i, n_out)
    
  }
  
  sum(error_i)
  
  
}


update_partition_knn <- function(partition, distance_matrix) {
  
  min_index <- which(distance_matrix == min(distance_matrix), arr.ind = TRUE)
  index_1 <- which(partition == min_index[1])
  index_2 <- which(partition == min_index[2])
  partition[c(index_1, index_2)] <- min(partition[c(index_1, index_2)])
  indexes <- which(partition >  min_index[2])
  partition[indexes] <- partition[indexes] - 1
  
  return(partition)
  
}



# Auxiliary functions for HCP method with SVR models (regression)


distance_global_svm <- function(list_1, list_2, n_out = 5, k = 5) {
  
  n_predictors <- ncol(list_1[[1]]) - 1
  n_1 <- length(list_1)
  n_2 <- length(list_2)
  n_row_1 <- numeric()
  n_row_2 <- numeric()
  
  for (i in 1 : n_1) {
    
    n_row_1[i] <- nrow(list_1[[i]])
    
  }
  
  for (i in 1 : n_2) {
    
    n_row_2[i] <- nrow(list_2[[i]])
    
  }
  
  for (i in 1 : n_1) {
    
    colnames(list_1[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  for (i in 1 : n_2) {
    
    colnames(list_2[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  
  list_1_train <- list()
  list_2_train <- list()
  list_1_test <- list()
  list_2_test <- list()
  
  for (i in 1 : n_1) {
    
    list_1_train[[i]] <- data.frame(list_1[[i]][1 : (n_row_1[i] - n_out),])
    list_1_test[[i]] <- data.frame(list_1[[i]][(n_row_1[i] - n_out + 1) : (n_row_1[i]),])
    
  }
  
  for (i in 1 : n_2) {
    
    list_2_train[[i]] <- data.frame(list_2[[i]][1 : (n_row_2[i] - n_out),])
    list_2_test[[i]] <- data.frame(list_2[[i]][(n_row_2[i] - n_out + 1) : (n_row_2[i]),])
    
  }
  
  
  data_1_train <- do.call(rbind, list_1_train)
  data_2_train <- do.call(rbind, list_2_train)
  data_1_test <- do.call(rbind, list_1_test)
  data_2_test <- do.call(rbind, list_2_test)
  
  data_total_train <- do.call(rbind, c(list_1_train, list_2_train))
  data_total_test <- do.call(rbind, c(list_1_test, list_2_test))
  
  # Fitting local models and computing local RSS
  
  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
  
  trc <- trainControl(method = 'none')
  tg = expand.grid(k = k)
  model_1 <- train(fmla, data_1_train, method = 'svmRadial', trControl = trc)
  model_2 <- train(fmla, data_2_train, method = 'svmRadial', trControl = trc)
  
  pred_1 <- predict(model_1, newdata = data_1_test)
  pred_2 <- predict(model_2, newdata = data_2_test)
  pred_1[is.na(pred_1)] <- 0
  pred_2[is.na(pred_2)] <- 0
  
  error_1 <- rmse_function(pred_1, data_1_test[, (n_predictors + 1)], n_1, n_out)
  error_2 <- rmse_function(pred_2, data_2_test[, (n_predictors + 1)], n_2, n_out)
  
  
  # Fitting a global model and computing global RSS
  
  global_model <- train(fmla, data_total_train, method = 'svmRadial', trControl = trc)
  pred_global_1 <- predict(global_model, newdata = data_1_test)
  pred_global_2 <- predict(global_model, newdata = data_2_test)
  pred_global_1[is.na(pred_global_1)] <- 0
  pred_global_2[is.na(pred_global_2)] <- 0
  error_global_1 <- rmse_function(pred_global_1, data_1_test[, (n_predictors + 1)], n_1, n_out)
  error_global_2 <- rmse_function(pred_global_2, data_2_test[, (n_predictors + 1)], n_2, n_out)
  
  local_error <- error_1 + error_2
  global_error <- error_global_1 + error_global_2
  
  
  return(1/(local_error - global_error))
  
}


dis_matrix_global_svm <- function(partition, df_list, n_out = 5, k = 5) {
  
  n_clusters <- length(unique(partition))
  new_list_df <- list()
  
  for (i in 1 : n_clusters) {
    
    indexes <- which(partition == i)
    new_list_df[[i]] <- df_list[indexes]
    
  }
  
  distance_matrix <- matrix(Inf, n_clusters, n_clusters)
  
  
  for (i in 1 : n_clusters) {
    
    for (j in 1 : n_clusters) {
      
      if (i < j) {
        
        distance_matrix[i, j] <- distance_global_svm(new_list_df[[i]], new_list_df[[j]], n_out = n_out, k = k)
        
      }
      
    }
    
  }
  
  return(distance_matrix)
  
}


of_svm <- function(partition, df_list, n_out = 5, k = 5) {
  
  n_row <- numeric()
  n_data <- length(df_list)
  n_clust <- length(unique(partition))
  n_predictors <- ncol(df_list[[1]]) - 1
  tg = expand.grid(k = k)
  trc <- trainControl(method = 'none')
  
  for (i in 1 : n_data) {
    
    n_row[i] <- nrow(df_list[[i]])
    
  }
  
  for (i in 1 : n_data) {
    
    colnames(df_list[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
  
  df_list_train <- list()
  df_list_test <- list()
  
  for (i in 1 : n_data) {
    
    df_list_train[[i]] <- data.frame(df_list[[i]][1 : (n_row[i] - n_out),])
    df_list_test[[i]] <- data.frame(df_list[[i]][(n_row[i] - n_out + 1) : (n_row[i]),])
    
  }
  
  gm_list <- list()
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    list_i <- df_list_train[indexes]
    dataset_i <- do.call(rbind, list_i)
    gm_list[[i]] <- train(fmla, data = dataset_i, method = 'svmRadial', trControl = trc)
    
  }
  
  
  error_i <- numeric()
  
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    l_i <- length(indexes)
    list_test_i <- df_list_test[indexes]
    dataset_test_i <- do.call(rbind, list_test_i)
    pred_i <- predict(gm_list[[i]], newdata = dataset_test_i)
    pred_i[is.na(pred_i)] <- 0
    error_i[i] <- rmse_function(pred_i, dataset_test_i[, (n_predictors + 1)], l_i, n_out)
    
  }
  
  sum(error_i)
  
  
}


update_partition_svm <- function(partition, distance_matrix) {
  
  min_index <- which(distance_matrix == min(distance_matrix), arr.ind = TRUE)
  index_1 <- which(partition == min_index[1])
  index_2 <- which(partition == min_index[2])
  partition[c(index_1, index_2)] <- min(partition[c(index_1, index_2)])
  indexes <- which(partition >  min_index[2])
  partition[indexes] <- partition[indexes] - 1
  
  return(partition)
  
}



# Auxiliary functions for HCP method with LDA models (regression)


cross_entropy <- function(p, phat){
  x <- 0
  for (i in 1:length(p)){
    
    phat[i] <- phat[i] + 0.00001
    x <- x + (p[i] * log(phat[i]))
    
  }
  return(-x)
}


cross_entropy_total <- function(p_vector, phat_dataset) {
  
  classes <- sort(unique(p_vector))
  n_classes <- length(classes)
  l_vector <- length(p_vector)
  
  matrix_p <- matrix(0, nrow = l_vector, ncol = n_classes)
  
  for (i in 1 : l_vector) {
    
    for (j in 1 : n_classes) {
      
      if (p_vector[i] == j) {
        
        matrix_p[i, j] <- 1
        
      }
      
    }
    
  }
  
  vector_entropy <- numeric()
  
  
  for (i in 1 : l_vector) {
    
    vector_entropy[i] <- cross_entropy(matrix_p[i,], phat_dataset[i,])
    
  } 
  
  return(sum(vector_entropy))
  
}


cross_entropy_total_function <- function(x, y, n_series, n_out) {
  
  x_new <- split_vector_chunks(x, n_series, n_out)
  y_new <- split_matrix_chunks(y, n_series, n_out)
  lxn <- length(x_new)
  
  errors <- numeric()
  
  for (i in 1 : lxn) {
    
    errors[i] <- cross_entropy_total(x_new[[i]], y_new[[i]])
    
  }
  
  sum(errors)
  
}

distance_global_lda <- function(list_1, list_2, n_out = 5) {
  
  
  n_predictors <- ncol(list_1[[1]]) - 1
  n_1 <- length(list_1)
  n_2 <- length(list_2)
  n_row_1 <- numeric()
  n_row_2 <- numeric()
  
  for (i in 1 : n_1) {
    
    n_row_1[i] <- nrow(list_1[[i]])
    
  }
  
  for (i in 1 : n_2) {
    
    n_row_2[i] <- nrow(list_2[[i]])
    
  }
  
  for (i in 1 : n_1) {
    
    colnames(list_1[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  for (i in 1 : n_2) {
    
    colnames(list_2[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  
  list_1_train <- list()
  list_2_train <- list()
  list_1_test <- list()
  list_2_test <- list()
  
  for (i in 1 : n_1) {
    
    list_1_train[[i]] <- data.frame(list_1[[i]][1 : (n_row_1[i] - n_out),])
    list_1_test[[i]] <- data.frame(list_1[[i]][(n_row_1[i] - n_out + 1) : (n_row_1[i]),])
    
  }
  
  for (i in 1 : n_2) {
    
    list_2_train[[i]] <- data.frame(list_2[[i]][1 : (n_row_2[i] - n_out),])
    list_2_test[[i]] <- data.frame(list_2[[i]][(n_row_2[i] - n_out + 1) : (n_row_2[i]),])
    
  }
  
  
  data_1_train <- do.call(rbind, list_1_train)
  data_2_train <- do.call(rbind, list_2_train)
  data_1_test <- do.call(rbind, list_1_test)
  data_2_test <- do.call(rbind, list_2_test)
  
  data_total_train <- do.call(rbind, c(list_1_train, list_2_train))
  data_total_test <- do.call(rbind, c(list_1_test, list_2_test))
  
  # Fitting local models and computing local RSS
  
  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
  
  model_1 <- lda(fmla, data = data_1_train)
  model_2 <- lda(fmla, data = data_2_train)
  
  pred_1 <- predict(model_1, newdata = data_1_test)$posterior
  pred_2 <- predict(model_2, newdata = data_2_test)$posterior
  
  error_1 <- cross_entropy_total_function(data_1_test[, (n_predictors + 1)], pred_1, n_1, n_out)
  error_2 <- cross_entropy_total_function(data_2_test[, (n_predictors + 1)], pred_2, n_2, n_out)
  
  
  # Fitting a global model and computing global RSS
  
  global_model <- lda(fmla, data = data_total_train)
  pred_global_1 <- predict(global_model, newdata = data_1_test)$posterior
  pred_global_2 <- predict(global_model, newdata = data_2_test)$posterior
  error_global_1 <- cross_entropy_total_function(data_1_test[, (n_predictors + 1)], pred_global_1, n_1, n_out)
  error_global_2 <- cross_entropy_total_function(data_2_test[, (n_predictors + 1)], pred_global_2, n_2, n_out)
  
  local_error <- error_1 + error_2
  global_error <- error_global_1 + error_global_2
  
  if (is.na(global_error < local_error)) {
    
    global_error <- rnorm(1)
    local_error <- rnorm(1)
    
  }
  
  if (global_error < local_error) {
  
  return(1/(local_error - global_error))
    
  } else {
    
    return(Inf)
    
  }
  
}



dis_matrix_global_lda <- function(partition, df_list, n_out = 5) {
  
  n_clusters <- length(unique(partition))
  new_list_df <- list()
  
  for (i in 1 : n_clusters) {
    
    indexes <- which(partition == i)
    new_list_df[[i]] <- df_list[indexes]
    
  }
  
  distance_matrix <- matrix(Inf, n_clusters, n_clusters)
  
  
  for (i in 1 : n_clusters) {
    
    for (j in 1 : n_clusters) {
      
      if (i < j) {
        
        distance_matrix[i, j] <- distance_global_lda(new_list_df[[i]], new_list_df[[j]], n_out = n_out)
        
      }
      
    }
    
  }
  
  return(distance_matrix)
  
}


oof_lda <- function(partition, df_list, n_out = 5) {
  
  n_row <- numeric()
  n_data <- length(df_list)
  n_clust <- length(unique(partition))
  n_predictors <- ncol(df_list[[1]]) - 1
  
  for (i in 1 : n_data) {
    
    n_row[i] <- nrow(df_list[[i]])
    
  }
  
  for (i in 1 : n_data) {
    
    colnames(df_list[[i]]) <- c(paste0("x", 1 : n_predictors), 'y')
    
  }
  
  xnam <- paste0("x", 1 : n_predictors)
  fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
  
  df_list_train <- list()
  df_list_test <- list()
  
  for (i in 1 : n_data) {
    
    df_list_train[[i]] <- data.frame(df_list[[i]][1 : (n_row[i] - n_out),])
    df_list_test[[i]] <- data.frame(df_list[[i]][(n_row[i] - n_out + 1) : (n_row[i]),])
    
  }
  
  gm_list <- list()
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    list_i <- df_list_train[indexes]
    dataset_i <- do.call(rbind, list_i)
    gm_list[[i]] <- lda(fmla, data = dataset_i)
    
  }
  
  
  error_i <- numeric()
  
  
  for (i in 1 : n_clust) {
    
    indexes <- which(partition == i)
    l_i <- length(indexes)
    list_test_i <- df_list_test[indexes]
    dataset_test_i <- do.call(rbind, list_test_i)
    pred_i <- predict(gm_list[[i]], newdata = dataset_test_i)$posterior
    error_i[i] <- cross_entropy_total_function(dataset_test_i[, (n_predictors + 1)], pred_i, l_i, n_out)
    
  }
  
  sum(error_i)
  
  
}


update_partition_lda <- function(partition, distance_matrix) {
  
  min_index <- which(distance_matrix == min(distance_matrix), arr.ind = TRUE)
  index_1 <- which(partition == min_index[1])
  index_2 <- which(partition == min_index[2])
  partition[c(index_1, index_2)] <- min(partition[c(index_1, index_2)])
  indexes <- which(partition >  min_index[2])
  partition[indexes] <- partition[indexes] - 1
  
  return(partition)
  
}




