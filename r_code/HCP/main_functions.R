


# Main function for HCP method with linear models (regression). Naive implementation. 


hclust_global <- function(df_list, n_out = 5, max_iter = 100) {
  
  n_data <- length(df_list)
  dis_matrix <- matrix(1, nrow = n_data, ncol = n_data)
  partition <- 1 : n_data
  of_vector <- numeric()
  of_vector[1] <- 10000000000000000000
  of_vector[2] <- 1000000000000000000
  
  
  
  for (i in 1 : max_iter) {
    
    if (sum(diff(of_vector) < 0) == (length(of_vector) - 1)) {
    
    dis_matrix <- dis_matrix_global(partition, df_list, n_out = n_out)
    number <- nrow(dis_matrix)^2 - nrow(dis_matrix) - (nrow(dis_matrix)^2 - nrow(dis_matrix)) * (1/2)
    
    of_vector[i + 2] <- of(partition, df_list, n_out = n_out)
    
    if (sum(dis_matrix < 0) != number) {
      
    dis_matrix[dis_matrix < 0] <- Inf
    partition <- update_partition(partition, dis_matrix)
    
    }
    
    
  }
    
  }
  
  return_list <- list(partition = partition, of = of_vector[-c(1,2)], i = length(of_vector) - 3)
  return(return_list)
  
  
  
}



# Main function for HCP method with knn models (regression). Naive implementation.


hclust_global_knn <- function(df_list, n_out = 5, max_iter = 100, k = 5) {
  
  n_data <- length(df_list)
  dis_matrix <- matrix(1, nrow = n_data, ncol = n_data)
  partition <- 1 : n_data
  of_vector <- numeric()
  of_vector[1] <- 10000000000000000000
  of_vector[2] <- 1000000000000000000
  
  
  
  for (i in 1 : max_iter) {
    
    if (sum(diff(of_vector) < 0) == (length(of_vector) - 1)) {
      
      dis_matrix <- dis_matrix_global_knn(partition, df_list, n_out = n_out, k = k)
      number <- nrow(dis_matrix)^2 - nrow(dis_matrix) - (nrow(dis_matrix)^2 - nrow(dis_matrix)) * (1/2)
      
      of_vector[i + 2] <- of_knn(partition, df_list, n_out = n_out, k = k)
      
      if (sum(dis_matrix < 0) != number) {
        
        dis_matrix[dis_matrix < 0] <- Inf
        partition <- update_partition_knn(partition, dis_matrix)
        
      }
      
      
    }
    
  }
  
  return_list <- list(partition = partition, of = of_vector[-c(1,2)], i = length(of_vector) - 3)
  return(return_list)
  
  
  
}



# Main function for HCP method with SVR models (regression). Naive implementation.


hclust_global_svm <- function(df_list, n_out = 5, max_iter = 100, k = 5) {
  
  n_data <- length(df_list)
  dis_matrix <- matrix(1, nrow = n_data, ncol = n_data)
  partition <- 1 : n_data
  of_vector <- numeric()
  of_vector[1] <- 10000000000000000000
  of_vector[2] <- 1000000000000000000
  
  
  
  for (i in 1 : max_iter) {
    
    if (sum(diff(of_vector) < 0) == (length(of_vector) - 1)) {
      
      dis_matrix <- dis_matrix_global_svm(partition, df_list, n_out = n_out, k = k)
      number <- nrow(dis_matrix)^2 - nrow(dis_matrix) - (nrow(dis_matrix)^2 - nrow(dis_matrix)) * (1/2)
      
      of_vector[i + 2] <- of_svm(partition, df_list, n_out = n_out, k = k)
      
      if (sum(dis_matrix < 0) != number) {
        
        dis_matrix[dis_matrix < 0] <- Inf
        partition <- update_partition_svm(partition, dis_matrix)
        
      }
      
      
    }
    
  }
  
  return_list <- list(partition = partition, of = of_vector[-c(1,2)], i = length(of_vector) - 3)
  return(return_list)
  
  
  
}



# Main function for HCP method with LDA models (classification). Naive implementation.


hclust_global_lda <- function(df_list, n_out = 5, max_iter = 100) {
  
  n_data <- length(df_list)
  dis_matrix <- matrix(1, nrow = n_data, ncol = n_data)
  partition <- 1 : n_data
  of_vector <- numeric()
  of_vector[1] <- 10000000000000000000
  of_vector[2] <- 1000000000000000000
  
  
  
  for (i in 1 : max_iter) {
    
    if (sum(diff(of_vector) < 0) == (length(of_vector) - 1)| is.na(sum(diff(of_vector) < 0) == (length(of_vector) - 1))) {
      
      dis_matrix <- dis_matrix_global_lda(partition, df_list, n_out = n_out)
      number <- nrow(dis_matrix)^2 - nrow(dis_matrix) - (nrow(dis_matrix)^2 - nrow(dis_matrix)) * (1/2)
      
      of_vector[i + 2] <- of_lda(partition, df_list, n_out = n_out)
      
      if (sum(dis_matrix == Inf) != (dim(dis_matrix)[1] * dim(dis_matrix)[2])) {
        
        dis_matrix[dis_matrix < 0] <- Inf
        partition <- update_partition_lda(partition, dis_matrix)
        
      }
      
      
    }
    
  }
  
  return_list <- list(partition = partition, of = of_vector[-c(1,2)], i = length(of_vector) - 3)
  return(return_list)
  
  
  
}
