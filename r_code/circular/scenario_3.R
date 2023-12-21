

cluster_1 <- list()
cluster_2 <- list()
cluster_3 <- list()
grid_length <- 500
n_series <- 5
K <- 3
radius <- 1.2
grid_radius <- seq(0.5, 2, by = 0.1)
l_grid_radius <- length(grid_radius)
lags <- c(1, 2)
levels <- c(0.1, 0.5, 0.9)
l_lags <- length(lags)
l_levels <- length(levels)
selected_r <- numeric() 
grid_f_p <- 1.5
transformation <- transformation_1
quantile_function <- quantile.circular

ground_truth <- c(rep(1, n_series), rep(2, n_series), rep(3, n_series))

ari_vector_fl <- numeric()
ari_vector_q <- numeric()
ari_vector_j <- numeric()
ari_vector_noncircular <- numeric()
j_vector_fl <- numeric()
j_vector_q <- numeric()
j_vector_j <- numeric()
j_vector_noncircular <- numeric()
trials <- 200
matrix_results <- matrix(0, 10, 8)

set.seed(1234567)

count <- 1

for (i1 in grid_length) {
  
  for (i2 in grid_f_p) {
    
    for (j in 1 : trials) {
      
      for (i in 1 : n_series) {
        
        cluster_1[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.4, 0.4), beta = c(0.05, 0.05), n = i1), 
                                         garch.sim(alpha = c(0.1, 0.1, 0.1), beta = c(0.6, -0.3), n = i1))
        cluster_1[[i]][is.na(cluster_1[[i]])] <- 0
        cluster_2[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.05, 0.05), beta = c(0.4, 0.4), n = i1), 
                                         garch.sim(alpha = c(0.1, 0.1, 0.1), beta = c(0.6, -0.3), n = i1))
        cluster_2[[i]][is.na(cluster_2[[i]])] <- 0
        cluster_3[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.05, 0.4), beta = c(0.4, 0.05), n = i1), 
                                         garch.sim(alpha = c(0.1, 0.5, 0.5), beta = c(0.2, -0.7), n = i1))
        cluster_3[[i]][is.na(cluster_3[[i]])] <- 0
        
        
      }
      
      cluster <- c(cluster_1, cluster_2, cluster_3)
      l_cluster <- length(cluster)
      
      
      features_fl <- list()
      
      for (i in 1 : l_cluster) {
        
        features_fl[[i]] <- autocorrelations_fl(cluster[[i]], lags = lags)
        features_fl[[i]][is.na(features_fl[[i]])] <- 0
        
      }
      
      
      features_q_list <- list()
      
      for (k in 1 : l_grid_radius) {
        
        features_q <- list()
        
        for (i in 1 : l_cluster) {
          
          features_q[[i]] <- autocorrelations_q(cluster[[i]], levels = levels, lags = lags, radius = grid_radius[k], quantile_function = quantile_function)
          features_q[[i]][is.na(features_q[[i]])] <- 0
          
        }
        
        features_q_list[[k]] <- features_q
        
      }
      
      xie_beni_vector <- numeric()
      
      for (k in 1 : l_grid_radius) {
        
        matrix_features_q <- list_to_matrix(features_q_list[[k]])
        dis_matrix_q <- (1/(4 * l_lags * l_levels^2)) * dist(matrix_features_q)^2
        clustering_q <- FKM(matrix_features_q, k = 3, m = i2, maxit = 1000)
        xie_beni_vector[k] <- XB(clustering_q$Xca, clustering_q$U, clustering_q$H)
        
        
      }
      
      
      selected_r[j] <- grid_radius[which.min(xie_beni_vector)]
      
      features_j <- list()
      
      for (i in 1 : l_cluster) {
        
        features_j[[i]] <- autocorrelations_j(cluster[[i]], lags = lags)
        features_j[[i]][is.na(features_j[[i]])] <- 0
        
      }
      
      
      features_noncircular <- list()
      
      for (i in 1 : l_cluster) {
        
        features_noncircular[[i]] <- autocorrelations_noncircular(cluster[[i]], levels = levels, lags = lags)
        features_noncircular[[i]][is.na(features_noncircular[[i]])] <- 0
        
      }
      
      
      
      
      matrix_features_fl <- list_to_matrix(features_fl)
      matrix_features_q <- list_to_matrix(features_q_list[[which.min(xie_beni_vector)]])
      matrix_features_j <- list_to_matrix(features_j)
      matrix_features_noncircular <- list_to_matrix(features_noncircular)
      
      
      auxiliary_vector_q_ari <- numeric()
      auxiliary_vector_fl_ari <- numeric()
      auxiliary_vector_j_ari <- numeric()
      auxiliary_vector_noncircular_ari <- numeric()
      auxiliary_vector_q_j <- numeric()
      auxiliary_vector_fl_j <- numeric()
      auxiliary_vector_j_j <- numeric()
      auxiliary_vector_noncircular_j <- numeric()
      
      for (k in 1 : 200) {
        
        clustering_fl <- fuzzy_c_medoids((matrix_features_fl), C = K, m = i2, max_iter = 100, dis = s_euclidean_distance)$U
        auxiliary_vector_fl_ari[k] <- ARI.F(ground_truth, clustering_fl)
        auxiliary_vector_fl_j[k] <- JACCARD.F(ground_truth, clustering_fl)
        clustering_q <- fuzzy_c_medoids((matrix_features_q), C = K, m = i2, max_iter = 100, dis = s_euclidean_distance)$U
        auxiliary_vector_q_ari[k] <- ARI.F(ground_truth, clustering_q)
        auxiliary_vector_q_j[k] <- JACCARD.F(ground_truth, clustering_q)
        clustering_j <- fuzzy_c_medoids((matrix_features_j), C = K, m = i2, max_iter = 100, dis = s_euclidean_distance)$U
        auxiliary_vector_j_ari[k] <- ARI.F(ground_truth, clustering_j)
        auxiliary_vector_j_j[k] <- JACCARD.F(ground_truth, clustering_j)
        clustering_noncircular <- fuzzy_c_medoids((matrix_features_noncircular), C = K, m = i2, max_iter = 100, dis = s_euclidean_distance)$U
        auxiliary_vector_noncircular_ari[k] <- ARI.F(ground_truth, clustering_noncircular)
        auxiliary_vector_noncircular_j[k] <- JACCARD.F(ground_truth, clustering_noncircular)
        
      }
      
      
      ari_vector_fl[j] <- max(auxiliary_vector_fl_ari)
      ari_vector_q[j] <- max(auxiliary_vector_q_ari)
      ari_vector_j[j] <- max(auxiliary_vector_j_ari)
      ari_vector_noncircular[j] <- max(auxiliary_vector_noncircular_ari)
      j_vector_fl[j] <- max(auxiliary_vector_fl_j)
      j_vector_q[j] <- max(auxiliary_vector_q_j)
      j_vector_j[j] <- max(auxiliary_vector_j_j)
      j_vector_noncircular[j] <- max(auxiliary_vector_noncircular_j)
      
      print(j)
      
    }
    
    matrix_results[count,] <- c(mean(ari_vector_fl), mean(ari_vector_j), mean(ari_vector_q), mean(ari_vector_noncircular),
                                mean(j_vector_fl), mean(j_vector_j), mean(j_vector_q), mean(j_vector_noncircular))
    count <- count + 1
    
    
  }
  
  
  
}



round(matrix_results, 4)


# 2DS plot 1


cluster_1 <- list()
cluster_2 <- list()
cluster_3 <- list()
grid_length <- 200
n_series <- 5
K <- 3
radius <- 1.2
grid_radius <- seq(0.5, 2, by = 0.1)
l_grid_radius <- length(grid_radius)
lags <- c(1, 2, 3)
levels <- c(0.1, 0.5, 0.9)
l_lags <- length(lags)
l_levels <- length(levels)
selected_r <- numeric() 
grid_f_p <- 1.6
transformation <- transformation_1

ground_truth <- c(rep(1, n_series), rep(2, n_series), rep(3, n_series))


trials <- 100
matrix_results <- matrix(0, 10, 8)
ground_truth <- c(rep(1, 50), rep(2, 50), rep(3, 50))

set.seed(4)

for (i in 1 : 50) {
  
  cluster_1[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.4, 0.4), beta = c(0.05, 0.05), n = 2000), 
                                   garch.sim(alpha = c(0.1, 0.1, 0.1), beta = c(0.6, -0.3), n = 2000))
  cluster_1[[i]][is.na(cluster_1[[i]])] <- 0
  cluster_2[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.05, 0.05), beta = c(0.4, 0.4), n = 2000), 
                                   garch.sim(alpha = c(0.1, 0.1, 0.1), beta = c(0.6, -0.3), n = 2000))
  cluster_2[[i]][is.na(cluster_2[[i]])] <- 0
  cluster_3[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.05, 0.4), beta = c(0.4, 0.05), n = 2000), 
                                   garch.sim(alpha = c(0.1, 0.5, 0.5), beta = c(0.2, -0.7), n = 2000))
  cluster_3[[i]][is.na(cluster_3[[i]])] <- 0
  
}

cluster <- c(cluster_1, cluster_2, cluster_3)


features_q <- list()

for (i in 1 : 150) {
  
  features_q[[i]] <- autocorrelations_q(cluster[[i]], levels = levels, lags = lags, radius = 1.4, quantile_function = quantile_function)
  features_q[[i]][is.na(features_q[[i]])] <- 0
  
}

matrix_features_q <- list_to_matrix(features_q)
dis_matrix_q <- (1/(4 * l_lags * l_levels^2)) * dist(matrix_features_q)^2
plot_5 <- plot_2d_scaling(dis_matrix_q, cluster_labels = ground_truth)$plot

plot_5_final <- plot_5 + theme(axis.title = element_text(size = 10), axis.text = element_text(size = 10),
                               legend.position = 'none', plot.title = element_text(size = 12)) +
  ggtitle(TeX('Scenario 3 ($\\eta_1$)'))


# 2DS plot 2

transformation <- transformation_2

ground_truth <- c(rep(1, n_series), rep(2, n_series), rep(3, n_series))


trials <- 100
matrix_results <- matrix(0, 10, 8)
ground_truth <- c(rep(1, 50), rep(2, 50), rep(3, 50))

set.seed(4)

for (i in 1 : 50) {
  
  cluster_1[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.4, 0.4), beta = c(0.05, 0.05), n = 2000), 
                                   garch.sim(alpha = c(0.1, 0.1, 0.1), beta = c(0.6, -0.3), n = 2000))
  cluster_1[[i]][is.na(cluster_1[[i]])] <- 0
  cluster_2[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.05, 0.05), beta = c(0.4, 0.4), n = 2000), 
                                   garch.sim(alpha = c(0.1, 0.1, 0.1), beta = c(0.6, -0.3), n = 2000))
  cluster_2[[i]][is.na(cluster_2[[i]])] <- 0
  cluster_3[[i]] <- transformation(garch.sim(alpha = c(0.1, 0.05, 0.4), beta = c(0.4, 0.05), n = 2000), 
                                   garch.sim(alpha = c(0.1, 0.5, 0.5), beta = c(0.2, -0.7), n = 2000))
  cluster_3[[i]][is.na(cluster_3[[i]])] <- 0
  
}

cluster <- c(cluster_1, cluster_2, cluster_3)


features_q <- list()

for (i in 1 : 150) {
  
  features_q[[i]] <- autocorrelations_q(cluster[[i]], levels = levels, lags = lags, radius = 1, quantile_function = quantile_function)
  features_q[[i]][is.na(features_q[[i]])] <- 0
  
}

matrix_features_q <- list_to_matrix(features_q)
dis_matrix_q <- (1/(4 * l_lags * l_levels^2)) * dist(matrix_features_q)^2
plot_6 <- plot_2d_scaling(dis_matrix_q, cluster_labels = ground_truth)$plot

plot_6_final <- plot_6 + theme(axis.title = element_text(size = 10), axis.text = element_text(size = 10),
                               legend.position = 'none', plot.title = element_text(size = 12)) +
  ggtitle(TeX('Scenario 3 ($\\eta_2$)'))


plot_total <- grid.arrange(plot_1_final, plot_2_final, plot_3_final,
                           plot_4_final, plot_5_final, plot_6_final, ncol = 2)

