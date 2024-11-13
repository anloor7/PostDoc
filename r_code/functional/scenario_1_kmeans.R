

cluster_1 <- list()
cluster_2 <- list()
cluster_3 <- list()
cluster_4 <- list()
time_f_1 <- numeric()
time_k_m_1 <- numeric()
time_k_i_1 <- numeric()
time_tsy_1 <- numeric()
time_acff_1 <- numeric()
time_acffs_1 <- numeric()
time_f_2 <- numeric()
time_k_m_2 <- numeric()
time_k_i_2 <- numeric()
time_tsy_2 <- numeric()
time_acff_2 <- numeric()
time_acffs_2 <- numeric()
trials <- 200
series_length <- 600
vector_fp <- c(1.2, 1.4, 1.6, 1.8, 2)
l_fp <- length(vector_fp)
K <- 4
table_results <- matrix(0, nrow = 5, ncol = 8)

vector_ari_auxiliary <- numeric()
vector_ari_auxiliary_k_m <- numeric()
vector_ari_auxiliary_k_i <- numeric()
vector_ari_auxiliary_acff <- numeric()
vector_ari_auxiliary_acffs <- numeric()
vector_j_auxiliary <- numeric()
vector_j_auxiliary_k_m <- numeric()
vector_j_auxiliary_k_i <- numeric()
vector_j_auxiliary_acff <- numeric()
vector_j_auxiliary_acffs <- numeric()

vector_ari <- matrix(0, l_fp, trials)
vector_ari_k_m <- matrix(0, l_fp, trials)
vector_ari_k_i <- matrix(0, l_fp, trials)
vector_ari_acff <- matrix(0, l_fp, trials)
vector_ari_acffs <- matrix(0, l_fp, trials)
vector_j <- matrix(0, l_fp, trials)
vector_j_k_m <- matrix(0, l_fp, trials)
vector_j_k_i <- matrix(0, l_fp, trials)
vector_j_acff <- matrix(0, l_fp, trials)
vector_j_acffs <- matrix(0, l_fp, trials)

set.seed(1234)

ground_truth <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5))

set.seed(1234)

for (j in 1 : trials) {
  
  for (i in 1 : 5) {
    
    cluster_1[[i]] <- hanlin_linear(l = series_length, p = 100, 1, -0.3, 0.1)
    cluster_2[[i]] <- hanlin_linear(l = series_length, p = 100, 1, 0.3, 0.3)
    cluster_3[[i]] <- hanlin_linear_2(l = series_length, p = 100, -0.4, 0.5, -0.3, 0.5)
    cluster_4[[i]] <- hanlin_linear_2(l = series_length, p = 100, 0.4, 0.7, 0.3, 0.7)
    
  }
  
  
  cluster <- c(cluster_1, cluster_2, cluster_3, cluster_4)
  
  start_time <- Sys.time()
  
  feature_matrix <- lapply(cluster, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1, 2))
  feature_dataset <- list_to_matrix(feature_matrix)
  feature_dataset[is.na(feature_dataset)] <- 0
  
  end_time <- Sys.time()
  
  time_f_1[j] <- end_time - start_time
  
  start_time <- Sys.time()
  
  feature_matrix_k_m <- lapply(cluster, aucors_kendall_m, lags = c(1, 2))
  feature_dataset_k_m <- list_to_matrix(feature_matrix_k_m)
  feature_dataset_k_m[is.na(feature_dataset_k_m)] <- 0
  
  end_time <- Sys.time()
  
  time_k_m_1[j] <- end_time - start_time
  
  start_time <- Sys.time()
  
  feature_matrix_k_i <- lapply(cluster, aucors_kendall_i, lags = c(1, 2))
  feature_dataset_k_i <- list_to_matrix(feature_matrix_k_i)
  feature_dataset_k_i[is.na(feature_dataset_k_i)] <- 0
  
  end_time <- Sys.time()
  
  time_k_i_1[j] <- end_time - start_time
  
  start_time <- Sys.time()
  
  feature_matrix_acff <- lapply(cluster, aucors_acff, max_lag = 2)
  feature_dataset_acff <- list_to_matrix(feature_matrix_acff)
  feature_dataset_acff[is.na(feature_dataset_acff)] <- 0
  
  end_time <- Sys.time()
  
  time_acff_1[j] <- end_time - start_time
  
  start_time <- Sys.time()
  
  feature_matrix_acffs <- lapply(cluster, compute_acffs_scenario_1)
  feature_dataset_acffs <- list_to_matrix(feature_matrix_acffs)
  feature_dataset_acffs[is.na(feature_dataset_acffs)] <- 0
  
  end_time <- Sys.time()
  
  time_acffs_1[j] <- end_time - start_time
  
  
  for (k1 in 1 : l_fp) {
    
    start_time <- Sys.time()
    
    for (k2 in 1 : 200) {
      
      cl <- FKM(X = feature_dataset, m = vector_fp[k1], k = K)$U
      
      
      vector_ari_auxiliary[k2] <- ARI.F(ground_truth, cl)
      vector_j_auxiliary[k2] <- JACCARD.F(ground_truth, cl)
      
      
    }
    
    end_time <- Sys.time()
    
    time_f_2[j] <- end_time - start_time
    
    start_time <- Sys.time()
    
    for (k2 in 1 : 200) {
      
      
      cl_k_m <- FKM(X = feature_dataset_k_m, m = vector_fp[k1], k = K)$U
      
      
      
      vector_ari_auxiliary_k_m[k2] <- ARI.F(ground_truth, cl_k_m)
      vector_j_auxiliary_k_m[k2] <- JACCARD.F(ground_truth, cl_k_m)
      
    }
    
    end_time <- Sys.time()
    
    time_k_m_2[j] <- end_time - start_time
    
    start_time <- Sys.time()
    
    for (k2 in 1 : 200) {
      
      
      cl_k_i <- FKM(X = feature_dataset_k_i, m = vector_fp[k1], k = K)$U
      
      
      vector_ari_auxiliary_k_i[k2] <- ARI.F(ground_truth, cl_k_i)
      vector_j_auxiliary_k_i[k2] <- JACCARD.F(ground_truth, cl_k_i)
      
    }
    
    end_time <- Sys.time()
    
    time_k_i_2[j] <- end_time - start_time
    
    start_time <- Sys.time()
    
    for (k2 in 1 : 200) {
      
      cl_acff <- FKM(X = feature_dataset_acff, m = vector_fp[k1], k = K)$U
      
      
      vector_ari_auxiliary_acff[k2] <- ARI.F(ground_truth, cl_acff)
      vector_j_auxiliary_acff[k2] <- JACCARD.F(ground_truth, cl_acff)
      
      
    }
    
    end_time <- Sys.time()
    
    time_acff_2[j] <- end_time - start_time
    
    
    start_time <- Sys.time()
    
    for (k2 in 1 : 200) {
      
      cl_acffs <- FKM(X = feature_dataset_acffs, m = vector_fp[k1], k = K)$U
      
      
      vector_ari_auxiliary_acffs[k2] <- ARI.F(ground_truth, cl_acffs)
      vector_j_auxiliary_acffs[k2] <- JACCARD.F(ground_truth, cl_acffs)
      
      
    }
    
    end_time <- Sys.time()
    
    time_acffs_2[j] <- end_time - start_time
    
    vector_ari[k1, j] <- max(vector_ari_auxiliary)
    vector_ari_k_m[k1, j] <- max(vector_ari_auxiliary_k_m)
    vector_ari_k_i[k1, j] <- max(vector_ari_auxiliary_k_i)
    vector_ari_acff[k1, j] <- max(vector_ari_auxiliary_acff)
    vector_ari_acffs[k1, j] <- max(vector_ari_auxiliary_acffs)
    vector_j[k1, j] <- max(vector_j_auxiliary)
    vector_j_k_m[k1, j] <- max(vector_j_auxiliary_k_m)
    vector_j_k_i[k1, j] <- max(vector_j_auxiliary_k_i)
    vector_j_acff[k1, j] <- max(vector_j_auxiliary_acff)
    vector_j_acffs[k1, j] <- max(vector_j_auxiliary_acffs)
    
    
  }
  
  print(j)
  
}

table_results <- matrix(0, nrow = 5, ncol = 10)

table_results[1, 1] <- rowmeans(vector_ari)[1]
table_results[2, 1] <- rowmeans(vector_ari)[2]
table_results[3, 1] <- rowmeans(vector_ari)[3]
table_results[4, 1] <- rowmeans(vector_ari)[4]
table_results[5, 1] <- rowmeans(vector_ari)[5]

table_results[1, 2] <- rowmeans(vector_ari_acff)[1]
table_results[2, 2] <- rowmeans(vector_ari_acff)[2]
table_results[3, 2] <- rowmeans(vector_ari_acff)[3]
table_results[4, 2] <- rowmeans(vector_ari_acff)[4]
table_results[5, 2] <- rowmeans(vector_ari_acff)[5]

table_results[1, 3] <- rowmeans(vector_ari_acffs)[1]
table_results[2, 3] <- rowmeans(vector_ari_acffs)[2]
table_results[3, 3] <- rowmeans(vector_ari_acffs)[3]
table_results[4, 3] <- rowmeans(vector_ari_acffs)[4]
table_results[5, 3] <- rowmeans(vector_ari_acffs)[5]

table_results[1, 4] <- rowmeans(vector_ari_k_m)[1]
table_results[2, 4] <- rowmeans(vector_ari_k_m)[2]
table_results[3, 4] <- rowmeans(vector_ari_k_m)[3]
table_results[4, 4] <- rowmeans(vector_ari_k_m)[4]
table_results[5, 4] <- rowmeans(vector_ari_k_m)[5]

table_results[1, 5] <- rowmeans(vector_ari_k_i)[1]
table_results[2, 5] <- rowmeans(vector_ari_k_i)[2]
table_results[3, 5] <- rowmeans(vector_ari_k_i)[3]
table_results[4, 5] <- rowmeans(vector_ari_k_i)[4]
table_results[5, 5] <- rowmeans(vector_ari_k_i)[5]


table_results[1, 6] <- rowmeans(vector_j)[1]
table_results[2, 6] <- rowmeans(vector_j)[2]
table_results[3, 6] <- rowmeans(vector_j)[3]
table_results[4, 6] <- rowmeans(vector_j)[4]
table_results[5, 6] <- rowmeans(vector_j)[5]

table_results[1, 7] <- rowmeans(vector_j_acff)[1]
table_results[2, 7] <- rowmeans(vector_j_acff)[2]
table_results[3, 7] <- rowmeans(vector_j_acff)[3]
table_results[4, 7] <- rowmeans(vector_j_acff)[4]
table_results[5, 7] <- rowmeans(vector_j_acff)[5]

table_results[1, 8] <- rowmeans(vector_j_acffs)[1]
table_results[2, 8] <- rowmeans(vector_j_acffs)[2]
table_results[3, 8] <- rowmeans(vector_j_acffs)[3]
table_results[4, 8] <- rowmeans(vector_j_acffs)[4]
table_results[5, 8] <- rowmeans(vector_j_acffs)[5]

table_results[1, 9] <- rowmeans(vector_j_k_m)[1]
table_results[2, 9] <- rowmeans(vector_j_k_m)[2]
table_results[3, 9] <- rowmeans(vector_j_k_m)[3]
table_results[4, 9] <- rowmeans(vector_j_k_m)[4]
table_results[5, 9] <- rowmeans(vector_j_k_m)[5]

table_results[1, 10] <- rowmeans(vector_j_k_i)[1]
table_results[2, 10] <- rowmeans(vector_j_k_i)[2]
table_results[3, 10] <- rowmeans(vector_j_k_i)[3]
table_results[4, 10] <- rowmeans(vector_j_k_i)[4]
table_results[5, 10] <- rowmeans(vector_j_k_i)[5]

table_results[, 1 : 5]
table_results[, 6 : 10]



# 2DS plots

ground_truth <- c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50))

cluster_1 <- list()
cluster_2 <- list()
cluster_3 <- list()
cluster_4 <- list()

series_length <- 1000
set.seed(1234)

for (i in 1 : 50) {
  
  cluster_1[[i]] <- hanlin_linear(l = series_length, p = 100, 1, -0.3, 0.1)
  cluster_2[[i]] <- hanlin_linear(l = series_length, p = 100, 1, 0.3, 0.3)
  cluster_3[[i]] <- hanlin_linear_2(l = series_length, p = 100, -0.4, 0.5, -0.3, 0.5)
  cluster_4[[i]] <- hanlin_linear_2(l = series_length, p = 100, 0.4, 0.7, 0.3, 0.7)
  
}

cluster <- c(cluster_1, cluster_2, cluster_3, cluster_4)


feature_matrix <- lapply(cluster, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1, 2))
dis_matrix_f_1 <- (1/(4 * 2 * 3^2)) * dist(list_to_matrix(feature_matrix))^2

plot_1 <- plot_2d_scaling(dis_matrix_f_1, cluster_labels = ground_truth)$plot + 
  theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10),
        legend.position = 'bottom', plot.title = element_text(size = 13), legend.text = element_text(size = 10)) +
  ylim(c(-0.0060, 0.0090)) + scale_color_discrete(labels = c(TeX('$C_1$'), TeX('$C_2$'), TeX('$C_3$'), TeX('$C_4$')))

shared_legend <- extract_legend(plot_1)



savefig('plot_1_final', width = 12, height = 10, type = 'png', toplines = 0.5)
plot_1 + theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10),
               legend.position = 'bottom', plot.title = element_text(size = 13)) +
  ggtitle(TeX('Scenario 1')) +
  ylim(c(-0.0060, 0.0090))
dev.off()
