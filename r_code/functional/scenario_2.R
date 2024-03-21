

cluster_1 <- list()
cluster_2 <- list()
cluster_3 <- list()
cluster_4 <- list()
trials <- 200
sed <- function(x, y) {as.numeric((x - y) %*% (x - y))}
series_length <- 200
vector_fp <- c(1.2, 1.4, 1.6, 1.8, 2)
l_fp <- length(vector_fp)
K <- 4
table_results <- matrix(0, nrow = 5, ncol = 6)

vector_ari_auxiliary <- numeric()
vector_ari_auxiliary_k_m <- numeric()
vector_ari_auxiliary_k_i <- numeric()
vector_j_auxiliary <- numeric()
vector_j_auxiliary_k_m <- numeric()
vector_j_auxiliary_k_i <- numeric()

vector_ari <- matrix(0, l_fp, trials)
vector_ari_k_m <- matrix(0, l_fp, trials)
vector_ari_k_i <- matrix(0, l_fp, trials)
vector_j <- matrix(0, l_fp, trials)
vector_j_k_m <- matrix(0, l_fp, trials)
vector_j_k_i <- matrix(0, l_fp, trials)


ground_truth <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5))

set.seed(1234)

for (j in 1 : trials) {
  
  
  for (i in 1 : 5) {
    
    cluster_1[[i]] <- hanlin_nonlinear(l = series_length, p = 100, 0.75, 0.5, 0.5)
    cluster_2[[i]] <- hanlin_nonlinear(l = series_length, p = 100, 0.75, 0.9, 0.5)
    cluster_3[[i]] <- fgarch_sim(l = series_length, mean = 0.01, p = 100, 14)
    cluster_4[[i]] <- fgarch_sim(l = series_length, mean = 0.01, p = 100, 15)
    
  }
  
  
  cluster <- c(cluster_1, cluster_2, cluster_3, cluster_4)
  
  feature_matrix <- lapply(cluster, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1))
  feature_dataset <- list_to_matrix(feature_matrix)
  feature_dataset[is.na(feature_dataset)] <- 0
  
  feature_matrix_k_m <- lapply(cluster, aucors_kendall_m, lags = c(1))
  feature_dataset_k_m <- list_to_matrix(feature_matrix_k_m)
  feature_dataset_k_m[is.na(feature_dataset_k_m)] <- 0
  
  feature_matrix_k_i <- lapply(cluster, aucors_kendall_i, lags = c(1))
  feature_dataset_k_i <- list_to_matrix(feature_matrix_k_i)
  feature_dataset_k_i[is.na(feature_dataset_k_i)] <- 0
  
  
  for (k1 in 1 : l_fp) {
    
    for (k2 in 1 : 200) {
      
      cl <- fuzzy_c_medoids(X = feature_dataset, m = vector_fp[k1], C = K, dis = sed)$U
      cl_k_m <- fuzzy_c_medoids(X = cbind(c(feature_dataset_k_m), c(rep(0, 20))), m = vector_fp[k1], C = K, dis = sed)$U
      cl_k_i <- fuzzy_c_medoids(X = cbind(c(feature_dataset_k_i), c(rep(0, 20))), m = vector_fp[k1], C = K, dis = sed)$U
      
      vector_ari_auxiliary[k2] <- ARI.F(ground_truth, cl)
      vector_ari_auxiliary_k_m[k2] <- ARI.F(ground_truth, cl_k_m)
      vector_ari_auxiliary_k_i[k2] <- ARI.F(ground_truth, cl_k_i)
      vector_j_auxiliary[k2] <- JACCARD.F(ground_truth, cl)
      vector_j_auxiliary_k_m[k2] <- JACCARD.F(ground_truth, cl_k_m)
      vector_j_auxiliary_k_i[k2] <- JACCARD.F(ground_truth, cl_k_i)
      
    }
    
    vector_ari[k1, j] <- max(vector_ari_auxiliary)
    vector_ari_k_m[k1, j] <- max(vector_ari_auxiliary_k_m)
    vector_ari_k_i[k1, j] <- max(vector_ari_auxiliary_k_i)
    vector_j[k1, j] <- max(vector_j_auxiliary)
    vector_j_k_m[k1, j] <- max(vector_j_auxiliary_k_m)
    vector_j_k_i[k1, j] <- max(vector_j_auxiliary_k_i)
    
    
  }
  
  print(j)
  
}

table_results[1, 1] <- rowMeans(vector_ari)[1]
table_results[2, 1] <- rowMeans(vector_ari)[2]
table_results[3, 1] <- rowMeans(vector_ari)[3]
table_results[4, 1] <- rowMeans(vector_ari)[4]
table_results[5, 1] <- rowMeans(vector_ari)[5]

table_results[1, 2] <- rowMeans(vector_ari_k_m)[1]
table_results[2, 2] <- rowMeans(vector_ari_k_m)[2]
table_results[3, 2] <- rowMeans(vector_ari_k_m)[3]
table_results[4, 2] <- rowMeans(vector_ari_k_m)[4]
table_results[5, 2] <- rowMeans(vector_ari_k_m)[5]

table_results[1, 3] <- rowMeans(vector_ari_k_i)[1]
table_results[2, 3] <- rowMeans(vector_ari_k_i)[2]
table_results[3, 3] <- rowMeans(vector_ari_k_i)[3]
table_results[4, 3] <- rowMeans(vector_ari_k_i)[4]
table_results[5, 3] <- rowMeans(vector_ari_k_i)[5]


table_results[1, 4] <- rowMeans(vector_j)[1]
table_results[2, 4] <- rowMeans(vector_j)[2]
table_results[3, 4] <- rowMeans(vector_j)[3]
table_results[4, 4] <- rowMeans(vector_j)[4]
table_results[5, 4] <- rowMeans(vector_j)[5]

table_results[1, 5] <- rowMeans(vector_j_k_m)[1]
table_results[2, 5] <- rowMeans(vector_j_k_m)[2]
table_results[3, 5] <- rowMeans(vector_j_k_m)[3]
table_results[4, 5] <- rowMeans(vector_j_k_m)[4]
table_results[5, 5] <- rowMeans(vector_j_k_m)[5]

table_results[1, 6] <- rowMeans(vector_j_k_i)[1]
table_results[2, 6] <- rowMeans(vector_j_k_i)[2]
table_results[3, 6] <- rowMeans(vector_j_k_i)[3]
table_results[4, 6] <- rowMeans(vector_j_k_i)[4]
table_results[5, 6] <- rowMeans(vector_j_k_i)[5]

table_results


# 2DS plots


ground_truth <- c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50))

cluster_1 <- list()
cluster_2 <- list()
cluster_3 <- list()
cluster_4 <- list()

series_length <- 1000

set.seed(1234)

for (i in 1 : 50) {
  
  cluster_1[[i]] <- hanlin_nonlinear(l = series_length, p = 100, 0.75, 0.5, 0.5)
  cluster_2[[i]] <- hanlin_nonlinear(l = series_length, p = 100, 0.75, 0.9, 0.5)
  cluster_3[[i]] <- fgarch_sim(l = series_length, mean = 0.01, p = 100, 14)
  cluster_4[[i]] <- fgarch_sim(l = series_length, mean = 0.01, p = 100, 15)
  
}

cluster <- c(cluster_1, cluster_2, cluster_3, cluster_4)


feature_matrix <- lapply(cluster, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1, 2))
dis_matrix_f_2 <- (1/(4 * 2 * 3^2)) * dist(list_to_matrix(feature_matrix))^2

plot_2 <- plot_2d_scaling(dis_matrix_f, cluster_labels = ground_truth)$plot

plot_2_final <- plot_2 + theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10),
                               legend.position = 'none', plot.title = element_text(size = 12)) +
  ggtitle(TeX('Scenario 2')) +
  ylim(c(-0.0050, 0.0070))

plot_total <- grid.arrange(plot_1_final, plot_2_final, ncol = 2)
  

graph_total <- grid.arrange(
  arrangeGrob(plot_total), nrow = 2, ncol = 1, shared_legend,
  heights = c(40, 5))
