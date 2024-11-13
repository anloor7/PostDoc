

cluster_1 <- list()
cluster_2 <- list()
trials <- 200
series_length <- 600
vector_fp <- seq(1.1, 7, 0.1)
l_fp <- length(vector_fp)
K <- 2
cutoff <- 0.7
table_results <- matrix(0, nrow = l_fp, ncol = 3)


matrix_f <- matrix(0, l_fp, trials)
matrix_k_m <- matrix(0, l_fp, trials)
matrix_k_i <- matrix(0, l_fp, trials)
matrix_acff <- matrix(0, l_fp, trials)
matrix_acffs <- matrix(0, l_fp, trials)

# set.seed(1234)

for (j in 1 : trials) {
  
  
  for (i in 1 : 5) {
    
    cluster_1[[i]] <- hanlin_nonlinear(l = series_length, p = 100, 0.75, 0.9, 0.5)
    cluster_2[[i]] <- fgarch_sim(l = series_length, p = 100, coef = 14)
    
  }
  
  cluster_3 <- list(hanlin_nonlinear(l = series_length, p = 100, 0.75, 0, 0.5))
  cluster <- c(cluster_1, cluster_2, cluster_3)
  
  feature_matrix <- lapply(cluster, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1))
  feature_dataset <- list_to_matrix(feature_matrix)
  feature_dataset[is.na(feature_dataset)] <- 0
  
  # feature_matrix_k_m <- lapply(cluster, aucors_kendall_m, lags = c(1))
  feature_dataset_k_m <- list_to_matrix(feature_matrix_k_m)
  feature_dataset_k_m[is.na(feature_dataset_k_m)] <- 0
  
  # feature_matrix_k_i <- lapply(cluster, aucors_kendall_i, lags = c(1))
  feature_dataset_k_i <- list_to_matrix(feature_matrix_k_i)
  feature_dataset_k_i[is.na(feature_dataset_k_i)] <- 0
  
  # feature_matrix_acff <- lapply(cluster, aucors_acff, max_lag = 1)
  feature_dataset_acff <- list_to_matrix(feature_matrix_acff)
  feature_dataset_acff[is.na(feature_dataset_acff)] <- 0
  
  # feature_matrix_acffs <- lapply(cluster, compute_acffs_scenario_2)
  feature_dataset_acffs <- list_to_matrix(feature_matrix_acffs)
  feature_dataset_acffs[is.na(feature_dataset_acffs)] <- 0
  
  
  for (k1 in 1 : l_fp) {
    
    
    cl <- FKM(X = feature_dataset, m = vector_fp[k1], k = K)$U
    cl_k_m <- FKM(X = cbind(feature_dataset_k_m, c(rep(0, 11))), m = vector_fp[k1], k = K)$U
    cl_k_i <- FKM(X = cbind(feature_dataset_k_i, c(rep(0, 11))), m = vector_fp[k1],k = K)$U
    cl_acff <- FKM(X = cbind(feature_dataset_acff, c(rep(0, 11))), m = vector_fp[k1], k = K)$U
    cl_acffs <- FKM(X = cbind(feature_dataset_acffs, c(rep(0, 11))), m = vector_fp[k1], k = K)$U
    
    if (sum(cl) != 11) {
      
      cl <- cbind(rep(1, 11), rep(0, 11))
      
    }
    
    
    if (sum(cl_k_m) != 11) {
      
      cl_k_m <- cbind(rep(1, 11), rep(0, 11))
      
    }
    
    
    if (sum(cl_k_i) != 11) {
      
      cl_k_i <- cbind(rep(1, 11), rep(0, 11))
      
    }
    
    
    if (sum(cl_acff) != 11) {
      
      cl_acff <- cbind(rep(1, 11), rep(0, 11))
      
    }
    
    
    if (sum(cl_acffs) != 11) {
      
      cl_acffs <- cbind(rep(1, 11), rep(0, 11))
      
    }
    
    
    max_mem_f <- apply(cl, 1, max)
    max_mem_k_m <- apply(cl_k_m, 1, max)
    max_mem_k_i <- apply(cl_k_i, 1, max)
    max_mem_acff <- apply(cl_acff, 1, max)
    max_mem_acffs <- apply(cl_acffs, 1, max)
    
    
    
    l_1 <- length(unique(fuzzytocrisp(cl)[1 : 5]))
    l_2 <- length(unique(fuzzytocrisp(cl)[6 : 10]))
    l_3 <- length(unique(fuzzytocrisp(cl)[1 : 10]))
    
    
    if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
        sum(max_mem_f[1 : 10] > cutoff) == 10 &
        max_mem_f[11] < cutoff) {
      
      matrix_f[k1, j] <- 1
      
    } else {
      
      matrix_f[k1, j] <- 0
      
    }
    
    
    l_1 <- length(unique(fuzzytocrisp(cl_k_m)[1 : 5]))
    l_2 <- length(unique(fuzzytocrisp(cl_k_m)[6 : 10]))
    l_3 <- length(unique(fuzzytocrisp(cl_k_m)[1 : 10]))
    
    
    if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
        sum(max_mem_k_m[1 : 10] > cutoff) == 10 &
        max_mem_k_m[11] < cutoff) {
      
      matrix_k_m[k1, j] <- 1
      
    } else {
      
      matrix_k_m[k1, j] <- 0
      
    }
    
    
    
    
    l_1 <- length(unique(fuzzytocrisp(cl_k_i)[1 : 5]))
    l_2 <- length(unique(fuzzytocrisp(cl_k_i)[6 : 10]))
    l_3 <- length(unique(fuzzytocrisp(cl_k_i)[1 : 10]))
    
    
    if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
        sum(max_mem_k_i[1 : 10] > cutoff) == 10 &
        max_mem_k_i[11] < cutoff) {
      
      matrix_k_i[k1, j] <- 1
      
    } else {
      
      matrix_k_i[k1, j] <- 0
      
    }
    
    l_1 <- length(unique(fuzzytocrisp(cl_acff)[1 : 5]))
    l_2 <- length(unique(fuzzytocrisp(cl_acff)[6 : 10]))
    l_3 <- length(unique(fuzzytocrisp(cl_acff)[1 : 10]))
    
    
    if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
        sum(max_mem_acff[1 : 10] > cutoff) == 10 &
        max_mem_acff[11] < cutoff) {
      
      matrix_acff[k1, j] <- 1
      
    } else {
      
      matrix_acff[k1, j] <- 0
      
    }
    
    
    l_1 <- length(unique(fuzzytocrisp(cl_acffs)[1 : 5]))
    l_2 <- length(unique(fuzzytocrisp(cl_acffs)[6 : 10]))
    l_3 <- length(unique(fuzzytocrisp(cl_acffs)[1 : 10]))
    
    
    if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
        sum(max_mem_acffs[1 : 10] > cutoff) == 10 &
        max_mem_acffs[11] < cutoff) {
      
      matrix_acffs[k1, j] <- 1
      
    } else {
      
      matrix_acffs[k1, j] <- 0
      
    }
    
    
  }
  
  print(j)
  
}

rate_f <- rowmeans(matrix_f)
rate_k_m <- rowmeans(matrix_k_m)
rate_k_i <- rowmeans(matrix_k_i)
rate_acff <- rowmeans(matrix_acff)
rate_acffs <- rowmeans(matrix_acffs)

mean(rate_f)
mean(rate_k_m)
mean(rate_k_i)
mean(rate_acff)
mean(rate_acffs)

max(rate_f)
max(rate_k_m)
max(rate_k_i)
max(rate_acff)
max(rate_acffs)

trapz(vector_fp, rate_f)
trapz(vector_fp, rate_k_m)
trapz(vector_fp, rate_k_i)
trapz(vector_fp, rate_acff)
trapz(vector_fp, rate_acffs)


# Representing plots of the curves 

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_functional/code')
load('rate_f_4_200_2.RData')
load('rate_acff_4_200_2.RData')
load('rate_acffs_4_200_2.RData')
load('rate_k_m_4_200_2.RData')
load('rate_k_i_4_200_2.RData')


axis_size <- 8
title_size <- 9
vector_fp <- seq(1.1, 7, 0.1)



vector_results = c(rate_f, rate_acff, rate_acffs, rate_k_m, rate_k_i)
vector_f_p_repeated = rep(vector_fp, 5)
vector_color = factor(c(rep('1', 60), rep('2', 60), rep('3', 60), rep('4', 60), rep('5', 60)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)
plot_3 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('blue', 'red', 'brown', 'darkorange', 'darkgreen')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 4 (T=200)')) + ylim(c(0, 1))


load('rate_f_4_600_2.RData')
load('rate_acff_4_600_2.RData')
load('rate_acffs_4_600_2.RData')
load('rate_k_m_4_600_2.RData')
load('rate_k_i_4_600_2.RData')

vector_fp <- seq(1.1, 7, 0.1)



vector_results = c(rate_f, rate_acff, rate_acffs, rate_k_m, rate_k_i)
vector_f_p_repeated = rep(vector_fp, 5)
vector_color = factor(c(rep('1', 60), rep('2', 60), rep('3', 60), rep('4', 60), rep('5', 60)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)

plot_4 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('blue', 'red', 'brown', 'darkorange', 'darkgreen')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 4 (T=600)')) + ylim(c(0, 1))

plot_total <- grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol = 2)


graph_total <- grid.arrange(
  arrangeGrob(plot_total), nrow = 2, ncol = 1, shared_legend,
  heights = c(40, 5))

