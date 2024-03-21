

cluster_1 <- list()
cluster_2 <- list()
trials <- 200
series_length <- 200
vector_fp <- seq(1.1, 7, 0.1)
l_fp <- length(vector_fp)
K <- 2
cutoff <- 0.7
table_results <- matrix(0, nrow = l_fp, ncol = 3)


matrix_f <- matrix(0, l_fp, trials)
matrix_k_m <- matrix(0, l_fp, trials)
matrix_k_i <- matrix(0, l_fp, trials)

set.seed(1234)

for (j in 1 : trials) {
  
  
  for (i in 1 : 5) {
    
    cluster_1[[i]] <- hanlin_linear_2(l = series_length, p = 100, -0.4, 0.5, -0.4, 0.5)
    cluster_2[[i]] <- hanlin_linear_2(l = series_length, p = 100, 0.4, 0.5, 0.4, 0.5)
    
  }
  
  cluster_3 <- list(hanlin_linear_2(l = series_length, p = 100, 0, 0.5, 0, 0.5))
  cluster <- c(cluster_1, cluster_2, cluster_3)
  
  feature_matrix <- lapply(cluster, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1, 2))
  feature_dataset <- list_to_matrix(feature_matrix)
  feature_dataset[is.na(feature_dataset)] <- 0
  
  feature_matrix_k_m <- lapply(cluster, aucors_kendall_m, lags = c(1, 2))
  feature_dataset_k_m <- list_to_matrix(feature_matrix_k_m)
  feature_dataset_k_m[is.na(feature_dataset_k_m)] <- 0
  
  feature_matrix_k_i <- lapply(cluster, aucors_kendall_i, lags = c(1, 2))
  feature_dataset_k_i <- list_to_matrix(feature_matrix_k_i)
  feature_dataset_k_i[is.na(feature_dataset_k_i)] <- 0
  
  
  for (k1 in 1 : l_fp) {
    

      
      cl <- fuzzy_c_medoids(X = feature_dataset, m = vector_fp[k1], C = K, dis = sed)$U
      cl_k_m <- fuzzy_c_medoids(X = cbind(c(feature_dataset_k_m), c(rep(0, 11))), m = vector_fp[k1], C = K, dis = sed)$U
      cl_k_i <- fuzzy_c_medoids(X = cbind(c(feature_dataset_k_i), c(rep(0, 11))), m = vector_fp[k1], C = K, dis = sed)$U
      max_mem_f <- apply(cl, 1, max)
      max_mem_k_m <- apply(cl_k_m, 1, max)
      max_mem_k_i <- apply(cl_k_i, 1, max)
      
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
    
    
  }
  
  print(j)
  
}

rate_f <- rowmeans(matrix_f)
rate_k_m <- rowmeans(matrix_k_m)
rate_k_i <- rowmeans(matrix_k_i)

mean(rate_f)
mean(rate_k_m)
mean(rate_k_i)

max(rate_f)
max(rate_k_m)
max(rate_k_i)

trapz(vector_fp, rate_f)
trapz(vector_fp, rate_k_m)
trapz(vector_fp, rate_k_i)


# Representing plots of the curves 

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_functional/code')
load('rate_f_3_200.RData')
load('rate_k_m_3_200.RData')
load('rate_k_i_3_200.RData')
axis_size <- 9.5
title_size <- 10.5

vector_fp <- seq(1.1, 7, 0.1)
rate_k_m[c(58, 59, 60)] <- c(0, 0, 0)
rate_k_i[c(58, 59, 60)] <- c(0, 0, 0)


vector_results = c(rate_f, rate_k_m, rate_k_i)
vector_f_p_repeated = rep(vector_fp, 3)
vector_color = factor(c(rep('1', 60), rep('2', 60), rep('3', 60)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)
plot_1_auxiliary = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('blue', 'darkorange', 'darkgreen'), 
                      labels = c(TeX('$\\widehat{d}_{FQA}$'), TeX('$\\widehat{d}_{K_m}$'), TeX('$\\widehat{d}_{K_i}$'))) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 3 (T=200)')) + ylim(c(0, 1)) 

shared_legend <- extract_legend(plot_1_auxiliary)


plot_1 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                              y = vector_results, 
                                              colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('blue', 'darkorange', 'darkgreen')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 3 (T=200)')) + ylim(c(0, 1))


load('rate_f_3_600.RData')
load('rate_k_m_3_600.RData')
load('rate_k_i_3_600.RData')
vector_fp <- seq(1.1, 7, 0.1)
rate_k_m[c(58, 59, 60)] <- c(0, 0, 0)
rate_k_i[c(58, 59, 60)] <- c(0, 0, 0)


vector_results = c(rate_f, rate_k_m, rate_k_i)
vector_f_p_repeated = rep(vector_fp, 3)
vector_color = factor(c(rep('1', 60), rep('2', 60), rep('3', 60)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)


plot_2 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('blue', 'darkorange', 'darkgreen')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 3 (T=600)')) + ylim(c(0, 1))

