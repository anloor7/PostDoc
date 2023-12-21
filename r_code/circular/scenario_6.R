

cluster_1 <- list()
cluster_2 <- list()
grid_length <- c(500, 1000)
n_series <- 5
K <- 2
radius <- 1.2
grid_radius <- seq(0.5, 2, by = 0.1)
l_grid_radius <- length(grid_radius)
lags <- c(1, 2, 3)
levels <- c(0.1, 0.5, 0.9)
l_lags <- length(lags)
l_levels <- length(levels)
selected_r <- numeric() 
grid_f_p <- seq(1.1, 4, 0.1)
transformation <- transformation_1
quantile_function <- quantile.circular
cutoff <- 0.70

rate_q <- numeric()
rate_fl <- numeric()
rate_j <- numeric()
rate_noncircular <- numeric()

vector_q <- numeric()
vector_fl <- numeric()
vector_j <- numeric()
vector_noncircular <- numeric()

trials <- 200

set.seed(1234)

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
        
      }
      
      cluster_3 <- transformation(garch.sim(alpha = c(0.1, 0.225, 0.225), beta = c(0.225, 0.225), n = i1), 
                                       garch.sim(alpha = c(0.1, 0.1, 0.1), beta = c(0.6, -0.3), n = i1))
      cluster_3[is.na(cluster_1[[i]])] <- 0
      cluster <- c(cluster_1, cluster_2, list(cluster_3))
      
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
        clustering_q <- FKM(matrix_features_q, k = K, m = i2, maxit = 1000)
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
      
      matrix_features_fl <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_fl)
      matrix_features_q <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_q_list[[which.min(xie_beni_vector)]])
      matrix_features_j <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_j)
      matrix_features_noncircular <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_noncircular)
      
      dis_matrix_fl <- as.matrix(proxy::dist(matrix_features_fl)^2)
      dis_matrix_q <- as.matrix(proxy::dist(matrix_features_q)^2)
      dis_matrix_j <- as.matrix(proxy::dist(matrix_features_j)^2)
      dis_matrix_noncircular <- as.matrix(proxy::dist(matrix_features_noncircular)^2)
      
      # d_{CQA}
      
      
      auxiliary_q_u <- list()
      auxiliary_q_of <- numeric()
      
      for (k in 1 : 200) {
        
        auxiliary_q_clustering <- nonweighted_clustering((dis_matrix_q), C = K, m = i2, max_iter = 100)
        auxiliary_q_u[[k]] <-  auxiliary_q_clustering$U
        auxiliary_q_of[k] <- auxiliary_q_clustering$of
        
      }
      
      max_mem_q <- apply(auxiliary_q_u[[which.min(auxiliary_q_of)]], 1, max)
      clustering_q <- fuzzytocrisp(auxiliary_q_u[[which.min(auxiliary_q_of)]])
      
      l_1 <- length(unique(clustering_q[1 : 5]))
      l_2 <- length(unique(clustering_q[6 : 10]))
      l_3 <- length(unique(clustering_q[1 : 10]))
      
      
      if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
          sum(max_mem_q[1 : 10] > cutoff) == 10 &
          max_mem_q[11] < cutoff) {
        
        rate_q[j] <- 1
        
      } else {
        
        rate_q[j] <- 0
        
      }
      
      # d_{FL}
      
      auxiliary_fl_u <- list()
      auxiliary_fl_of <- numeric()
      
      for (k in 1 : 200) {
        
        auxiliary_fl_clustering <- nonweighted_clustering((dis_matrix_fl), C = K, m = i2, max_iter = 100)
        auxiliary_fl_u[[k]] <-  auxiliary_fl_clustering$U
        auxiliary_fl_of[k] <- auxiliary_fl_clustering$of
        
      }
      
      max_mem_fl <- apply(auxiliary_fl_u[[which.min(auxiliary_fl_of)]], 1, max)
      clustering_fl <- fuzzytocrisp(auxiliary_fl_u[[which.min(auxiliary_fl_of)]])
      
      l_1 <- length(unique(clustering_fl[1 : 5]))
      l_2 <- length(unique(clustering_fl[6 : 10]))
      l_3 <- length(unique(clustering_fl[1 : 10]))
      
      
      if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
          sum(max_mem_fl[1 : 10] > cutoff) == 10 &
          max_mem_fl[11] < cutoff) {
        
        rate_fl[j] <- 1
        
      } else {
        
        rate_fl[j] <- 0
        
      }
      
      # d_J
      
      
      auxiliary_j_u <- list()
      auxiliary_j_of <- numeric()
      
      for (k in 1 : 200) {
        
        auxiliary_j_clustering <- nonweighted_clustering((dis_matrix_j), C = K, m = i2, max_iter = 100)
        auxiliary_j_u[[k]] <-  auxiliary_j_clustering$U
        auxiliary_j_of[k] <- auxiliary_j_clustering$of
        
      }
      
      max_mem_j <- apply(auxiliary_j_u[[which.min(auxiliary_j_of)]], 1, max)
      clustering_j <- fuzzytocrisp(auxiliary_j_u[[which.min(auxiliary_j_of)]])
      
      l_1 <- length(unique(clustering_j[1 : 5]))
      l_2 <- length(unique(clustering_j[6 : 10]))
      l_3 <- length(unique(clustering_j[1 : 10]))
      
      
      if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
          sum(max_mem_j[1 : 10] > cutoff) == 10 &
          max_mem_j[11] < cutoff) {
        
        rate_j[j] <- 1
        
      } else {
        
        rate_j[j] <- 0
        
      }
      
      # d_{CQA}
      
      auxiliary_noncircular_u <- list()
      auxiliary_noncircular_of <- numeric()
      
      for (k in 1 : 200) {
        
        auxiliary_noncircular_clustering <- nonweighted_clustering((dis_matrix_noncircular), C = K, m = i2, max_iter = 100)
        auxiliary_noncircular_u[[k]] <-  auxiliary_noncircular_clustering$U
        auxiliary_noncircular_of[k] <- auxiliary_noncircular_clustering$of
        
      }
      
      max_mem_noncircular <- apply(auxiliary_noncircular_u[[which.min(auxiliary_noncircular_of)]], 1, max)
      clustering_noncircular <- fuzzytocrisp(auxiliary_noncircular_u[[which.min(auxiliary_noncircular_of)]])
      
      l_1 <- length(unique(clustering_noncircular[1 : 5]))
      l_2 <- length(unique(clustering_noncircular[6 : 10]))
      l_3 <- length(unique(clustering_noncircular[1 : 10]))
      
      
      if (l_1 == 1 & l_2 == 1 & l_3 == 2 &
          sum(max_mem_noncircular[1 : 10] > cutoff) == 10 &
          max_mem_noncircular[11] < cutoff) {
        
        rate_noncircular[j] <- 1
        
      } else {
        
        rate_noncircular[j] <- 0
        
      }
      
    }
    
    
    
    vector_q[count] <- mean(rate_q)
    vector_fl[count] <- mean(rate_fl)
    vector_j[count] <- mean(rate_j)
    vector_noncircular[count] <- mean(rate_noncircular)
    print(count)
    count <- count + 1
    
    
    
    
  }
  
}


# Loading the data
setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_circular/code')
title_size <- 10
axis_size <- 9
legend_size <- 10
grid_f_p <- seq(1.1, 4, 0.1)
load('vector_q_3_1.RData')
load('vector_fl_3_1.RData')
load('vector_j_3_1.RData')
load('vector_noncircular_3_1.RData')


# Creating the corresponding table

table_previous <- matrix(0, nrow = 2 * length(grid_f_p), ncol = 4)
table_previous[,1] <- vector_q
table_previous[,2] <- vector_fl
table_previous[,3] <- vector_j
table_previous[,4] <- vector_noncircular
round(table_previous, 3)

l_m <- length(grid_f_p)
apply(table_previous[1 : l_m,], 2, max)
trapz(grid_f_p, table_previous[1 : l_m, 1])
trapz(grid_f_p, table_previous[1 : l_m, 2])
trapz(grid_f_p, table_previous[1 : l_m, 3])
trapz(grid_f_p, table_previous[1 : l_m, 4])

apply(table_previous[(l_m + 1) : (2 * l_m),], 2, max)
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 1])
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 2])
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 3])
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 4])

# Plotting the corresponding linecharts

vector_results = c(table_previous[31 : 60, 2], table_previous[31 : 60, 3], table_previous[31 : 60, 1], table_previous[31 : 60, 4])
vector_f_p_repeated = rep(grid_f_p, 4)
vector_color = factor(c(rep('1', 30), rep('2', 30), rep('3', 30), rep('4', 30)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)

plot_5 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('red', 'orange', 'blue', 'green')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 6 ($\\eta_1$)')) + ylim(c(0, 1))



plot_5_auxiliary = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                             y = vector_results, 
                                             colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('red', 'orange', 'blue', 'green'), 
                      labels =  c(TeX('$\\widehat{d}_{FL}$'),
                                  TeX('$\\widehat{d}_{JS}$'), 
                                  TeX('$\\widehat{d}_{CQA}$'),
                                  TeX('$\\widehat{d}_{QA}$'))) + 
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = legend_size),
        plot.title = element_text(hjust = 0.5, size = 12)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 6')) + ylim(c(0, 1))

shared_legend <- extract_legend(plot_1_auxiliary)


# Loading the data

grid_f_p <- seq(1.1, 4, 0.1)
load('vector_q_3_2.RData')
load('vector_fl_3_2.RData')
load('vector_j_3_2.RData')
load('vector_noncircular_3_2.RData')


# Creating the corresponding table

table_previous <- matrix(0, nrow = 2 * length(grid_f_p), ncol = 4)
table_previous[,1] <- vector_q
table_previous[,2] <- vector_fl
table_previous[,3] <- vector_j
table_previous[,4] <- vector_noncircular
round(table_previous, 3)

l_m <- length(grid_f_p)
apply(table_previous[1 : l_m,], 2, max)
trapz(grid_f_p, table_previous[1 : l_m, 1])
trapz(grid_f_p, table_previous[1 : l_m, 2])
trapz(grid_f_p, table_previous[1 : l_m, 3])
trapz(grid_f_p, table_previous[1 : l_m, 4])

apply(table_previous[(l_m + 1) : (2 * l_m),], 2, max)
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 1])
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 2])
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 3])
trapz(grid_f_p, table_previous[(l_m + 1) : (2 * l_m), 4])

# Plotting the corresponding linecharts

vector_results = c(table_previous[31 : 60, 2], table_previous[31 : 60, 3], table_previous[31 : 60, 1], table_previous[31 : 60, 4])
vector_f_p_repeated = rep(grid_f_p, 4)
vector_color = factor(c(rep('1', 30), rep('2', 30), rep('3', 30), rep('4', 30)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)

plot_6 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('red', 'orange', 'blue', 'green')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 6 ($\\eta_2$)')) + ylim(c(0, 1))

plot_all <- grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6)

plot <- grid.arrange(
  arrangeGrob(plot_all), nrow = 2, ncol = 1, shared_legend,
  heights = c(40, 5))

