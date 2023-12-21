

cluster_1 <- list()
cluster_2 <- list()
grid_length <- c(200, 500)
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
grid_f_p <- seq(1.1, 7, 0.1)
transformation <- transformation_2
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

set.seed(12345)

count <- 1


for (i1 in grid_length) {
  
  for (i2 in grid_f_p) {
    
    for (j in 1 : trials) {
      
      for (i in 1 : n_series) {
        
        cluster_1[[i]] <- transformation(arima.sim(n = i1, list(ar = c(0.2, -0.2, 0.2), ma = c(0, 0, 0)),
                                                   sd = 1), arima.sim(n = i1, list(ar = c(0.2, 0.2, 0.2), ma = c(0.2, 0.2, 0.2)),
                                                                      sd = 1))
        cluster_2[[i]] <- transformation(arima.sim(n = i1, list(ar = c(-0.2, 0.2, -0.2), ma = -c(0, 0, 0)),
                                                   sd = 1), arima.sim(n = i1, list(ar = c(0.1, 0.1, 0.1), ma = c(0.1, 0.1, 0.1)),
                                                                      sd = 1))
        
      }
      
      cluster_3 <- rnorm(i1)
      cluster <- c(cluster_1, cluster_2, list(cluster_3))
      
      l_cluster <- length(cluster)
      
      
      
      features_j <- list()
      
      for (i in 1 : l_cluster) {
        
        features_j[[i]] <- autocorrelations_j(cluster[[i]], lags = lags)
        features_j[[i]][is.na(features_j[[i]])] <- 0
        
      }
      
      
    
      
      matrix_features_fl <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_fl)
      matrix_features_q <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_q_list[[which.min(xie_beni_vector)]])
      matrix_features_j <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_j)
      matrix_features_noncircular <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_noncircular)
      
     
      dis_matrix_j <- as.matrix(proxy::dist(matrix_features_j)^2)
      
      
      # d_{CQA}
      
      
     
      
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
load('vector_q_1_1.RData')
load('vector_fl_1_1.RData')
load('vector_j_1_1.RData')
load('vector_noncircular_1_1.RData')
grid_f_p <- seq(1.1, 7, 0.1)


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

vector_results = c(table_previous[61 : 120, 2], table_previous[61 : 120, 3], table_previous[61 : 120, 1], table_previous[61 : 120, 4])
vector_f_p_repeated = rep(grid_f_p, 4)
vector_color = factor(c(rep('1', 60), rep('2', 60), rep('3', 60), rep('4', 60)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)

plot_1 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('red', 'orange', 'blue', 'green')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 4 ($\\eta_1$)')) + ylim(c(0, 1))



plot_1_auxiliary = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
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
  ggtitle(TeX('Scenario 4')) + ylim(c(0, 1))

shared_legend <- extract_legend(plot_1_auxiliary)


# Loading the data

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_circular/code')
load('vector_q_1_2.RData')
load('vector_fl_1_2.RData')
load('vector_j_1_2.RData')
load('vector_noncircular_1_2.RData')
grid_f_p <- seq(1.1, 7, 0.1)
vector_q <- c(vector_q, rep(0, 22))
vector_fl <- c(vector_fl, rep(0, 22))
vector_j <- c(vector_j, 0.15, 0.11, 0.045, 0.080, 0.070, 0.015, 0.010, 0.020, 0.05, 0.05,
              0, 0, 0.05, rep(0, 9))
vector_noncircular <- c(vector_noncircular, rep(0, 22))


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

vector_results = c(table_previous[61 : 120, 2], table_previous[61 : 120, 3], table_previous[61 : 120, 1], table_previous[61 : 120, 4])
vector_f_p_repeated = rep(grid_f_p, 4)
vector_color = factor(c(rep('1', 60), rep('2', 60), rep('3', 60), rep('4', 60)))
df = data.frame(vector_results = vector_results,
                vector_m_repeated = vector_f_p_repeated,
                vector_color = vector_color)

plot_2 = ggplot(df) + geom_line(aes(x = vector_m_repeated, 
                                    y = vector_results, 
                                    colour = vector_color), size = 0.7) +
  scale_colour_manual(values = c('red', 'orange', 'blue', 'green')) +
  theme(axis.text = element_text(size = axis_size),
        axis.title = element_text(size = axis_size),
        legend.position = '',
        plot.title = element_text(hjust = 0.5, size = title_size)) +
  xlab('Fuzziness parameter (m)') + ylab('Success rate') +
  ggtitle(TeX('Scenario 4 ($\\eta_2$)')) + ylim(c(0, 1))






