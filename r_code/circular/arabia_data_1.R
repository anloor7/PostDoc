

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_circular/applications')
wind_data <- read.csv('arabia.csv')
wind_data <- wind_data[, c(3, 5, 10)]

# Selecting only the observations pertaining to the year 2017

wind_data_clean <- subset(wind_data, OBSERVATION_DATE >= as.Date('2010-01-01') & OBSERVATION_DATE <= as.Date('2017-12-31'))

list_series_1 <- list()
list_series_2 <- list()

# Selecting the time of one particular station


wind_data_s1 <- subset(wind_data_clean, STATION_NAME == 'ABHA')

list_series_1[[1]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-01-01') & OBSERVATION_DATE <= as.Date('2010-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[2]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-02-01') & OBSERVATION_DATE <= as.Date('2010-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[3]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-03-01') & OBSERVATION_DATE <= as.Date('2010-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[4]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2010-12-01') & OBSERVATION_DATE <= as.Date('2010-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[5]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-01-01') & OBSERVATION_DATE <= as.Date('2011-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[6]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-02-01') & OBSERVATION_DATE <= as.Date('2011-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[7]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-03-01') & OBSERVATION_DATE <= as.Date('2011-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[8]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2011-12-01') & OBSERVATION_DATE <= as.Date('2011-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[9]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-01-01') & OBSERVATION_DATE <= as.Date('2012-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[10]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-02-01') & OBSERVATION_DATE <= as.Date('2012-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[11]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-03-01') & OBSERVATION_DATE <= as.Date('2012-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[12]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2012-12-01') & OBSERVATION_DATE <= as.Date('2012-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[13]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-01-01') & OBSERVATION_DATE <= as.Date('2013-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[14]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-02-01') & OBSERVATION_DATE <= as.Date('2013-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[15]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-03-01') & OBSERVATION_DATE <= as.Date('2013-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[16]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2013-12-01') & OBSERVATION_DATE <= as.Date('2013-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[17]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-01-01') & OBSERVATION_DATE <= as.Date('2014-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[18]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-02-01') & OBSERVATION_DATE <= as.Date('2014-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[19]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-03-01') & OBSERVATION_DATE <= as.Date('2014-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[20]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2014-12-01') & OBSERVATION_DATE <= as.Date('2014-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[21]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-01-01') & OBSERVATION_DATE <= as.Date('2015-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[22]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-02-01') & OBSERVATION_DATE <= as.Date('2015-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[23]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-03-01') & OBSERVATION_DATE <= as.Date('2015-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[24]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2015-12-01') & OBSERVATION_DATE <= as.Date('2015-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[25]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-01-01') & OBSERVATION_DATE <= as.Date('2016-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[26]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-02-01') & OBSERVATION_DATE <= as.Date('2016-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[27]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-03-01') & OBSERVATION_DATE <= as.Date('2016-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[28]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2016-12-01') & OBSERVATION_DATE <= as.Date('2016-12-31'))$WIND_DIRECTION_ANGLE
list_series_1[[29]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-01-01') & OBSERVATION_DATE <= as.Date('2017-01-31'))$WIND_DIRECTION_ANGLE
list_series_1[[30]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-02-01') & OBSERVATION_DATE <= as.Date('2017-02-28'))$WIND_DIRECTION_ANGLE
list_series_1[[31]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-03-01') & OBSERVATION_DATE <= as.Date('2017-03-31'))$WIND_DIRECTION_ANGLE
list_series_1[[32]] <- (pi/180) * subset(wind_data_s1, OBSERVATION_DATE >= as.Date('2017-12-01') & OBSERVATION_DATE <= as.Date('2017-12-31'))$WIND_DIRECTION_ANGLE


# Selecting the time of other particular station

wind_data_s2 <- subset(wind_data_clean, STATION_NAME == 'ABHA')

list_series_2[[1]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-06-01') & OBSERVATION_DATE <= as.Date('2010-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[2]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-07-01') & OBSERVATION_DATE <= as.Date('2010-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[3]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-08-01') & OBSERVATION_DATE <= as.Date('2010-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[4]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2010-09-01') & OBSERVATION_DATE <= as.Date('2010-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[5]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-06-01') & OBSERVATION_DATE <= as.Date('2011-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[6]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-07-01') & OBSERVATION_DATE <= as.Date('2011-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[7]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-08-01') & OBSERVATION_DATE <= as.Date('2011-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[8]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2011-09-01') & OBSERVATION_DATE <= as.Date('2011-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[9]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-06-01') & OBSERVATION_DATE <= as.Date('2012-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[10]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-07-01') & OBSERVATION_DATE <= as.Date('2012-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[11]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-08-01') & OBSERVATION_DATE <= as.Date('2012-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[12]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2012-09-01') & OBSERVATION_DATE <= as.Date('2012-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[13]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-06-01') & OBSERVATION_DATE <= as.Date('2013-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[14]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-07-01') & OBSERVATION_DATE <= as.Date('2013-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[15]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-08-01') & OBSERVATION_DATE <= as.Date('2013-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[16]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2013-09-01') & OBSERVATION_DATE <= as.Date('2013-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[17]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-06-01') & OBSERVATION_DATE <= as.Date('2014-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[18]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-07-01') & OBSERVATION_DATE <= as.Date('2014-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[19]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-08-01') & OBSERVATION_DATE <= as.Date('2014-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[20]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2014-09-01') & OBSERVATION_DATE <= as.Date('2014-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[21]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-06-01') & OBSERVATION_DATE <= as.Date('2015-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[22]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-07-01') & OBSERVATION_DATE <= as.Date('2015-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[23]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-08-01') & OBSERVATION_DATE <= as.Date('2015-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[24]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2015-09-01') & OBSERVATION_DATE <= as.Date('2015-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[25]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-06-01') & OBSERVATION_DATE <= as.Date('2016-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[26]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-07-01') & OBSERVATION_DATE <= as.Date('2016-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[27]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-08-01') & OBSERVATION_DATE <= as.Date('2016-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[28]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2016-09-01') & OBSERVATION_DATE <= as.Date('2016-09-30'))$WIND_DIRECTION_ANGLE
list_series_2[[29]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-06-01') & OBSERVATION_DATE <= as.Date('2017-06-30'))$WIND_DIRECTION_ANGLE
list_series_2[[30]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-07-01') & OBSERVATION_DATE <= as.Date('2017-07-31'))$WIND_DIRECTION_ANGLE
list_series_2[[31]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-08-01') & OBSERVATION_DATE <= as.Date('2017-08-31'))$WIND_DIRECTION_ANGLE
list_series_2[[32]] <- (pi/180) * subset(wind_data_s2, OBSERVATION_DATE >= as.Date('2017-09-01') & OBSERVATION_DATE <= as.Date('2017-09-30'))$WIND_DIRECTION_ANGLE

list_series <- c(list_series_1, list_series_2)

vector_lengths <- numeric()

for (j in 1 : 64) {
  
  vector_lengths[j] <- length(list_series[[j]])
  
}

for (i in 1 : 64) {
  
  list_series[[i]][list_series[[i]] > 10] <- 0
  list_series[[i]][list_series[[i]] == 0] <- median.circular(list_series[[i]]) + 2 * pi
  
}

ground_truth <- c(rep(1, 32), rep(2, 32))

max_lag <- 20
alpha <- 0.05
corrected_alpha <- alpha/(64 * max_lag)

# Selection of the optimal set of lags

lags <- numeric()

for (j in 1 : 64) {
  
  auxiliary <- test_lags_circular(series = list_series[[j]], max_lag = max_lag,
                         alpha = corrected_alpha)
  index <- ifelse(length(which.max(auxiliary$correlation)) == 0, 0, 
                  which.max(abs(auxiliary$correlation)))
  lags[[j]] <- ifelse(length(auxiliary$lags) >= 1, auxiliary$lags[index],
                      0)
  
  print(j)
  
}


# Selection of r and m

features_q <- list()
set.seed(1234)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
vector_r <- seq(0.1, 2, 0.1)
l_r <- length(vector_r)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  for (j2 in vector_r) {
    
    for (i in 1 : 64) {
      
      features_q[[i]] <- autocorrelations_q(list_series[[i]], levels = levels, lags = lags, radius = j2, quantile_function = quantile_function)
      l_l <- length(which(is.na(features_q[[i]])))
      features_q[[i]][is.na(features_q[[i]])] <- runif(l_l, 0, 2 * pi)
      
    }
    
    matrix_features_q <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_q)
    dis_matrix_q <- as.matrix(proxy::dist(matrix_features_q)^2)
  
  clustering_q <- FKM((matrix_features_q), k = 2, m = j1)
  xie_beni[k] <- XB(matrix_features_q, 
                     clustering_q$U, clustering_q$H, m = 2)
  
  k <- k + 1
  print(j1)
  
}

}

which.min(xie_beni)
xie_beni_matrix <- matrix(xie_beni, nrow = l_m, ncol = l_r, byrow = T)


plot_2d_scaling <- function (distance_matrix, cluster_labels = NULL, title = "") 
{
  mds <- stats::cmdscale(distance_matrix, 2, list. = TRUE)
  gof <- mds$GOF[1]
  if (is.null(cluster_labels)) {
    df <- data.frame(cbind(mds$points))
    X1 <- df$X1
    X2 <- df$X2
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2)) + 
      ggplot2::geom_point(size = 2.5, col = "blue") + ggplot2::xlab("Coordinate 1") + 
      ggplot2::ylab("Coordinate 2") + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), 
                                                     axis.title = ggplot2::element_text(size = 17), plot.title = ggplot2::element_text(hjust = 0.5, 
                                                                                                                                       size = 18), legend.position = "bottom", legend.title = ggplot2::element_blank(), 
                                                     legend.text = ggplot2::element_text(size = 12)) + 
      ggplot2::ggtitle(title)
    return_list <- list(plot = plot, coordinates_2d = mds$points, 
                        gof = gof)
    return(return_list)
  }
  else {
    n_labels <- length(unique(cluster_labels))
    vector_labels <- numeric(n_labels)
    
    vector_labels[1] <- 'Winter'
    vector_labels[2] <- 'Summer'
    
    df <- data.frame(cbind(mds$points), factor(cluster_labels))
    colnames(df)[3] <- "series"
    series <- df$series
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2, 
                                             col = series)) + ggplot2::geom_point(size = 2.5) + 
      ggplot2::scale_color_manual(labels = vector_labels, values = c('blue', 'orangered')) + 
      ggplot2::xlab("Coordinate 1") + ggplot2::ylab("Coordinate 2") + 
      ggplot2::theme(axis.text = ggplot2::element_text(size = 15), 
                     axis.title = ggplot2::element_text(size = 17), 
                     plot.title = ggplot2::element_text(hjust = 0.5, 
                                                        size = 18), legend.position = "bottom", legend.title = ggplot2::element_blank(), 
                     legend.text = ggplot2::element_text(size = 12)) + 
      ggplot2::ggtitle(title)
    return_list <- list(plot = plot, coordinates_2d = mds$points, 
                        gof = gof)
    return(return_list)
  }
}

lags <- c(1)
levels <- c(0.1, 0.5, 0.9)
radius <- 0.7
features_q <- list()

for (i in 1 : 64) {
  
  features_q[[i]] <- autocorrelations_q(list_series[[i]], levels = levels, lags = lags, radius = radius, quantile_function = quantile_function)
  l_l <- length(which(is.na(features_q[[i]])))
  features_q[[i]][is.na(features_q[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_q <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_q)
dis_matrix_q <- as.matrix(proxy::dist(matrix_features_q)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_q, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
                             axis.text = element_text(size = 13))

set.seed(12345)
clustering_q <- fuzzy_c_medoids(matrix_features_q, C = 2, m = 1.9, dis = sqe)
clustering_q$U
ARI.F(ground_truth, clustering_q$U)
JACCARD.F(ground_truth, clustering_q$U)



# d_{FL}


# Selection of r and m

features_fl <-list()
set.seed(123456)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
    
    for (i in 1 : 64) {
      
      features_fl[[i]] <- autocorrelations_fl(list_series[[i]], lags = lags)
      l_l <- length(which(is.na(features_fl[[i]])))
      features_fl[[i]][is.na(features_fl[[i]])] <- runif(l_l, 0, 2 * pi)
      
    }
    
    matrix_features_fl <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_fl)
    matrix_features_fl <- cbind(matrix_features_fl, rep(0, 64))
    dis_matrix_fl <- as.matrix(proxy::dist(matrix_features_fl)^2)
    
    clustering_fl <- FKM(matrix_features_fl, k = 2, m = j1)
    xie_beni[k] <- XB(matrix_features_fl, 
                      clustering_fl$U, clustering_fl$H, m = 2)
    
    k <- k + 1
    print(j1)
    
  
  
}

which.min(xie_beni)


lags <- c(1)
features_fl <- list()

for (i in 1 : 64) {
  
  features_fl[[i]] <- autocorrelations_fl(list_series[[i]], lags = lags)
  l_l <- length(which(is.na(features_fl[[i]])))
  features_fl[[i]][is.na(features_fl[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_fl <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_fl)
matrix_features_fl <- cbind(matrix_features_fl, rep(0, 64))
dis_matrix_fl <- as.matrix(proxy::dist(matrix_features_fl)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_fl, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(12345)
clustering_fl <- fuzzy_c_medoids(matrix_features_fl, C = 2, m = 2, dis = sqe)
clustering_fl$U
ARI.F(ground_truth, clustering_fl$U)
JACCARD.F(ground_truth, clustering_fl$U)



# d_{J}


# Selection of r and m

features_j <-list()
set.seed(123456)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  
  for (i in 1 : 64) {
    
    features_j[[i]] <- autocorrelations_j(list_series[[i]], lags = lags)
    l_l <- length(which(is.na(features_j[[i]])))
    features_j[[i]][is.na(features_j[[i]])] <- runif(l_l, 0, 2 * pi)
    
  }
  
  matrix_features_j <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_j)
  matrix_features_j <- cbind(matrix_features_j, rep(0, 64))
  dis_matrix_j <- as.matrix(proxy::dist(matrix_features_j)^2)
  
  clustering_j <- FKM(matrix_features_j, k = 2, m = j1)
  xie_beni[k] <- XB(matrix_features_j, 
                    clustering_j$U, clustering_j$H, m = 2)
  
  k <- k + 1
  print(j1)
  
  
  
}

which.min(xie_beni)


lags <- c(1)
features_j <- list()

for (i in 1 : 64) {
  
  features_j[[i]] <- autocorrelations_j(list_series[[i]], lags = lags)
  l_l <- length(which(is.na(features_j[[i]])))
  features_j[[i]][is.na(features_j[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_j <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_j)
matrix_features_j <- cbind(matrix_features_j, rep(0, 64))
dis_matrix_j <- as.matrix(proxy::dist(matrix_features_j)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_j, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(12345)
clustering_j <- fuzzy_c_medoids(matrix_features_j, C = 2, m = 2, dis = sqe)
clustering_j$U
ARI.F(ground_truth, clustering_j$U)
JACCARD.F(ground_truth, clustering_j$U)


# d_{QA}


# Selection of m

features_noncircular <- list()
set.seed(1234)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
    for (i in 1 : 64) {
      
      features_noncircular[[i]] <- autocorrelations_noncircular(list_series[[i]], levels = levels, lags = lags)
      l_l <- length(which(is.na(features_noncircular[[i]])))
      features_noncircular[[i]][is.na(features_noncircular[[i]])] <- runif(l_l, 0, 2 * pi)
      
    }
    
    matrix_features_noncircular <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_noncircular)
    dis_matrix_noncircular <- as.matrix(proxy::dist(matrix_features_noncircular)^2)
    
    clustering_noncircular <- FKM((matrix_features_noncircular), k = 2, m = j1)
    xie_beni[k] <- XB(matrix_features_noncircular, 
                      clustering_noncircular$U, clustering_noncircular$H, m = 2)
    
    k <- k + 1
    print(j1)
    
  }
  
which.min(xie_beni)


lags <- c(1)
features_noncircular <- list()

for (i in 1 : 64) {
  
  features_noncircular[[i]] <- autocorrelations_noncircular(list_series[[i]], lags = lags, levels = levels)
  l_l <- length(which(is.na(features_noncircular[[i]])))
  features_noncircular[[i]][is.na(features_noncircular[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_noncircular <- (1/sqrt(4 * l_lags)) * list_to_matrix(features_noncircular)
dis_matrix_noncircular <- as.matrix(proxy::dist(matrix_features_noncircular)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_noncircular, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(12345)
clustering_noncircular <- fuzzy_c_medoids(matrix_features_noncircular, C = 2, m = 1.6, dis = sqe)
clustering_noncircular$U
ARI.F(ground_truth, clustering_noncircular$U)
JACCARD.F(ground_truth, clustering_noncircular$U)

# Distance based on circular quantiles


# Selection of m

features_mean <- list()
set.seed(1234)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  for (i in 1 : 64) {
    
    features_mean[[i]] <- as.numeric(quantile.circular(list_series[[i]], probs = c(0.1, 0.5, 0.9)))
   
    
  }
  
  matrix_features_mean <- list_to_matrix(features_mean)

  
  clustering_mean <- FKM(cbind(matrix_features_mean, rep(0, 64)), k = 2, m = j1)
  xie_beni[k] <- XB(cbind(matrix_features_mean, rep(0, 64)), 
                    clustering_mean$U, clustering_mean$H, m = 2)
  
  k <- k + 1
  print(j1)
  
}

which.min(xie_beni)

features_mean <- list()

for (i in 1 : 64) {
  
  features_mean[[i]] <- as.numeric(quantile.circular(list_series[[i]], probs = c(0.1, 0.5, 0.9)))
 
  
}


matrix_features_mean <- list_to_matrix(features_mean)


set.seed(12345)
clustering_mean <- fuzzy_c_medoids(cbind(matrix_features_mean, rep(0, 64)), C = 2, m = 1.9, dis = sed)
clustering_mean$U
ARI.F(ground_truth, clustering_mean$U)
JACCARD.F(ground_truth, clustering_mean$U)

# Plots of medoids for distance d_{CQA}


lags <- c(1)
levels <- c(0.1, 0.5, 0.9)
radius <- 0.7
features_q <- list()

for (i in 1 : 64) {
  
  features_q[[i]] <- autocorrelations_q(list_series[[i]], levels = levels, lags = lags, radius = radius, quantile_function = quantile_function)
  l_l <- length(which(is.na(features_q[[i]])))
  features_q[[i]][is.na(features_q[[i]])] <- runif(l_l, 0, 2 * pi)
  
}


matrix_features_q <- (1/sqrt(4 * l_lags * l_levels^2)) * list_to_matrix(features_q)
dis_matrix_q <- as.matrix(proxy::dist(matrix_features_q)^2)

plot_2ds <- plot_2d_scaling(dis_matrix_q, cluster_labels = ground_truth)$plot +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13))

set.seed(12345)
clustering_q <- fuzzy_c_medoids(matrix_features_q, C = 2, m = 1.9, dis = sqe)
clustering_q$U
ARI.F(ground_truth, clustering_q$U)
JACCARD.F(ground_truth, clustering_q$U)

vector_1 <- numeric()
vector_1[1] <- autocorrelation_q(list_series[[29]], tau_1 = 0.1, tau_2 = 0.1, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[2] <- autocorrelation_q(list_series[[29]], tau_1 = 0.1, tau_2 = 0.5, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[3] <- autocorrelation_q(list_series[[29]], tau_1 = 0.1, tau_2 = 0.9, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[4] <- autocorrelation_q(list_series[[29]], tau_1 = 0.5, tau_2 = 0.1, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[5] <- autocorrelation_q(list_series[[29]], tau_1 = 0.5, tau_2 = 0.5, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[6] <- autocorrelation_q(list_series[[29]], tau_1 = 0.5, tau_2 = 0.9, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[7] <- autocorrelation_q(list_series[[29]], tau_1 = 0.9, tau_2 = 0.1, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[8] <- autocorrelation_q(list_series[[29]], tau_1 = 0.9, tau_2 = 0.5, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_1[9] <- autocorrelation_q(list_series[[29]], tau_1 = 0.9, tau_2 = 0.9, radius = 0.7, lag = 1, quantile_function = quantile.circular)

vector_2 <- numeric()
vector_2[1] <- autocorrelation_q(list_series[[33]], tau_1 = 0.1, tau_2 = 0.1, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[2] <- autocorrelation_q(list_series[[33]], tau_1 = 0.1, tau_2 = 0.5, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[3] <- autocorrelation_q(list_series[[33]], tau_1 = 0.1, tau_2 = 0.9, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[4] <- autocorrelation_q(list_series[[33]], tau_1 = 0.5, tau_2 = 0.1, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[5] <- autocorrelation_q(list_series[[33]], tau_1 = 0.5, tau_2 = 0.5, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[6] <- autocorrelation_q(list_series[[33]], tau_1 = 0.5, tau_2 = 0.9, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[7] <- autocorrelation_q(list_series[[33]], tau_1 = 0.9, tau_2 = 0.1, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[8] <- autocorrelation_q(list_series[[33]], tau_1 = 0.9, tau_2 = 0.5, radius = 0.7, lag = 1, quantile_function = quantile.circular)
vector_2[9] <- autocorrelation_q(list_series[[33]], tau_1 = 0.9, tau_2 = 0.9, radius = 0.7, lag = 1, quantile_function = quantile.circular)

c_vector <- c('1', '1', '1', '2', '2', '2')
cqa_vector_1 <- c(vector_2[c(1, 4, 7)], vector_1[c(1, 4, 7)])
cqa_vector_2 <- c(vector_2[c(2, 5, 8)], vector_1[c(2, 5, 8)])
cqa_vector_3 <- c(vector_2[c(3, 6, 9)], vector_1[c(3, 6, 9)])

df_1 <- data.frame(tau_2 = c_vector, tau_1 = cqa_vector_1, x = rep(c(0.1, 0.5, 0.9), 2))
df_2 <- data.frame(tau_2 = c_vector, tau_1 = cqa_vector_2, x = rep(c(0.1, 0.5, 0.9), 2))
df_3 <- data.frame(tau_2 = c_vector, tau_1 = cqa_vector_3, x = rep(c(0.1, 0.5, 0.9), 2))

plot_1_prev <- ggplot(df_1, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-1, 1)) + ggtitle(TeX('$\\tau_2=0.1$')) +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,  size = 18),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('Medoid ($C_1$)'), TeX('Medoid ($C_2$)'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$'))  

shared_legend <- extract_legend(plot_1_prev)

plot_1 <- ggplot(df_1, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-1, 1)) + ggtitle(TeX('$\\tau_2=0.1$')) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,  size = 13),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('$\\tau_2=0.1$'), TeX('$\\tau_2=0.5$'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$')) + ylab(TeX('$\\widehat{\\rho}(\\tau_1, 0.1, 1, 0.7)$')) +
  scale_x_continuous(breaks = c(0.1, 0.5, 0.9))

plot_2 <- ggplot(df_2, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-1, 1)) + ggtitle(TeX('$\\tau_2=0.5$')) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,  size = 13),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('$\\tau_2=0.1$'), TeX('$\\tau_2=0.5$'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$')) + ylab(TeX('$\\widehat{\\rho}(\\tau_1, 0.5, 1, 0.7)$')) +
  scale_x_continuous(breaks = c(0.1, 0.5, 0.9))


plot_3 <- ggplot(df_3, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-1, 1)) + ggtitle(TeX('$\\tau_2=0.9$')) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,  size = 13),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('$\\tau_2=0.1$'), TeX('$\\tau_2=0.5$'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$')) + ylab(TeX('$\\widehat{\\rho}(\\tau_1, 0.9, 1, 0.7)$')) +
  scale_x_continuous(breaks = c(0.1, 0.5, 0.9))

plot_total <- grid.arrange(plot_1, plot_2, plot_3, ncol = 3)

plot_final <- grid.arrange(
  arrangeGrob(plot_total), nrow = 2, ncol = 1, shared_legend,
  heights = c(40, 8))

