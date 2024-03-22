
setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_functional/data/finance')


# Loading the data

first_data <- read.csv('first_hanlin.csv')[c(1, 4, 7, 8)]


# Indexes of companies in communication services sector and corresponding dataset

indexes_f_1 <- which(first_data$X.RIC == 'GOOGL.OQ'); df_f_1 <- first_data[indexes_f_1,] 
indexes_f_2 <- which(first_data$X.RIC == 'GOOG.OQ'); df_f_2 <- first_data[indexes_f_2,] 
indexes_f_3 <- which(first_data$X.RIC == 'T.N'); df_f_3 <- first_data[indexes_f_3,]
indexes_f_4 <- which(first_data$X.RIC == 'CHTR.OQ'); df_f_4 <- first_data[indexes_f_4,]
indexes_f_5 <- which(first_data$X.RIC == 'CSCO.OQ'); df_f_5 <- first_data[indexes_f_5,] 
indexes_f_6 <- which(first_data$X.RIC == 'CMCSA.OQ'); df_f_6 <- first_data[indexes_f_6,] 
indexes_f_7 <- which(first_data$X.RIC == 'EA.OQ'); df_f_7 <- first_data[indexes_f_7,] 
indexes_f_8 <- which(first_data$X.RIC == 'FOXA.OQ'); df_f_8 <- first_data[indexes_f_8,] 
indexes_f_9 <- which(first_data$X.RIC == 'FOX.OQ'); df_f_9 <- first_data[indexes_f_9,] 
indexes_f_10 <- which(first_data$X.RIC == 'IPG.N'); df_f_10 <- first_data[indexes_f_10,] 
indexes_f_11 <- which(first_data$X.RIC == 'LYV.N'); df_f_11 <- first_data[indexes_f_11,] 
indexes_f_12 <- which(first_data$X.RIC == 'FB.OQ'); df_f_12 <- first_data[indexes_f_12,] 
indexes_f_13 <- which(first_data$X.RIC == 'NFLX.OQ'); df_f_13 <- first_data[indexes_f_13,] 
indexes_f_14 <- which(first_data$X.RIC == 'NWSA.OQ'); df_f_14 <- first_data[indexes_f_14,] 
indexes_f_15 <- which(first_data$X.RIC == 'NWS.OQ'); df_f_15 <- first_data[indexes_f_15,] 
indexes_f_16 <- which(first_data$X.RIC == 'OMC.N'); df_f_16 <- first_data[indexes_f_16,] 
indexes_f_17 <- which(first_data$X.RIC == 'TMUS.OQ'); df_f_17 <- first_data[indexes_f_17,] 
indexes_f_18 <- which(first_data$X.RIC == 'TTWO.OQ'); df_f_18 <- first_data[indexes_f_18,] 
indexes_f_19 <- which(first_data$X.RIC == 'VZ.N'); df_f_19 <- first_data[indexes_f_19,] 
indexes_f_20 <- which(first_data$X.RIC == 'DIS.N'); df_f_20 <- first_data[indexes_f_20,] 




# Indexes of companies in energy sector and corresponding dataset

indexes_u_1 <- which(first_data$X.RIC == 'APA.N'); df_u_1 <- first_data[indexes_u_1,] 
indexes_u_2 <- which(first_data$X.RIC == 'BKR.N'); df_u_2 <- first_data[indexes_u_2,] 
indexes_u_3 <- which(first_data$X.RIC == 'CVX.N'); df_u_3 <- first_data[indexes_u_3,] 
indexes_u_4 <- which(first_data$X.RIC == 'COP.N'); df_u_4 <- first_data[indexes_u_4,]
indexes_u_5 <- which(first_data$X.RIC == 'DVN.N'); df_u_5 <- first_data[indexes_u_5,] 
indexes_u_6 <- which(first_data$X.RIC == 'FANG.OQ'); df_u_6 <- first_data[indexes_u_6,] 
indexes_u_7 <- which(first_data$X.RIC == 'EOG.N'); df_u_7 <- first_data[indexes_u_7,] 
indexes_u_8 <- which(first_data$X.RIC == 'EQT.N'); df_u_8 <- first_data[indexes_u_8,] 
indexes_u_9 <- which(first_data$X.RIC == 'XOM.N'); df_u_9 <- first_data[indexes_u_9,] 
indexes_u_10 <- which(first_data$X.RIC == 'HAL.N'); df_u_10 <- first_data[indexes_u_10,] 
indexes_u_11 <- which(first_data$X.RIC == 'HES.N'); df_u_11 <- first_data[indexes_u_11,] 
indexes_u_12 <- which(first_data$X.RIC == 'KMI.N'); df_u_12 <- first_data[indexes_u_12,] 
indexes_u_13 <- which(first_data$X.RIC == 'MRO.N'); df_u_13 <- first_data[indexes_u_13,] 
indexes_u_14 <- which(first_data$X.RIC == 'MPC.N'); df_u_14 <- first_data[indexes_u_14,] 
indexes_u_15 <- which(first_data$X.RIC == 'OXY.N'); df_u_15 <- first_data[indexes_u_15,] 
indexes_u_16 <- which(first_data$X.RIC == 'OKE.N'); df_u_16 <- first_data[indexes_u_16,] 
indexes_u_17 <- which(first_data$X.RIC == 'PSX.N'); df_u_17 <- first_data[indexes_u_17,] 
indexes_u_18 <- which(first_data$X.RIC == 'PXD.N'); df_u_18 <- first_data[indexes_u_18,] 
indexes_u_19 <- which(first_data$X.RIC == 'SLB.N'); df_u_19 <- first_data[indexes_u_19,] 
indexes_u_20 <- which(first_data$X.RIC == 'VLO.N'); df_u_20 <- first_data[indexes_u_20,] 


# Creating the corresponding FTSdf_f_1

fts_f_1 <- create_sp_fts(df_f_1); fts_f_1[is.na(fts_f_1)] <- 0
fts_f_2 <- create_sp_fts(df_f_2); fts_f_2[is.na(fts_f_2)] <- 0
fts_f_3 <- create_sp_fts(df_f_3); fts_f_3[is.na(fts_f_3)] <- 0
fts_f_4 <- create_sp_fts(df_f_4); fts_f_4[is.na(fts_f_4)] <- 0
fts_f_5 <- create_sp_fts(df_f_5); fts_f_5[is.na(fts_f_5)] <- 0
fts_f_6 <- create_sp_fts(df_f_6); fts_f_6[is.na(fts_f_6)] <- 0
fts_f_7 <- create_sp_fts(df_f_7); fts_f_7[is.na(fts_f_7)] <- 0
fts_f_8 <- create_sp_fts(df_f_8); fts_f_8[is.na(fts_f_8)] <- 0
fts_f_9 <- create_sp_fts(df_f_9); fts_f_9[is.na(fts_f_9)] <- 0
fts_f_10 <- create_sp_fts(df_f_10); fts_f_10[is.na(fts_f_10)] <- 0
fts_f_11 <- create_sp_fts(df_f_11); fts_f_11[is.na(fts_f_11)] <- 0
fts_f_12 <- create_sp_fts(df_f_12); fts_f_12[is.na(fts_f_12)] <- 0
fts_f_13 <- create_sp_fts(df_f_13); fts_f_13[is.na(fts_f_13)] <- 0
fts_f_14 <- create_sp_fts(df_f_14); fts_f_14[is.na(fts_f_14)] <- 0
fts_f_15 <- create_sp_fts(df_f_15); fts_f_15[is.na(fts_f_15)] <- 0
fts_f_16 <- create_sp_fts(df_f_16); fts_f_16[is.na(fts_f_16)] <- 0
fts_f_17 <- create_sp_fts(df_f_17); fts_f_17[is.na(fts_f_17)] <- 0
fts_f_18 <- create_sp_fts(df_f_18); fts_f_18[is.na(fts_f_18)] <- 0
fts_f_19 <- create_sp_fts(df_f_19); fts_f_19[is.na(fts_f_19)] <- 0
fts_f_20 <- create_sp_fts(df_f_20); fts_f_20[is.na(fts_f_20)] <- 0

fts_f <- list(fts_f_1, fts_f_2, fts_f_3, fts_f_4, fts_f_5,
              fts_f_6, fts_f_7, fts_f_8, fts_f_9, fts_f_10,
              fts_f_11, fts_f_12, fts_f_13, fts_f_14, fts_f_15,
              fts_f_16, fts_f_17, fts_f_18, fts_f_19, fts_f_20)


fts_u_1 <- create_sp_fts(df_u_1); fts_u_1[is.na(fts_u_1)] <- 0
fts_u_2 <- create_sp_fts(df_u_2); fts_u_2[is.na(fts_u_2)] <- 0
fts_u_3 <- create_sp_fts(df_u_3); fts_u_3[is.na(fts_u_3)] <- 0
fts_u_4 <- create_sp_fts(df_u_4); fts_u_4[is.na(fts_u_4)] <- 0
fts_u_5 <- create_sp_fts(df_u_5); fts_u_5[is.na(fts_u_5)] <- 0
fts_u_6 <- create_sp_fts(df_u_6); fts_u_6[is.na(fts_u_6)] <- 0
fts_u_7 <- create_sp_fts(df_u_7); fts_u_7[is.na(fts_u_7)] <- 0
fts_u_8 <- create_sp_fts(df_u_8); fts_u_8[is.na(fts_u_8)] <- 0
fts_u_9 <- create_sp_fts(df_u_9); fts_u_9[is.na(fts_u_9)] <- 0
fts_u_10 <- create_sp_fts(df_u_10); fts_u_10[is.na(fts_u_10)] <- 0
fts_u_11 <- create_sp_fts(df_u_11); fts_u_11[is.na(fts_u_11)] <- 0
fts_u_12 <- create_sp_fts(df_u_12); fts_u_12[is.na(fts_u_12)] <- 0
fts_u_13 <- create_sp_fts(df_u_13); fts_u_13[is.na(fts_u_13)] <- 0
fts_u_14 <- create_sp_fts(df_u_14); fts_u_14[is.na(fts_u_14)] <- 0
fts_u_15 <- create_sp_fts(df_u_15); fts_u_15[is.na(fts_u_15)] <- 0
fts_u_16 <- create_sp_fts(df_u_16); fts_u_16[is.na(fts_u_16)] <- 0
fts_u_17 <- create_sp_fts(df_u_17); fts_u_17[is.na(fts_u_17)] <- 0
fts_u_18 <- create_sp_fts(df_u_18); fts_u_18[is.na(fts_u_18)] <- 0
fts_u_19 <- create_sp_fts(df_u_19); fts_u_19[is.na(fts_u_19)] <- 0
fts_u_20 <- create_sp_fts(df_u_20); fts_u_20[is.na(fts_u_20)] <- 0

fts_u <- list(fts_u_1, fts_u_2, fts_u_3, fts_u_4, fts_u_5,
              fts_u_6, fts_u_7, fts_u_8, fts_u_9, fts_u_10,
              fts_u_11, fts_u_12, fts_u_13, fts_u_14, fts_u_15,
              fts_u_16, fts_u_17, fts_u_18, fts_u_19, fts_u_20)


fts_list <- c(fts_f, fts_u)



# Selecting the set of lags

test_1 <- test_functional(fts_f_1, lags = seq(1, 10))
test_2 <- test_functional(fts_f_2, lags = seq(1, 10))
test_3 <- test_functional(fts_f_3, lags = seq(1, 10))
test_4 <- test_functional(fts_f_4, lags = seq(1, 10))
test_5 <- test_functional(fts_f_5, lags = seq(1, 10))
test_6 <- test_functional(fts_f_6, lags = seq(1, 10))
test_7 <- test_functional(fts_f_7, lags = seq(1, 10))
test_8 <- test_functional(fts_f_8, lags = seq(1, 10))
test_9 <- test_functional(fts_f_9, lags = seq(1, 10))
test_10 <- test_functional(fts_f_10, lags = seq(1, 10))
test_11 <- test_functional(fts_f_11, lags = seq(1, 10))
test_12 <- test_functional(fts_f_12, lags = seq(1, 10))
test_13 <- test_functional(fts_f_13, lags = seq(1, 10))
test_14 <- test_functional(fts_f_14, lags = seq(1, 10))
test_15 <- test_functional(fts_f_15, lags = seq(1, 10))
test_16 <- test_functional(fts_f_16, lags = seq(1, 10))
test_17 <- test_functional(fts_f_17, lags = seq(1, 10))
test_18 <- test_functional(fts_f_18, lags = seq(1, 10))
test_19 <- test_functional(fts_f_19, lags = seq(1, 10))
test_20 <- test_functional(fts_f_20, lags = seq(1, 10))


test_21 <- test_functional(fts_u_1, lags = seq(1, 10))
test_22 <- test_functional(fts_u_2, lags = seq(1, 10))
test_23 <- test_functional(fts_u_3, lags = seq(1, 10))
test_24 <- test_functional(fts_u_4, lags = seq(1, 10))
test_25 <- test_functional(fts_u_5, lags = seq(1, 10))
test_26 <- test_functional(fts_u_6, lags = seq(1, 10))
test_27 <- test_functional(fts_u_7, lags = seq(1, 10))
test_28 <- test_functional(fts_u_8, lags = seq(1, 10))
test_29 <- test_functional(fts_u_9, lags = seq(1, 10))
test_30 <- test_functional(fts_u_10, lags = seq(1, 10))
test_31 <- test_functional(fts_u_11, lags = seq(1, 10))
test_32 <- test_functional(fts_u_12, lags = seq(1, 10))
test_33 <- test_functional(fts_u_13, lags = seq(1, 10))
test_34 <- test_functional(fts_u_14, lags = seq(1, 10))
test_35 <- test_functional(fts_u_15, lags = seq(1, 10))
test_36 <- test_functional(fts_u_16, lags = seq(1, 10))
test_37 <- test_functional(fts_u_17, lags = seq(1, 10))
test_38 <- test_functional(fts_u_18, lags = seq(1, 10))
test_39 <- test_functional(fts_u_19, lags = seq(1, 10))
test_40 <- test_functional(fts_u_20, lags = seq(1, 10))



# Selecting the hyperparameter m

features_fqa <- list()
set.seed(1234)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

feature_matrix_fqa <- lapply(fts_list, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = lags)
feature_dataset_fqa <- list_to_matrix(feature_matrix_fqa)
feature_dataset_fqa[is.na(feature_dataset_fqa)] <- 0

for (j1 in vector_m) {
  
  
  dis_matrix_fqa <- as.matrix(proxy::dist(feature_dataset_fqa)^2)
  
  clustering_fqa <- FKM((feature_dataset_fqa), k = 2, m = j1)
  xie_beni[k] <- XB(feature_dataset_fqa, 
                    clustering_fqa$U, clustering_fqa$H, m = 2)
  
  k <- k + 1
  print(j1)
  
}

which.min(xie_beni)


features_k_m <- list()
set.seed(1234)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

feature_matrix_km <- lapply(fts_list, aucors_kendall_m, lag = lags)
feature_dataset_km <- cbind(list_to_matrix(feature_matrix_km), rep(0, 40))
feature_dataset_km[is.na(feature_dataset_km)] <- 0

for (j1 in vector_m) {
  
  # dis_matrix_km <- as.matrix(proxy::dist(feature_dataset_km)^2)
  
  clustering_km <- FKM(feature_dataset_km, k = 2, m = j1)
  xie_beni[k] <- XB(feature_dataset_km, 
                    clustering_km$U, clustering_km$H, m = 2)
  
  k <- k + 1
  print(j1)
  
}

which.min(xie_beni)



features_k_i <- list()
set.seed(1234)
lags <- c(1)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
l_m <- length(vector_m)
k <- 1
xie_beni <- numeric()

feature_matrix_ki <- lapply(fts_list, aucors_kendall_i, lag = lags)
feature_dataset_ki <- cbind(list_to_matrix(feature_matrix_ki), rep(0, 40))
feature_dataset_ki[is.na(feature_dataset_ki)] <- 0

for (j1 in vector_m) {
  
  
  # dis_matrix_ki <- as.matrix(proxy::dist(feature_dataset_ki)^2)
  
  clustering_ki <- FKM(feature_dataset_ki, k = 2, m = j1)
  xie_beni[k] <- XB(feature_dataset_ki, 
                    clustering_ki$U, clustering_ki$H, m = 2)
  
  k <- k + 1
  print(j1)
  
}

which.min(xie_beni)


# Performing feature extraction

sed <- function(x, y) {sum((x - y)^2)}
fts_list <- c(fts_f, fts_u)
K <- 2
ground_truth <- c(rep(1, length(fts_f)), rep(2, length(fts_u)))
feature_matrix <- lapply(fts_list, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1))
feature_dataset <- list_to_matrix(feature_matrix)
feature_dataset[is.na(feature_dataset)] <- 0

set.seed(113)
cl <- fuzzy_c_medoids(X = feature_dataset_fqa, m = 1.8, C = K, dis = sed)$U
ARI.F(ground_truth, cl); JACCARD.F(ground_truth, cl)
external_validation(fuzzytocrisp(cl), ground_truth)
external_validation(fuzzytocrisp(cl), ground_truth, method = 'jaccard_index')
set.seed(113)
cl_aux <- fuzzy_c_medoids(X = feature_dataset, m = 1.8, C = K, dis = sed)
ARI.F(ground_truth, cl_aux$U)
cl_aux$of[cl_aux$of == Inf] <- 0
sum(cl_aux$of)


dis_matrix_f_1 <- (1/(4 * 2 * 3^2)) * dist(list_to_matrix(feature_matrix))^2

plot_1 <- plot_2d_scaling(dis_matrix_f_1, cluster_labels = ground_truth)$plot + 
  theme(axis.title = element_text(size = 11), axis.text = element_text(size = 10),
        legend.position = 'bottom', plot.title = element_text(size = 12), legend.text = element_text(size=10)) +
  scale_color_discrete(labels = c('CS sector', 'EN sector'))





feature_matrix_k_m <- lapply(fts_list, aucors_kendall_m, lags = c(1))
feature_dataset_k_m <- list_to_matrix(feature_matrix_k_m)
feature_matrix_k_m[is.na(feature_matrix_k_m)] <- 0

set.seed(123)
cl <- fuzzy_c_medoids(X = cbind(feature_dataset_k_m, rep(0, 40)), m = 2, C = K, dis = sed)$U
ARI.F(ground_truth, cl)
external_validation(fuzzytocrisp(cl), ground_truth)



feature_matrix_k_i <- lapply(fts_list, aucors_kendall_i, lags = c(1))
feature_dataset_k_i <- list_to_matrix(feature_matrix_k_i)
feature_matrix_k_i[is.na(feature_matrix_k_i)] <- 0

cl <- fuzzy_c_medoids(X = cbind(feature_dataset_k_i, rep(0, 40)), m = 2, C = K, dis = sed)$U
ARI.F(ground_truth, cl)


# Plots

hours <- seq(9.5, 16, by = 1/12)
par(mfrow = c(1,1))

series_1_smoothed <- t(apply(fts_f_1[1 : 100,], 1, c))
plot(fdata(series_1_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-returns', main = 'GOOGL',
     ylim = c(-0.011, 0.011))

savefig('series_1_smoothed', width=12, height=10, toplines=0.8, type= 'png')
plot(fdata(series_1_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-returns', main = 'GOOGL',
     ylim = c(-0.011, 0.011))
dev.off()


series_2_smoothed <- t(apply(fts_u_1[1 : 100,], 1, c))
plot(fdata(series_2_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-returns', main = 'APA',
     ylim = c(-0.011, 0.011))


savefig('series_2_smoothed', width=12, height=10, toplines=0.8, type= 'png')
plot(fdata(series_2_smoothed, argvals = hours[-c(1, 2)]), xlab = 'Trading time', ylab = 'Log-returns', main = 'APA',
     ylim = c(-0.011, 0.011))
dev.off()


create_sp_fts <- function(df) {
  
  library(data.table)
  
  # Retaining only the times of interest
  
  wanted_strings <- c('14:3', '14:4', '14:5', 'T15:', '16:', '17:',
                      '18:', '19:', 'T20:', '21:00', '21:05')
  indexes_1 <- which(df$Date.Time %like% wanted_strings[1])
  indexes_2 <- which(df$Date.Time %like% wanted_strings[2])
  indexes_3 <- which(df$Date.Time %like% wanted_strings[3])
  indexes_4 <- which(df$Date.Time %like% wanted_strings[4])
  indexes_5 <- which(df$Date.Time %like% wanted_strings[5])
  indexes_6 <- which(df$Date.Time %like% wanted_strings[6])
  indexes_7 <- which(df$Date.Time %like% wanted_strings[7])
  indexes_8 <- which(df$Date.Time %like% wanted_strings[8])
  indexes_9 <- which(df$Date.Time %like% wanted_strings[9])
  indexes_10 <- which(df$Date.Time %like% wanted_strings[10])
  indexes_11 <- which(df$Date.Time %like% wanted_strings[11])
  
  indexes <- c(indexes_1, indexes_2, indexes_3, indexes_4, indexes_5,
               indexes_6, indexes_7, indexes_8, indexes_9, indexes_10, indexes_11)
  
  df_mod <- df[indexes,]
  
  df_mod$Price <- (df_mod$Close.Bid + df_mod$Close.Ask)/2
  df_new <- df_mod[, -c(1, 3, 4)]
  df_new$Date.Time <- as.Date(df_mod$Date.Time)
  
  
  # Vector of different days
  
  vector_days <- unique(df_new$Date.Time)
  
  
  # Length of the series
  
  l_series <- length(vector_days)
  
  
  # Indexes of each day
  
  indexes <- list()
  
  for (i in 1 : l_series) {
    
    indexes[[i]] <- which(df_new$Date.Time == vector_days[i])
    
  }
  
  
  # Creating the FTS
  
  fts <- list()
  
  for (i in 1 : l_series) {
    
    fts[[i]] <- df_new$Price[indexes[[i]]]
    
  }
  
  
  for (i in 1 : l_series) {
    
    if (length(fts[[i]]) < 78) {
      
      remaining <- 78 - length(fts[[i]])
      fts[[i]] <- c(fts[[i]], rep(0, remaining))
      
    }
    
    if (length(fts[[i]]) > 78) {
      
      left <- length(fts[[i]]) - 78
      fts[[i]] <- fts[[i]][- (1 : left)]
      
    }
    
  }
  
  fts_matrix <- list_to_matrix(fts)
  
  for (i in 1 : l_series) {
    
    indexes_0 <- which(fts_matrix[i,] == 0)
    fts_matrix[i,][indexes_0] <- mean(fts_matrix[i,])
    
  }
  
  fts_idcr <- t(apply(fts_matrix, 1, idcr))
  # fts_idcr[fts_idcr == -Inf] <- 0
  fts_idcr[fts_idcr < - 0.01] <- 0
  fts_idcr[fts_idcr > 0.01] <- 0
  
  return(fts_idcr)
  
}


# Plot medoids


matrix_1 <- matrix(0, 40, 9)

for (i in 1 : 40) {

matrix_1[i, 1] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
matrix_1[i, 2] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
matrix_1[i, 3] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
matrix_1[i, 4] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
matrix_1[i, 5] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
matrix_1[i, 6] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
matrix_1[i, 7] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
matrix_1[i, 8] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
matrix_1[i, 9] <- cl[i, 1] * aucor_functional(fts_list[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)

}

vector_1 <- colSums((matrix_1))/sum(cl[,1])




matrix_2 <- matrix(0, 40, 9)

for (i in 1 : 40) {
  
  matrix_2[i, 1] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
  matrix_2[i, 2] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
  matrix_2[i, 3] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
  matrix_2[i, 4] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
  matrix_2[i, 5] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
  matrix_2[i, 6] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
  matrix_2[i, 7] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
  matrix_2[i, 8] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
  matrix_2[i, 9] <- cl[i, 2] * aucor_functional(fts_list[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)
  
}

vector_2 <- colSums((matrix_2))/sum(cl[,2])

c_vector <- c('1', '1', '1', '2', '2', '2')
cqa_vector_1 <- c(vector_1[c(1, 4, 7)], vector_2[c(1, 4, 7)])
cqa_vector_2 <- c(vector_1[c(2, 5, 8)], vector_2[c(2, 5, 8)])
cqa_vector_3 <- c(vector_1[c(3, 6, 9)], vector_2[c(3, 6, 9)])

df_1 <- data.frame(tau_2 = c_vector, tau_1 = cqa_vector_1, x = rep(c(0.1, 0.5, 0.9), 2))
df_2 <- data.frame(tau_2 = c_vector, tau_1 = cqa_vector_2, x = rep(c(0.1, 0.5, 0.9), 2))
df_3 <- data.frame(tau_2 = c_vector, tau_1 = cqa_vector_3, x = rep(c(0.1, 0.5, 0.9), 2))

plot_1_prev <- ggplot(df_1, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-0.7, 0.7)) + ggtitle(TeX('$\\tau_2=0.1$')) +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,  size = 18),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('Cluster 1'), TeX('Cluster 2'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$'))  

shared_legend <- extract_legend(plot_1_prev)

plot_1 <- ggplot(df_1, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-0.7, 0.7)) + ggtitle(TeX('$\\tau_2=0.1$')) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,  size = 13),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('$\\tau_2=0.1$'), TeX('$\\tau_2=0.5$'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$')) + ylab(TeX('$\\bar{\\rho}(\\tau_1, 0.1, 1)$')) +
  scale_x_continuous(breaks = c(0.1, 0.5, 0.9)) 
 

plot_2 <- ggplot(df_2, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-0.35, 0.35)) + ggtitle(TeX('$\\tau_2=0.5$')) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,  size = 13),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('$\\tau_2=0.1$'), TeX('$\\tau_2=0.5$'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$')) + ylab(TeX('$\\bar{\\rho}(\\tau_1, 0.5, 1)$')) +
  scale_x_continuous(breaks = c(0.1, 0.5, 0.9)) 
 
 


plot_3 <- ggplot(df_3, aes(x = x, y = tau_1, col = tau_2)) + geom_line(size = 0.8) +
  geom_point(size = 1.5) + ylim(c(-0.7, 0.7)) + ggtitle(TeX('$\\tau_2=0.9$')) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,  size = 13),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = c(TeX('$\\tau_2=0.1$'), TeX('$\\tau_2=0.5$'), TeX('$\\tau_2=0.9$'))) +
  xlab(TeX('$\\tau_1$')) + ylab(TeX('$\\bar{\\rho}(\\tau_1, 0.9, 1)$')) +
  scale_x_continuous(breaks = c(0.1, 0.5, 0.9)) 
 

plot_total <- grid.arrange(plot_1, plot_2, plot_3, ncol = 3)

plot_final <- grid.arrange(
  arrangeGrob(plot_total), nrow = 2, ncol = 1, shared_legend,
  heights = c(40, 8))


