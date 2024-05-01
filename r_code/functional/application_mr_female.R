

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/PostDoc/papers/clustering_functional/applications/application_mr')

australia <- read.table('australia.txt', header = T, na.strings = '.')
austria <- read.table('austria.txt', header = T, na.strings = '.')
belarus <- read.table('belarus.txt', header = T, na.strings = '.')
belgium <- read.table('belgium.txt', header = T, na.strings = '.')
bulgaria <- read.table('bulgaria.txt', header = T, na.strings = '.')
canada <- read.table('canada.txt', header = T, na.strings = '.')
chile <- read.table('chile.txt', header = T, na.strings = '.')
croatia <- read.table('croatia.txt', header = T, na.strings = '.')
czechia <- read.table('czechia.txt', header = T, na.strings = '.')
denmark <- read.table('denmark.txt', header = T, na.strings = '.')
estonia <- read.table('estonia.txt', header = T, na.strings = '.')
finland <- read.table('finland.txt', header = T, na.strings = '.')
france <- read.table('france.txt', header = T, na.strings = '.')
germany <- read.table('germany.txt', header = T, na.strings = '.')
greece <- read.table('greece.txt', header = T, na.strings = '.')
hong_kong <- read.table('hong_kong.txt', header = T, na.strings = '.')
hungary <- read.table('hungary.txt', header = T, na.strings = '.')
iceland <- read.table('iceland.txt', header = T, na.strings = '.')
ireland <- read.table('ireland.txt', header = T, na.strings = '.')
israel <- read.table('israel.txt', header = T, na.strings = '.')
italy <- read.table('italy.txt', header = T, na.strings = '.')
japan <- read.table('japan.txt', header = T, na.strings = '.')
korea <- read.table('korea.txt', header = T, na.strings = '.')
latvia <- read.table('latvia.txt', header = T, na.strings = '.')
lithuania <- read.table('lithuania.txt', header = T, na.strings = '.')
luxembourg <- read.table('luxembourg.txt', header = T, na.strings = '.')
netherlands <- read.table('netherlands.txt', header = T, na.strings = '.')
new_zealand <- read.table('new_zealand.txt', header = T, na.strings = '.')
norway <- read.table('norway.txt', header = T, na.strings = '.')
poland <- read.table('poland.txt', header = T, na.strings = '.')
portugal <- read.table('portugal.txt', header = T, na.strings = '.')
russia <- read.table('russia.txt', header = T, na.strings = '.')
slovakia <- read.table('slovakia.txt', header = T, na.strings = '.')
slovenia <- read.table('slovenia.txt', header = T, na.strings = '.')
spain <- read.table('spain.txt', header = T, na.strings = '.')
sweden <- read.table('sweden.txt', header = T, na.strings = '.')
switzerland <- read.table('switzerland.txt', header = T, na.strings = '.')
taiwan <- read.table('taiwan.txt', header = T, na.strings = '.')
uk <- read.table('uk.txt', header = T, na.strings = '.')
ukraine <- read.table('ukraine.txt', header = T, na.strings = '.')
usa <- read.table('usa.txt', header = T, na.strings = '.')

list_1 <- list(australia, austria, belarus, belgium, bulgaria, canada, chile,
               croatia, czechia, denmark, estonia, finland, france, germany,
               greece, hong_kong, hungary, iceland, ireland, israel, italy, 
               japan, korea, latvia, lithuania, luxembourg, netherlands, new_zealand,
               norway, poland, portugal, russia, slovakia, slovenia, spain,
               sweden, switzerland, taiwan, uk, ukraine, usa)

australia_p <- read.table('australia_p.txt', header = T, na.strings = '.')
austria_p <- read.table('austria_p.txt', header = T, na.strings = '.')
belarus_p <- read.table('belarus_p.txt', header = T, na.strings = '.')
belgium_p <- read.table('belgium_p.txt', header = T, na.strings = '.')
bulgaria_p <- read.table('bulgaria_p.txt', header = T, na.strings = '.')
canada_p <- read.table('canada_p.txt', header = T, na.strings = '.')
chile_p <- read.table('chile_p.txt', header = T, na.strings = '.')
croatia_p <- read.table('croatia_p.txt', header = T, na.strings = '.')
czechia_p <- read.table('czechia_p.txt', header = T, na.strings = '.')
denmark_p <- read.table('denmark_p.txt', header = T, na.strings = '.')
estonia_p <- read.table('estonia_p.txt', header = T, na.strings = '.')
finland_p <- read.table('finland_p.txt', header = T, na.strings = '.')
france_p <- read.table('france_p.txt', header = T, na.strings = '.')
germany_p <- read.table('germany_p.txt', header = T, na.strings = '.')
greece_p <- read.table('greece_p.txt', header = T, na.strings = '.')
hong_kong_p <- read.table('hong_kong_p.txt', header = T, na.strings = '.')
hungary_p <- read.table('hungary_p.txt', header = T, na.strings = '.')
iceland_p <- read.table('iceland_p.txt', header = T, na.strings = '.')
ireland_p <- read.table('ireland_p.txt', header = T, na.strings = '.')
israel_p <- read.table('israel_p.txt', header = T, na.strings = '.')
italy_p <- read.table('italy_p.txt', header = T, na.strings = '.')
japan_p <- read.table('japan_p.txt', header = T, na.strings = '.')
korea_p <- read.table('korea_p.txt', header = T, na.strings = '.')
latvia_p <- read.table('latvia_p.txt', header = T, na.strings = '.')
lithuania_p <- read.table('lithuania_p.txt', header = T, na.strings = '.')
luxembourg_p <- read.table('luxembourg_p.txt', header = T, na.strings = '.')
netherlands_p <- read.table('netherlands_p.txt', header = T, na.strings = '.')
new_zealand_p <- read.table('new_zealand_p.txt', header = T, na.strings = '.')
norway_p <- read.table('norway_p.txt', header = T, na.strings = '.')
poland_p <- read.table('poland_p.txt', header = T, na.strings = '.')
portugal_p <- read.table('portugal_p.txt', header = T, na.strings = '.')
russia_p <- read.table('russia_p.txt', header = T, na.strings = '.')
slovakia_p <- read.table('slovakia_p.txt', header = T, na.strings = '.')
slovenia_p <- read.table('slovenia_p.txt', header = T, na.strings = '.')
spain_p <- read.table('spain_p.txt', header = T, na.strings = '.')
sweden_p <- read.table('sweden_p.txt', header = T, na.strings = '.')
switzerland_p <- read.table('switzerland_p.txt', header = T, na.strings = '.')
taiwan_p <- read.table('taiwan_p.txt', header = T, na.strings = '.')
uk_p <- read.table('uk_p.txt', header = T, na.strings = '.')
ukraine_p <- read.table('ukraine_p.txt', header = T, na.strings = '.')
usa_p <- read.table('usa_p.txt', header = T, na.strings = '.')

list_1_p <- list(australia_p, austria_p, belarus_p, belgium_p, bulgaria_p, canada_p, chile_p,
                 croatia_p, czechia_p, denmark_p, estonia_p, finland_p, france_p, germany_p,
                 greece_p, hong_kong_p, hungary_p, iceland_p, ireland_p, israel_p, italy_p, 
                 japan_p, korea_p, latvia_p, lithuania_p, luxembourg_p, netherlands_p, new_zealand_p,
                 norway_p, poland_p, portugal_p, russia_p, slovakia_p, slovenia_p, spain_p,
                 sweden_p, switzerland_p, taiwan_p, uk_p, ukraine_p, usa_p)


list_2 <- list()
list_2_p <- list()

for (i in 1 : 41) {
  
  list_2[[i]] <- list_1[[i]][list_1[[i]]$Year >= 1960, c(1, 2, 3)]
  list_2_p[[i]] <- list_1_p[[i]][list_1_p[[i]]$Year >= 1960, c(1, 2, 3)]
  
}


# List of matrices for mortality rates and population

list_3 <- list()
list_3_p <- list()
years <- list()

for (i in 1 : 41) {
  
  years[[i]] <- unique(list_2[[i]]$Year)
  list_3[[i]] <- matrix(0, length(years[[i]]), 111)
  list_3_p[[i]] <- matrix(0, length(years[[i]]), 111)
  
  for (j in 1 : length(years[[i]])) {
    
    indexes_j <- which(list_2[[i]]$Year == years[[i]][j])
    list_3[[i]][j,] <- list_2[[i]]$Female[indexes_j]
    list_3_p[[i]][j,] <- list_2_p[[i]]$Female[indexes_j]  
    
  }
  
}

# Smoothing the data

list_4 <- list()
list_4_smoothed <- list()

for (i in 1 : 41) {
  
  list_4[[i]] <- demogdata(t(list_3[[i]]), pop = t(list_3_p[[i]]), ages = 0 : 110, years = years[[i]], type = 'mortality', label = '', name = '')
  list_4_smoothed[[i]] <- t(smooth.demogdata(list_4[[i]])[[3]][[1]])
  
}






list_5 <- list()
list_5_smoothed <- list()

for (i in 1 : 41) {
  
  list_5[[i]] <- transformed_mr(list_3[[i]])
  list_5_smoothed[[i]] <- transformed_mr(list_4_smoothed[[i]])
  
}


list_fts <- list_5_smoothed


# Plots

ages <- seq(0, 110, by = 1)
par(mfrow = c(1,1))

series_1_smoothed <- list_fts[[22]]
plot(fdata(series_1_smoothed, argvals = ages), xlab = 'Age', ylab = 'Mortality improvement rate', main = 'Japan')

savefig('series_1_smoothed', width=12, height=10, toplines=0.8, type= 'png')
plot(fdata(series_1_smoothed, argvals = ages), xlab = 'Age', ylab = 'Mortality improvement rate', main = 'Japan',
     ylim = c(-0.5, 0.5))
dev.off()


series_2_smoothed <- list_fts[[32]]
plot(fdata(series_2_smoothed, argvals = ages), xlab = 'Age', ylab = 'Mortality improvement rate', main = 'Russia')


savefig('series_2_smoothed', width=12, height=10, toplines=0.8, type= 'png')
plot(fdata(series_2_smoothed, argvals = ages), xlab = 'Age', ylab = '', main = 'Russia',
     ylim = c(-0.5, 0.5))
dev.off()


# Clustering

list_fts <- list_5_smoothed
list_fts <- list_fts[-c(7, 8, 14, 15, 16,  20, 23, 34)]



# Selection of the set of lags


test_1 <- test_functional(list_fts[[1]], lags = seq(1, 10))
test_2 <- test_functional(list_fts[[2]], lags = seq(1, 10))
test_3 <- test_functional(list_fts[[3]], lags = seq(1, 10))
test_4 <- test_functional(list_fts[[4]], lags = seq(1, 10))
test_5 <- test_functional(list_fts[[5]], lags = seq(1, 10))
test_6 <- test_functional(list_fts[[6]], lags = seq(1, 10))
test_7 <- test_functional(list_fts[[7]], lags = seq(1, 10))
test_8 <- test_functional(list_fts[[8]], lags = seq(1, 10))
test_9 <- test_functional(list_fts[[9]], lags = seq(1, 10))
test_10 <- test_functional(list_fts[[10]], lags = seq(1, 10))
test_11 <- test_functional(list_fts[[11]], lags = seq(1, 10))
test_12 <- test_functional(list_fts[[12]], lags = seq(1, 10))
test_13 <- test_functional(list_fts[[13]], lags = seq(1, 10))
test_14 <- test_functional(list_fts[[14]], lags = seq(1, 10))
test_15 <- test_functional(list_fts[[15]], lags = seq(1, 10))
test_16 <- test_functional(list_fts[[16]], lags = seq(1, 10))
test_17 <- test_functional(list_fts[[17]], lags = seq(1, 10))
test_18 <- test_functional(list_fts[[18]], lags = seq(1, 10))
test_19 <- test_functional(list_fts[[19]], lags = seq(1, 10))
test_20 <- test_functional(list_fts[[20]], lags = seq(1, 10))
test_21 <- test_functional(list_fts[[21]], lags = seq(1, 10))
test_22 <- test_functional(list_fts[[22]], lags = seq(1, 10))
test_23 <- test_functional(list_fts[[23]], lags = seq(1, 10))
test_24 <- test_functional(list_fts[[24]], lags = seq(1, 10))
test_25 <- test_functional(list_fts[[25]], lags = seq(1, 10))
test_26 <- test_functional(list_fts[[26]], lags = seq(1, 10))
test_27 <- test_functional(list_fts[[27]], lags = seq(1, 10))
test_28 <- test_functional(list_fts[[28]], lags = seq(1, 10))
test_29 <- test_functional(list_fts[[29]], lags = seq(1, 10))
test_30 <- test_functional(list_fts[[30]], lags = seq(1, 10))
test_31 <- test_functional(list_fts[[31]], lags = seq(1, 10))
test_32 <- test_functional(list_fts[[32]], lags = seq(1, 10))
test_33 <- test_functional(list_fts[[33]], lags = seq(1, 10))


# Selecting the hyperparameters C and m

features_fqa <- list()
set.seed(1234)
lags <- c(1, 2)
l_lags <- length(lags)
vector_m <- seq(1.1, 2, 0.1)
vector_c <- 2 : 6
l_m <- length(vector_m)
l_c <- length(vector_c)


feature_matrix_fqa <- lapply(list_fts, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = lags)
feature_dataset_fqa <- list_to_matrix(feature_matrix_fqa)
feature_dataset_fqa[is.na(feature_dataset_fqa)] <- 0

k <- 1
xie_beni <- numeric()

for (j1 in vector_m) {
  
  for (j2 in vector_c) {
    
    dis_matrix_fqa <- as.matrix(proxy::dist(feature_dataset_fqa)^2)
    
    clustering_fqa <- FKM((feature_dataset_fqa), k = j2, m = j1)
    xie_beni[k] <- XB(feature_dataset_fqa, 
                      clustering_fqa$U, clustering_fqa$H, m = 2)
    
    k <- k + 1
    print(j1)
    
  }
  
}

which.min(xie_beni)

set.seed(1234)
clustering_km <- fuzzy_c_medoids(feature_dataset_fqa, C = 6, m = 1.3, dis = sed)
cl <- clustering_km$U


# Summarizing the clusters

matrix_1 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_1[i, 1] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
  matrix_1[i, 2] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
  matrix_1[i, 3] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
  matrix_1[i, 4] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
  matrix_1[i, 5] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
  matrix_1[i, 6] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
  matrix_1[i, 7] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
  matrix_1[i, 8] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
  matrix_1[i, 9] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)
  
}

vector_1 <- colSums((matrix_1))/sum(cl[,1])




matrix_2 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_2[i, 1] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
  matrix_2[i, 2] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
  matrix_2[i, 3] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
  matrix_2[i, 4] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
  matrix_2[i, 5] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
  matrix_2[i, 6] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
  matrix_2[i, 7] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
  matrix_2[i, 8] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
  matrix_2[i, 9] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)
  
}

vector_2 <- colSums((matrix_2))/sum(cl[,2])


matrix_3 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_3[i, 1] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
  matrix_3[i, 2] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
  matrix_3[i, 3] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
  matrix_3[i, 4] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
  matrix_3[i, 5] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
  matrix_3[i, 6] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
  matrix_3[i, 7] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
  matrix_3[i, 8] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
  matrix_3[i, 9] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)
  
}

vector_3 <- colSums((matrix_3))/sum(cl[,3])


matrix_4 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_4[i, 1] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
  matrix_4[i, 2] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
  matrix_4[i, 3] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
  matrix_4[i, 4] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
  matrix_4[i, 5] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
  matrix_4[i, 6] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
  matrix_4[i, 7] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
  matrix_4[i, 8] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
  matrix_4[i, 9] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)
  
}

vector_4 <- colSums((matrix_4))/sum(cl[,4])


matrix_5 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_5[i, 1] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
  matrix_5[i, 2] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
  matrix_5[i, 3] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
  matrix_5[i, 4] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
  matrix_5[i, 5] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
  matrix_5[i, 6] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
  matrix_5[i, 7] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
  matrix_5[i, 8] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
  matrix_5[i, 9] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)
  
}

vector_5 <- colSums((matrix_5))/sum(cl[,5])


matrix_6 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_6[i, 1] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 1)
  matrix_6[i, 2] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 1) 
  matrix_6[i, 3] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 1)
  matrix_6[i, 4] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 1)
  matrix_6[i, 5] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 1)
  matrix_6[i, 6] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 1)
  matrix_6[i, 7] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 1)
  matrix_6[i, 8] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 1)
  matrix_6[i, 9] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 1)
  
}

vector_6 <- colSums((matrix_6))/sum(cl[,6])



matrix_7 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_7[i, 1] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 2)
  matrix_7[i, 2] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 2) 
  matrix_7[i, 3] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 2)
  matrix_7[i, 4] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 2)
  matrix_7[i, 5] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 2)
  matrix_7[i, 6] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 2)
  matrix_7[i, 7] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 2)
  matrix_7[i, 8] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 2)
  matrix_7[i, 9] <- cl[i, 1] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 2)
  
}

vector_7 <- colSums((matrix_7))/sum(cl[,1])




matrix_8 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_8[i, 1] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 2)
  matrix_8[i, 2] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 2) 
  matrix_8[i, 3] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 2)
  matrix_8[i, 4] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 2)
  matrix_8[i, 5] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 2)
  matrix_8[i, 6] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 2)
  matrix_8[i, 7] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 2)
  matrix_8[i, 8] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 2)
  matrix_8[i, 9] <- cl[i, 2] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 2)
  
}

vector_8 <- colSums((matrix_8))/sum(cl[,2])


matrix_9 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_9[i, 1] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 2)
  matrix_9[i, 2] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 2) 
  matrix_9[i, 3] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 2)
  matrix_9[i, 4] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 2)
  matrix_9[i, 5] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 2)
  matrix_9[i, 6] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 2)
  matrix_9[i, 7] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 2)
  matrix_9[i, 8] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 2)
  matrix_9[i, 9] <- cl[i, 3] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 2)
  
}

vector_9 <- colSums((matrix_9))/sum(cl[,3])


matrix_10 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_10[i, 1] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 2)
  matrix_10[i, 2] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 2) 
  matrix_10[i, 3] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 2)
  matrix_10[i, 4] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 2)
  matrix_10[i, 5] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 2)
  matrix_10[i, 6] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 2)
  matrix_10[i, 7] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 2)
  matrix_10[i, 8] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 2)
  matrix_10[i, 9] <- cl[i, 4] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 2)
  
}

vector_10 <- colSums((matrix_10))/sum(cl[,4])


matrix_11 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_11[i, 1] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 2)
  matrix_11[i, 2] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 2) 
  matrix_11[i, 3] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 2)
  matrix_11[i, 4] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 2)
  matrix_11[i, 5] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 2)
  matrix_11[i, 6] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 2)
  matrix_11[i, 7] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 2)
  matrix_11[i, 8] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 2)
  matrix_11[i, 9] <- cl[i, 5] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 2)
  
}

vector_11 <- colSums((matrix_11))/sum(cl[,5])


matrix_12 <- matrix(0, 33, 9)

for (i in 1 : 33) {
  
  matrix_12[i, 1] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.1, lag = 2)
  matrix_12[i, 2] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.5, lag = 2) 
  matrix_12[i, 3] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.1, tau_2 = 0.9, lag = 2)
  matrix_12[i, 4] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.1, lag = 2)
  matrix_12[i, 5] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.5, lag = 2)
  matrix_12[i, 6] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.5, tau_2 = 0.9, lag = 2)
  matrix_12[i, 7] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.1, lag = 2)
  matrix_12[i, 8] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.5, lag = 2)
  matrix_12[i, 9] <- cl[i, 6] * aucor_functional(list_fts[[i]], tau_1 = 0.9, tau_2 = 0.9, lag = 2)
  
}

vector_12 <- colSums((matrix_12))/sum(cl[,6])
