

# Financials sector

getSymbols("AFL", from = "2000/01/01", to = "2023/12/31")
series_1 <- t(apply(f_dataset(AFL)[, 1 : 20], 1, imcr))

getSymbols("ALL", from = "2000/01/01", to = "2023/12/31")
series_2 <- t(apply(f_dataset(ALL)[, 1 : 20], 1, imcr))

getSymbols("AXP", from = "2000/01/01", to = "2023/12/31")
series_3 <- t(apply(f_dataset(AXP)[, 1 : 20], 1, imcr))

getSymbols("AIG", from = "2000/01/01", to = "2023/12/31")
series_4 <- t(apply(f_dataset(AIG)[, 1 : 20], 1, imcr))

getSymbols("BLK", from = "2000/01/01", to = "2023/12/31")
series_5 <- t(apply(f_dataset(BLK)[, 1 : 20], 1, imcr))

getSymbols("AON", from = "2000/01/01", to = "2023/12/31")
series_6 <- t(apply(f_dataset(AON)[, 1 : 20], 1, imcr))

getSymbols("ACGL", from = "2000/01/01", to = "2023/12/31")
series_7 <- t(apply(f_dataset(ACGL)[, 1 : 20], 1, imcr))

getSymbols("AJG", from = "2000/01/01", to = "2023/12/31")
series_8 <- t(apply(f_dataset(AJG)[, 1 : 20], 1, imcr))

getSymbols("C", from = "2000/01/01", to = "2023/12/31")
series_9 <- t(apply(f_dataset(C)[, 1 : 20], 1, imcr))

getSymbols("BAC", from = "2000/01/01", to = "2023/12/31")
series_10 <- t(apply(f_dataset(BAC)[, 1 : 20], 1, imcr))

getSymbols("ZION", from = "2000/01/01", to = "2023/12/31")
series_11 <- t(apply(f_dataset(ZION)[, 1 : 20], 1, imcr))

getSymbols("ABT", from = "2000/01/01", to = "2023/12/31")
series_12 <- t(apply(f_dataset(ABT)[, 1 : 20], 1, imcr))

getSymbols("A", from = "2000/01/01", to = "2023/12/31")
series_13 <- t(apply(f_dataset(A)[, 1 : 20], 1, imcr))

getSymbols("BAX", from = "2000/01/01", to = "2023/12/31")
series_14 <- t(apply(f_dataset(BAX)[, 1 : 20], 1, imcr))

getSymbols("AMGN", from = "2000/01/01", to = "2023/12/31")
series_15 <- t(apply(f_dataset(AMGN)[, 1 : 20], 1, imcr))

getSymbols("L", from = "2000/01/01", to = "2023/12/31")
series_16 <- t(apply(f_dataset(L)[, 1 : 20], 1, imcr))

getSymbols("MCO", from = "2000/01/01", to = "2023/12/31")
series_17 <- t(apply(f_dataset(MCO)[, 1 : 20], 1, imcr))

getSymbols("MS", from = "2000/01/01", to = "2023/12/31")
series_18 <- t(apply(f_dataset(MS)[, 1 : 20], 1, imcr))

getSymbols("KEY", from = "2000/01/01", to = "2023/12/31")
series_19 <- t(apply(f_dataset(KEY)[, 1 : 20], 1, imcr))

getSymbols("HIG", from = "2000/01/01", to = "2023/12/31")
series_20 <- t(apply(f_dataset(HIG)[, 1 : 20], 1, imcr))

series_l1 <- list(series_1, series_2, series_3, series_4, series_5, series_6, series_7, series_8, series_9, series_10, 
                  series_11, series_12, series_13, series_14, series_15, series_16, series_17, series_18, series_19, series_20)  

# Utilities sector

getSymbols("AES", from = "2000/01/01", to = "2023/12/31")
series_1 <- t(apply(f_dataset(AES)[, 1 : 20], 1, imcr))

getSymbols("LNT", from = "2000/01/01", to = "2023/12/31")
series_2 <- t(apply(f_dataset(LNT)[, 1 : 20], 1, imcr))

getSymbols("AEE", from = "2000/01/01", to = "2023/12/31")
series_3 <- t(apply(f_dataset(AEE)[, 1 : 20], 1, imcr))

getSymbols("AEP", from = "2000/01/01", to = "2023/12/31")
series_4 <- t(apply(f_dataset(AEP)[, 1 : 20], 1, imcr))

getSymbols("D", from = "2000/01/01", to = "2023/12/31")
series_5 <- t(apply(f_dataset(D)[, 1 : 20], 1, imcr))

getSymbols("ATO", from = "2000/01/01", to = "2023/12/31")
series_6 <- t(apply(f_dataset(ATO)[, 1 : 20], 1, imcr))

getSymbols("CNP", from = "2000/01/01", to = "2023/12/31")
series_7 <- t(apply(f_dataset(CNP)[, 1 : 20], 1, imcr))

getSymbols("DUK", from = "2000/01/01", to = "2023/12/31")
series_8 <- t(apply(f_dataset(DUK)[, 1 : 20], 1, imcr))

getSymbols("EIX", from = "2000/01/01", to = "2023/12/31")
series_9 <- t(apply(f_dataset(EIX)[, 1 : 20], 1, imcr))

getSymbols("FE", from = "2000/01/01", to = "2023/12/31")
series_10 <- t(apply(f_dataset(FE)[, 1 : 20], 1, imcr))

getSymbols("XEL", from = "2000/01/01", to = "2023/12/31")
series_11 <- t(apply(f_dataset(XEL)[, 1 : 20], 1, imcr))

getSymbols("WEC", from = "2000/01/01", to = "2023/12/31")
series_12 <- t(apply(f_dataset(WEC)[, 1 : 20], 1, imcr))

getSymbols("SO", from = "2000/01/01", to = "2023/12/31")
series_13 <- t(apply(f_dataset(SO)[, 1 : 20], 1, imcr))

getSymbols("SRE", from = "2000/01/01", to = "2023/12/31")
series_14 <- t(apply(f_dataset(SRE)[, 1 : 20], 1, imcr))

getSymbols("PEG", from = "2000/01/01", to = "2023/12/31")
series_15 <- t(apply(f_dataset(PEG)[, 1 : 20], 1, imcr))

getSymbols("PPL", from = "2000/01/01", to = "2023/12/31")
series_16 <- t(apply(f_dataset(PPL)[, 1 : 20], 1, imcr))

getSymbols("PNW", from = "2000/01/01", to = "2023/12/31")
series_17 <- t(apply(f_dataset(PNW)[, 1 : 20], 1, imcr))

getSymbols("PCG", from = "2000/01/01", to = "2023/12/31")
series_18 <- t(apply(f_dataset(PCG)[, 1 : 20], 1, imcr))

getSymbols("ES", from = "2000/01/01", to = "2023/12/31")
series_19 <- t(apply(f_dataset(ES)[, 1 : 20], 1, imcr))

getSymbols("EXC", from = "2000/01/01", to = "2023/imcr12/31")
series_20 <- t(apply(f_dataset(EXC)[, 1 : 20], 1, imcr))


series_l2 <- list(series_1, series_2, series_3, series_4, series_5, series_6, series_7, series_8, series_9, series_10, 
                  series_11, series_12, series_13, series_14, series_15, series_16, series_17, series_18, series_19, series_20)  

list_series <- c(series_l1, series_l2)

feature_matrix <- lapply(list_series, aucors_functional, levels = c(0.1, 0.5, 0.9), lags = c(1))
feature_dataset <- list_to_matrix(feature_matrix)
feature_dataset[is.na(feature_dataset)] <- 0

clustering <- kmeans(feature_dataset, 2)$cluster
ground_truth <- c(rep(1, 20), rep(2, 20))
external_validation(ground_truth, clustering); clustering
