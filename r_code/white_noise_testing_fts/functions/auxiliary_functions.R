
# Load all these functions

simulate_iid_fts <- function(n_points, series_length) {
  # n_points: number of grid points in [0,1]
  # series_length: number of time points (curves)
  #
  # Returns: matrix of dimension series_length x n_points
  #          each row is a curve X_t observed on a common grid
  
  # Common grid (not strictly needed for simulation, but useful to store)
  grid <- seq(0, 1, length.out = n_points)
  
  # i.i.d. Gaussian white noise in L2([0,1]) approximated on the grid:
  # each entry N(0,1), independent over t and u.
  X <- matrix(rnorm(series_length * n_points), 
              nrow = series_length, ncol = n_points)
  
  attr(X, "grid") <- grid
  return(X)
}



simulate_quadratic_iid_fts <- function(n_points, series_length) {
  # n_points: number of grid points in [0,1]
  # series_length: number of time points (curves)
  #
  # Returns: matrix of dimension series_length x n_points
  #          each row is a curve X_t observed on a common grid
  
  # Common grid (not strictly needed for simulation, but useful to store)
  grid <- seq(0, 1, length.out = n_points)
  square <- grid^2
  
  series <- matrix(0, nrow = series_length, ncol = n_points)
  
  for (i in 1 : series_length) {
    
    series[i,] <- square + rnorm(n_points)
    
  }
  
  return(series)
  
}



simulate_quadratic_iid_t_fts <- function(n_points, series_length) {
  # n_points: number of grid points in [0,1]
  # series_length: number of time points (curves)
  #
  # Returns: matrix of dimension series_length x n_points
  #          each row is a curve X_t observed on a common grid
  
  # Common grid (not strictly needed for simulation, but useful to store)
  grid <- seq(0, 1, length.out = n_points)
  square <- grid^2
  
  series <- matrix(0, nrow = series_length, ncol = n_points)
  
  for (i in 1 : series_length) {
    
    series[i,] <- square + rt(n_points, df = 3)
    
  }
  
  return(series)
  
}



simulate_iid_cauchy_fts <- function(n_points, series_length) {
  # n_points: number of grid points in [0,1]
  # series_length: number of time points (curves)
  #
  # Returns: matrix of dimension series_length x n_points
  #          each row is a curve X_t observed on a common grid
  
  # Common grid on [0,1]
  
  grid <- seq(0, 1, length.out = n_points)
  
  # Precompute Fourier basis functions for k = 1,2,3
  
  cos_mat <- sapply(1:3, function(k) cos(2 * pi * k * grid))  # n_points × 3
  sin_mat <- sapply(1:3, function(k) sin(2 * pi * k * grid))  # n_points × 3
  
  # Storage for the series: rows = t, cols = grid points u
  
  series <- matrix(0, nrow = series_length, ncol = n_points)
  
  for (t in 1:series_length) {
    # Draw 7 i.i.d. standard Cauchy variables: Z_{1,t},...,Z_{7,t}
    
    Z <- rcauchy(7)  # Z[1] = Z_{1,t}, Z[2]=Z_{2,t},...,Z[7]=Z_{7,t}
    
    # Constant term Z_{1,t}
    
    Xt <- rep(Z[1], n_points)
    
    # Add sum_{k=1}^3 { Z_{2k,t} cos(2πku) + Z_{2k+1,t} sin(2πku) }
    # For k=1: Z[2], Z[3]; k=2: Z[4], Z[5]; k=3: Z[6], Z[7]
    
    Xt <- Xt +
      cos_mat[, 1] * Z[2] + sin_mat[, 1] * Z[3] +
      cos_mat[, 2] * Z[4] + sin_mat[, 2] * Z[5] +
      cos_mat[, 3] * Z[6] + sin_mat[, 3] * Z[7]
    
    series[t, ] <- Xt
  }
  
  attr(series, "grid") <- grid
  return(series)
  
}



make_phi_mat_gaussian <- function(n_points, c1, c2 = 1) {
  
  u_grid <- seq(0, 1, length.out = n_points)
  Gamma_uv <- outer(u_grid, u_grid, function(u, v) c1 * exp(-c2 * (u^2 + v^2)/2))
  Gamma_uv
  
}


simulate_linear_far1 <- function(series_length, n_points, phi_mat,
                                 burnin = 100) {
  
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi_mat) == c(n_points, n_points))) {
    
    stop("phi_mat must be an n_points x n_points matrix")
    
  }
  
  Phi_w <- phi_mat * dt
  
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rnorm(n_points, 0, 1)
  
  for (t in 2:(series_length + burnin)) {
    
    X[t, ] <- as.vector(Phi_w %*% X[t - 1, ]) + rnorm(n_points, 0, 1)
    
  }
  
  return(X[(burnin + 1):(series_length + burnin),])
  
}


simulate_linear_far1_heavy <- function(series_length, n_points, phi_mat,
                                       burnin = 100) {
  
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi_mat) == c(n_points, n_points))) {
    
    stop("phi_mat must be an n_points x n_points matrix")
    
  }
  
  Phi_w <- phi_mat * dt
  
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rt(n_points, df = 3)   # unscaled t3
  
  for (t in 2:(series_length + burnin)) {
    
    innov_t <- rt(n_points, df = 3)
    X[t, ]  <- as.vector(Phi_w %*% X[t - 1, ]) + innov_t
    
  }
  
  return(X[(burnin + 1):(series_length + burnin), ])
  
}




simulate_linear_far1_bm <- function(series_length, n_points, phi_mat, burnin = 100) {
  
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi_mat) == c(n_points, n_points))) {
    
    stop("phi_mat must be an n_points x n_points matrix")
    
  }
  
  Phi_w <- phi_mat * dt
  
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rnorm(n_points, 0, 1)  # Initial condition
  
  for (t in 2:(series_length + burnin)) {
    
    # BROWNIAN MOTION NOISE (cumsum of IID increments)
    bm_increments <- rnorm(n_points, 0, sqrt(dt))
    bm_noise <- cumsum(bm_increments)
    
    X[t, ] <- as.vector(Phi_w %*% X[t - 1, ]) + bm_noise
    
  }
  
  return(X[(burnin + 1):(series_length + burnin), ])
  
}


simulate_linear_far1_bm_contaminated <- function(series_length, n_points, phi_mat,
                                                 burnin   = 100,
                                                 p_contam = 0.10,
                                                 c_shift  = 10,
                                                 h_vec    = NULL,
                                                 seed     = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Grid and operator
  
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi_mat) == c(n_points, n_points))) {
    
    stop("phi_mat must be an n_points x n_points matrix")
    
  }
  
  Phi_w <- phi_mat * dt
  
  # Simulate clean FAR(1) with TRUE BROWNIAN MOTION noise
  
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rnorm(n_points, mean = 0, sd = 1)
  
  for (t in 2:(series_length + burnin)) {
    
    # TRUE BROWNIAN MOTION: cumsum of Wiener increments
    bm_increments <- rnorm(n_points, 0, sqrt(dt))
    bm_noise <- cumsum(bm_increments)
    X[t, ]  <- as.vector(Phi_w %*% X[t - 1, ]) + bm_noise
    
  }
  
  X_clean <- X[(burnin + 1):(series_length + burnin), , drop = FALSE]
  
  # Set up contamination shape 
  
  if (is.null(h_vec)) {
    h_vec <- rep(1, n_points)  # global level shift
  } else if (length(h_vec) != n_points) {
    stop("h_vec must have length equal to n_points")
  }
  
  # Choose contaminated time points (curves)
  
  n_contam <- floor(p_contam * series_length)
  if (n_contam > 0) {
    contam_idx <- sort(sample(series_length, size = n_contam, replace = FALSE))
  } else {
    contam_idx <- integer(0)
  }
  
  # Apply additive curve-level contamination - IDENTICAL
  
  X_out <- X_clean
  if (n_contam > 0) {
    shift_vec <- c_shift * h_vec
    for (t in contam_idx) {
      X_out[t, ] <- X_out[t, ] + shift_vec
    }
  }
  
  return(X_out)
}

generate_c1_c2 <- function(value) {
  
  # c1 <- runif(1, -value, value)
  
  # constants <- sample(c(0, 1), 2, replace = F)
  # c2 <- constants[1] * (abs(c1) - value) + constants[2] * (value - abs(c1))
  
  c1 <- runif(1, 0, value)  
  c2 <- c1-value
  
  return(c(c1, c2))
  
  
}


simulate_threshold_far1 <- function(series_length, n_points,
                                    phi1_mat, phi2_mat,
                                    r_fun, s_thresh,
                                    burnin = 100) {
  
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi1_mat) == c(n_points, n_points)) ||
      !all(dim(phi2_mat) == c(n_points, n_points))) {
    
    stop("phi1_mat and phi2_mat must be n_points x n_points matrices")
    
  }
  
  Phi1_w <- phi1_mat * dt
  Phi2_w <- phi2_mat * dt
  
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rnorm(n_points, 0, 1)
  
  for (t in 2:(series_length + burnin)) {
    
    r_val <- r_fun(X[t - 1, ])
    
    if (r_val <= s_thresh) {
      
      lin_part <- as.vector(Phi1_w %*% X[t - 1, ])
      
    } else {
      
      lin_part <- as.vector(Phi2_w %*% X[t - 1, ])
      
    }
    
    X[t, ] <- lin_part + rnorm(n_points, 0, 1)
    
  }
  
  return(X[(burnin + 1):(series_length + burnin),])
  
}


simulate_threshold_far1_heavy <- function(series_length, n_points,
                                          phi1_mat, phi2_mat,
                                          r_fun, s_thresh,
                                          burnin = 100) {
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi1_mat) == c(n_points, n_points)) ||
      !all(dim(phi2_mat) == c(n_points, n_points))) {
    stop("phi1_mat and phi2_mat must be n_points x n_points matrices")
  }
  
  Phi1_w <- phi1_mat * dt
  Phi2_w <- phi2_mat * dt
  
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rt(n_points, df = 3)
  
  for (t in 2:(series_length + burnin)) {
    r_val <- r_fun(X[t - 1, ])
    
    if (r_val <= s_thresh) {
      lin_part <- as.vector(Phi1_w %*% X[t - 1, ])
    } else {
      lin_part <- as.vector(Phi2_w %*% X[t - 1, ])
    }
    
    innov_t <- rt(n_points, df = 3)
    X[t, ]  <- lin_part + innov_t
  }
  
  X[(burnin + 1):(series_length + burnin), ]
}


simulate_threshold_far1_bm <- function(series_length, n_points,
                                       phi1_mat, phi2_mat,
                                       r_fun, s_thresh,
                                       burnin = 100) {
  
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi1_mat) == c(n_points, n_points)) ||
      !all(dim(phi2_mat) == c(n_points, n_points))) {
    stop("phi1_mat and phi2_mat must be n_points x n_points matrices")
  }
  
  Phi1_w <- phi1_mat * dt
  Phi2_w <- phi2_mat * dt
  
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rt(n_points, df = 3)
  
  for (t in 2:(series_length + burnin)) {
    r_val <- r_fun(X[t - 1, ])
    
    bm_increments <- rnorm(n_points, 0, sqrt(dt))
    bm_noise <- cumsum(bm_increments)
    
    if (r_val <= s_thresh) {
      lin_part <- as.vector(Phi1_w %*% X[t - 1, ])
    } else {
      lin_part <- as.vector(Phi2_w %*% X[t - 1, ])
    }
    
    X[t, ]  <- lin_part + bm_noise
  }
  
  X[(burnin + 1):(series_length + burnin), ]
}


simulate_threshold_far1_bm_contaminated <- function(series_length, n_points, 
                                                    phi1_mat, phi2_mat,
                                                    r_fun, s_thresh,
                                                    burnin   = 100,
                                                    p_contam = 0.10,
                                                    c_shift  = 10,
                                                    h_vec    = NULL,
                                                    seed     = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Grid and operators
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  if (!all(dim(phi1_mat) == c(n_points, n_points)) ||
      !all(dim(phi2_mat) == c(n_points, n_points))) {
    stop("phi1_mat and phi2_mat must be n_points x n_points matrices")
  }
  
  Phi1_w <- phi1_mat * dt
  Phi2_w <- phi2_mat * dt
  
  # Simulate clean threshold FAR(1) with TRUE BM noise
  X <- matrix(0, nrow = series_length + burnin, ncol = n_points)
  X[1, ] <- rnorm(n_points, mean = 0, sd = 1)
  
  for (t in 2:(series_length + burnin)) {
    r_val <- r_fun(X[t - 1, ])
    
    # TRUE BM: cumsum of Wiener increments (independent per t)
    bm_increments <- rnorm(n_points, 0, sqrt(dt))
    bm_noise <- cumsum(bm_increments)
    
    if (r_val <= s_thresh) {
      lin_part <- as.vector(Phi1_w %*% X[t - 1, ])
    } else {
      lin_part <- as.vector(Phi2_w %*% X[t - 1, ])
    }
    
    X[t, ] <- lin_part + bm_noise
  }
  
  X_clean <- X[(burnin + 1):(series_length + burnin), , drop = FALSE]
  
  # Contamination setup
  if (is.null(h_vec)) {
    h_vec <- rep(1, n_points)  # global level shift
  } else if (length(h_vec) != n_points) {
    stop("h_vec must have length equal to n_points")
  }
  
  # Choose contaminated curves
  n_contam <- floor(p_contam * series_length)
  if (n_contam > 0) {
    contam_idx <- sort(sample(series_length, size = n_contam, replace = FALSE))
  } else {
    contam_idx <- integer(0)
  }
  
  # Apply additive curve-level contamination
  X_out <- X_clean
  if (n_contam > 0) {
    shift_vec <- c_shift * h_vec
    for (t in contam_idx) {
      X_out[t, ] <- X_out[t, ] + shift_vec
    }
  }
  
  return(X_out)
}


simulate_p_value_linear_comb <- function(stat_obs, lambda, n_sim = 10000) {
  # Step 1: Simulate 10,000 realizations of the null distribution
  sim_stats <- replicate(n_sim, {
    sum(lambda * rchisq(length(lambda), df = 1))  # Each λ_k * χ²_1
  })
  
  # Step 2: p-value = proportion of simulated stats ≥ observed stat
  p_value <- mean(sim_stats >= stat_obs)
  return(p_value)
}



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
  # fts_idcr[fts_idcr < - 10] <- 0
  
  return(fts_idcr)
  
}


                    
simulate_iid_fts_bm <- function(n_points, series_length) {
  
  u_grid <- seq(0, 1, length.out = n_points)
  dt     <- u_grid[2] - u_grid[1]
  
  X <- matrix(0, nrow = series_length, ncol = n_points)
  
  for (i in 1 : series_length) {
    
    bm_increments <- rnorm(n_points, 0, sqrt(dt))
    bm_noise <- cumsum(bm_increments)
    
    X[i, ] <- bm_noise
    
  }
  
  return(X)
  
}


                    
