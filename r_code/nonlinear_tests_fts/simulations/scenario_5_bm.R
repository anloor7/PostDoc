
# Power analysis grid: T=200 fixed, c varies, alpha=0.05 only

series_length   <- 200
trials          <- 2000
quantile_levels <- seq(0.05, 0.95, 0.05)
n_points        <- 500
alpha           <- 0.05
lag_test        <- 1

# Vary serial dependence strength

c_values        <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)

methods <- c("FQA", "FPC", "FACF", "SDO", "FSACF")

# Results: [c_values × methods]

power_results <- array(NA_real_,
                       dim = c(length(c_values), length(methods)),
                       dimnames = list(paste0("c=", c_values), methods))

make_phi_mat_gaussian <- function(n_points, c1, c2 = 1) {
  u_grid <- seq(0, 1, length.out = n_points)
  Gamma_uv <- outer(u_grid, u_grid, function(u, v) c1 * exp(-c2 * (u^2 + v^2)/2))
  Gamma_uv
}

## ---- Main power grid loop ----
# set.seed(1234)

for (k in seq_along(c_values)) {
  
  c_val <- c_values[k]
  cat("Computing power for c =", c_val, "\n")
  
  phi_mat <- make_phi_mat_gaussian(n_points, c_val)
  
  # Rejection indicators for this c value
  
  rejections <- array(0L, dim = c(trials, length(methods)),
                      dimnames = list(NULL, methods))
  
  for (i in 1:trials) {
    
    # Generate FAR(1) series with current c
    
    series <- simulate_linear_far1_bm(series_length = series_length, 
                                   n_points = n_points, 
                                   phi_mat = phi_mat)
    f_data <- t(series)
    
    # FQA test
    
    aux <- estimate_fqa_vector_barlett_reduced(series, quantile_levels, lag_test)
    whole_statistic <- series_length * sum(aux$fqa^2)
    cov_mat <- aux$covariance
    eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
    lambda <- pmax(eig, 0)[eig > 0]
    
    # Proposed FQA
    
    crit_fqa <- simulate_q_linear_comb_chisq(lambda, prob = 1 - alpha)
    rejections[i, "FQA"] <- (whole_statistic > crit_fqa)
    
    # Competitors
    
    ind_res <- independence_test(f_data = f_data, components = 3, lag = lag_test, alpha = alpha)
    rejections[i, "FPC"] <- (ind_res$p_value < alpha)
    
    facf_res <- fACF_test(f_data = f_data, H = lag_test, iid = TRUE, M = NULL, alpha = alpha)
    rejections[i, "FACF"] <- (facf_res$p_value < alpha)
    
    sdo_res <- spectral_test(f_data, kernel = 'Parzen', bandwidth = 'adaptive', alpha = alpha)
    rejections[i, "SDO"] <- (sdo_res$p_value < alpha)
    
    fsacf_res <- fSACF_test(f_data = f_data, H = lag_test, pplot = FALSE, alpha = alpha)
    rejections[i, "FSACF"] <- (fsacf_res$p_value < alpha)
    
    if (i %% 200 == 0) cat("  Trial", i, "of", trials, "done\n")
    print(i)
  }
  
  # Store empirical power for this c
  power_results[k, ] <- apply(rejections, 2, mean)
  cat("c =", c_val, "Power (α=0.05):", round(power_results[k, ], 3), "\n\n")
}

## Final results table
print("Power results (T=200, α=0.05, varying c):")
print(round(power_results, 3))

# Saving the results

setwd('/Users/lopezoa/Library/Mobile Documents/com~apple~CloudDocs/academic_life/postdoc/papers/tests_fqa/code')
# save(power_results, file = 'power_results_s5_3.RData')


# Plot 

library(ggplot2)
library(tidyr)
library(dplyr)

load('power_results_s5_3.RData')

df_power <- as.data.frame(power_results)

# Add c as numeric variable
df_power$c <- sub("c=", "", rownames(df_power))
df_power$c <- as.numeric(df_power$c)

# Long format for ggplot
df_long <- df_power %>%
  tidyr::pivot_longer(
    cols = -c,
    names_to = "Method",
    values_to = "Power"
  )

# Order of methods in legend
df_long$Method <- factor(df_long$Method,
                         levels = c("FQA", "FPC", "FACF", "SDO", "FSACF"))

# Base: default ggplot2 colors for 5 categories
default_cols <- scales::hue_pal()(5)
names(default_cols) <- levels(df_long$Method)

# Override specific methods:
default_cols["FQA"] <- "#4AA5FF"  # blue for FQA
default_cols["SDO"] <- "#F8766D"  # red for SDO
default_cols["FSACF"] <- "#B15928"  # brown to avoid violet

# Shapes
shapes <- c(
  "FQA"   = 16,
  "FPC"   = 17,
  "FACF"  = 15,
  "SDO"   = 3,
  "FSACF" = 18
)

title_size <- 12
number_size <- 12
legend_size <- 12
title_size <- 14

plot_3 <- ggplot(df_long, aes(x = c, y = Power, color = Method, shape = Method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = default_cols) +
  scale_shape_manual(values = shapes) +
  scale_x_continuous(breaks = sort(unique(df_long$c))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(title = "Scenario 5 (Brownian motion noise)",
       x = "c",
       y = "Empirical rejection rate",
       color = "Method",
       shape = "Method") +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = title_size),
    axis.title.y = element_text(size = title_size),
    axis.text.x  = element_text(size = number_size),
    axis.text.y  = element_text(size = number_size),
    legend.title = element_blank(),
    legend.text  = element_text(size = legend_size),
    plot.title   = element_text(size = title_size, hjust = 0.5)
  )





