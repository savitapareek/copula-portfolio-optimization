rm(list = ls())

# Load required libraries
library(sgt)
library(quantmod)
library(copula)

strt=Sys.time()

# Step 1: Load returns
asset_names <- c("NFLX", "COST")
getSymbols(asset_names, from = "2018-04-01", to = "2025-03-31")
returns_list <- lapply(asset_names, function(ticker) na.omit(diff(log(Ad(get(ticker))))))
returns_matrix <- coredata(do.call(merge, c(returns_list, all = FALSE)))
colnames(returns_matrix) <- asset_names
returns <- zoo(returns_matrix, order.by = index(returns_list[[1]]))
dates <- index(returns)
d <- 2

# Step 2: Fit empirical beta copula
fit.empirical_beta_copula <- function(log_returns_matrix) {
  uniform_marginals <- matrix(NA, nrow = nrow(log_returns_matrix), ncol = d)
  for (i in 1:d) {
    tmp_df <- data.frame(x = log_returns_matrix[, i])
    sgt_fit <- sgt.mle(X.f = ~x, data = tmp_df,
                       start = list(mu = mean(tmp_df$x), sigma = sd(tmp_df$x), lambda = 0, p = 2, q = 2),
                       mean.cent = TRUE, var.adj = TRUE, method = "BFGS", itnmax = 2000)
    params <- as.numeric(sgt_fit$estimate)
    uniform_marginals[, i] <- psgt(tmp_df$x, mu = params[1], sigma = params[2],
                                   lambda = params[3], p = params[4], q = params[5])
  }
  copula_fit <- empCopula(uniform_marginals, smoothing = "beta")
  return(list(beta_copula = copula_fit, pseudo_data = uniform_marginals))
}

# Step 3: Custom contour plot with overlay
plot_beta_copula_contour_keynote <- function(beta_copula_obj, pseudo_data, main_title = "", insight = "") {
  u_vals <- seq(0.01, 0.99, length.out = 60)
  z_matrix <- outer(u_vals, u_vals, Vectorize(function(u1, u2) {
    dCopula(c(u1, u2), beta_copula_obj)
  }))
  par(mgp = c(1.8, 0.6, 0))  # shift axis title (U1, U2) closer to the axis
  
  contour(u_vals, u_vals, z_matrix,
          xlab = expression(U[1]), ylab = expression(U[2]),
          main = main_title, col = "#1B9E77", lwd = 1.5,
          labcex = 1.1, cex.main = 1.4, cex.axis = 1.1, cex.lab = 1.3,
          drawlabels = T)
  box(lwd = 1.2)
  mtext(insight, side = 1, line = 4, cex = 0.9, col = "gray30")
  points(pseudo_data[,1], pseudo_data[,2], pch = 16, col = rgb(0, 0, 0, 0.25), cex = 0.6)
}


# Step 4: Rolling window + plot
par(mfrow = c(2, 3), mar = c(3, 3, 1.5, 1), oma = c(.21, .21, .21, .21))
window_size <- 250
selected_starts <- c(1, 250, 550, 850, 1150, 1450)


for (i in seq_along(selected_starts)) {
  start_idx <- selected_starts[i]
  end_idx <- start_idx + window_size - 1
  if (end_idx > nrow(returns)) next
  data_window <- returns[start_idx:end_idx, ]
  fit <- fit.empirical_beta_copula(coredata(data_window))
  
  title_text <- paste0(
                       format(dates[start_idx], "%b %Y"), " – ",
                       format(dates[end_idx], "%b %Y"))
  
  plot_beta_copula_contour_keynote(fit$beta_copula, fit$pseudo_data,
                                   main_title = title_text)
}
end=Sys.time()
end-strt
