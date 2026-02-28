# Load necessary libraries
rm(list = ls())
gc()

library(quantmod)
library(sgt)
library(ADGofTest)
library(future.apply)

# Start timer
strt <- Sys.time()

# Define tickers and fetch data
tickers <- c("NFLX", "COST")
getSymbols(tickers, from = "2018-04-01", to = "2025-03-31")

# Log return calculation
calculate_returns <- function(ticker) {
  return(na.omit(diff(log(Ad(get(ticker))))))
}

# Rolling window and frequency
window_size <- 250
f <- 5

# Function to calculate and plot rolling SGT density
calculate_and_plot_density <- function(returns, window_size, f, stock_name) {
  n <- length(returns)
  num_windows <- floor((n - window_size + f) / f)
  roll_window <- num_windows - 1
  
  plan(multisession)
  
  fitted_densities <- future_lapply(seq_len(roll_window), function(i) {
    start_idx <- 1 + (i - 1) * f
    end_idx <- start_idx + window_size - 1
    if (end_idx > n) return(NULL)
    
    window_data <- returns[start_idx:end_idx]
    start <- list(mu = mean(window_data), sigma = sd(window_data),
                  lambda = 0, p = 2, q = 2)
    
    sgt_params <- sgt.mle(X.f = ~(window_data),
                          start = start, mean.cent = TRUE, var.adj = TRUE,
                          itnmax = 500, method = "BFGS")
    
    x_vals <- seq(min(window_data), max(window_data), length.out = 500)
    fitted_density <- dsgt(x_vals,
                           mu = sgt_params$estimate["mu"],
                           sigma = sgt_params$estimate["sigma"],
                           lambda = sgt_params$estimate["lambda"],
                           p = sgt_params$estimate["p"],
                           q = sgt_params$estimate["q"])
    
    list(x = x_vals, density = fitted_density)
  })
  
  fitted_densities <- Filter(Negate(is.null), fitted_densities)
  
  all_x <- unlist(lapply(fitted_densities, `[[`, "x"))
  all_y <- unlist(lapply(fitted_densities, `[[`, "density"))
  x_range <- range(all_x, na.rm = TRUE)
  y_range <- range(all_y, na.rm = TRUE)
  
  plot(NULL, xlim = c(-.2,.2), ylim = c(0,60),
       xlab = "Log Returns", ylab = "SGT Density",
       main = paste("Rolling SGT Densities:", stock_name),
       cex.lab = 1.1, cex.axis = 1, cex.main = 1.2,
       col.lab = "black", col.axis = "black", col.main = "black",
       font.lab = 1, font.main = 1)
  
  abline(v = 0, col = "gray50", lty = 2, lwd = 0.8)
  
  for (density in fitted_densities) {
    lines(density$x, density$density, col = rgb(0.1, 0.1, 0.8, alpha = 0.15), lwd = 1.2)
  }
  
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
}

# Prepare returns
returns_list <- lapply(tickers, calculate_returns)
names(returns_list) <- tickers

# Export directly to PDF
pdf("cluster_code/R codes/Rolling_SGT_Densities.pdf", width = 10, height = 5.5)
par(mfrow = c(1, 2), mar = c(5.3, 3, 3, 2), oma = c(0, 0, 0, 0), mgp = c(2, 0.7, 0))

for (ticker in tickers) {
  returns <- returns_list[[ticker]]
  calculate_and_plot_density(returns, window_size, f, ticker)
}
dev.off()

# End timer
end <- Sys.time()
print(end - strt)
