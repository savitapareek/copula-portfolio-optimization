# --- Setup ---
rm(list = ls()); gc()
library(quantmod)
library(zoo)

strt=Sys.time()
# Get PG and META data
asset_names=c("NFLX", "COST")
getSymbols(asset_names, from = "2018-04-01", to = "2025-03-31")

# Calculate log returns
returns_list <- list()
for (ticker in asset_names) {
  returns_list[[ticker]] <- na.omit(diff(log(Ad(get(ticker)))))
}
names(returns_list) <- asset_names  # If asset_names matches your list
sapply(returns_list, function(x) length(rownames(data.frame(x))))

# Combine all returns into a matrix
returns_matrix <- coredata(do.call(merge, c(returns_list, all = FALSE)))
colnames(returns_matrix) <- asset_names
rownames(returns_matrix)=rownames(data.frame(returns_list[[1]]))
dim(returns_matrix)
summary(returns_matrix)

# Merge returns (align dates)
returns <- zoo(returns_matrix, order.by = as.Date(rownames(returns_matrix)))

# Rolling parameters
window_size <- 250
step_size <- 5

# Compute rolling stats (mean, SD, Sharpe)
roll_stats <- rollapply(
  data = returns,
  width = window_size,
  by = step_size,
  align = "right",
  FUN = function(x) {
    m <- colMeans(x)
    s <- apply(x, 2, sd)
    sr <- ifelse(s == 0, NA, m / s)
    c(m, s, sr)
  },
  by.column = FALSE
)

# Assign column names and extract time index
colnames(roll_stats) <- c("PG_ret", "META_ret", "PG_sd", "META_sd", "PG_sr", "META_sr")
roll_stats <- na.omit(roll_stats)
roll_dates <- index(roll_stats)
dim(roll_stats)
summary(roll_stats)

# Compute rolling correlations
roll_corr_pearson <- rollapply(returns, width = window_size, by = step_size,
                               FUN = function(x) cor(as.numeric(x[,1]), as.numeric(x[,2])), 
                               align = "right",by.column = FALSE)
roll_corr_pearson=(na.omit(roll_corr_pearson))
roll_corr_kendall <- rollapply(returns, width = window_size, by = step_size,
                               FUN = function(x) cor(x[,1], x[,2], method = "kendall"), 
                               align = "right",by.column = FALSE)
roll_corr_kendall =(na.omit(roll_corr_kendall ))
roll_corr_spearman <- rollapply(returns, width = window_size, by = step_size,
                                FUN = function(x) cor(x[,1], x[,2], method = "spearman"),
                                align = "right",by.column = FALSE)
roll_corr_spearman=(na.omit(roll_corr_spearman))
corr_dates <- index(roll_corr_pearson)
summary(roll_corr_spearman)
summary(roll_corr_kendall)
summary(roll_corr_pearson)

# --- Fancy PDF Export ---
pdf("cluster_code/R codes/Rolling_mean_SD_corr.pdf", width = 9, height = 6)
par(mfrow = c(2, 2), 
    mar = c(3.8, 4.2, 3.2, 1), 
    mgp = c(2.2, 0.7, 0), 
    cex.axis = 0.85, 
    cex.lab = 1, 
    cex.main = 1.1)

# Define key events
event_dates <- as.Date(c("2020-03-01", "2022-02-24"))
#event_labels <- c("COVID-19 Market Crash", "Geopolitical Shock: Russia-Ukraine War")


# Common function for date axis
format_x_axis <- function(dates) {
  axis.Date(1, at = pretty(dates), format = "%b-%y")
}

# (a) Rolling Mean Return
yrange_ret <- range(roll_stats[, c("PG_ret", "META_ret")],
                    na.rm = TRUE)
plot(roll_dates, roll_stats[, "PG_ret"], type = "l", 
     col = "blue",
     ylab = "Mean Return", xaxt = "n",
     xlab = "", main = paste0("(a) ", asset_names[1], " vs ", asset_names[2], " Mean Return"),
     font.lab = 1, font.main = 1, ylim = yrange_ret)
lines(roll_dates, roll_stats[, "META_ret"], col = "red")
abline(v = event_dates, col = "gray50", lty = 2)
format_x_axis(roll_dates)
grid(col = "gray80", lty = "dotted")
legend("topright", legend = asset_names, col = c("blue", "red"), lty = 1, bty = "n", cex = 0.85)

# (b) Rolling Standard Deviation
yrange_sd <- range(roll_stats[, c("PG_sd", "META_sd")], na.rm = TRUE)
plot(roll_dates, roll_stats[, "PG_sd"], type = "l", col = "blue",
     ylab = "Standard Deviation", xaxt = "n",
     xlab = "", main = "(b) Volatility",
     font.lab = 1, font.main = 1, ylim = yrange_sd)
lines(roll_dates, roll_stats[, "META_sd"], col = "red")
abline(v = event_dates, col = "gray50", lty = 2)
format_x_axis(roll_dates)
grid(col = "gray80", lty = "dotted")

# (c) Rolling Sharpe Ratio
yrange_sr <- range(roll_stats[, c("PG_sr", "META_sr")], na.rm = TRUE)
plot(roll_dates, roll_stats[, "PG_sr"], type = "l", col = "blue",
     ylab = "Sharpe Ratio", xaxt = "n",
     xlab = "", main = "(c) Sharpe Ratio",
     font.lab = 1, font.main = 1, ylim = yrange_sr)
lines(roll_dates, roll_stats[, "META_sr"], col = "red")
abline(v = event_dates, col = "gray50", lty = 2)
format_x_axis(roll_dates)
grid(col = "gray80", lty = "dotted")

# (d) Rolling Correlation
yrange_corr <- range(roll_corr_pearson, roll_corr_kendall, roll_corr_spearman, na.rm = TRUE)
plot(corr_dates, roll_corr_pearson, type = "l", col = "black",
     ylab = "Correlation", xaxt = "n",
     xlab = "", main = "(d) Rolling Correlation",
     font.lab = 1, font.main = 1, ylim = yrange_corr)
lines(corr_dates, roll_corr_kendall, col = "darkred", lty = 1)
lines(corr_dates, roll_corr_spearman, col = "forestgreen", lty = 1)
abline(v = event_dates, col = "gray50", lty = 2)
format_x_axis(corr_dates)
grid(col = "gray80", lty = "dotted")
legend("topright", legend = c("Pearson", "Kendall", "Spearman"),
       col = c("black", "darkred", "forestgreen"), lty = 1, bty = "n", cex = 0.85)

dev.off()

end=Sys.time()
end-strt
