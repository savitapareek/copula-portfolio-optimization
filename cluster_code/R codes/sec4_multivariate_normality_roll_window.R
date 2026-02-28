rm(list=ls())

library(fPortfolio)
library(doParallel)
library(foreach)
strt=Sys.time()

#### use the GitHub folder cluster_code/data
returns_matrix <- readRDS("cluster_code/data/US_7.rds")

win_len <- 250    #250 for US,245 IN and 244 for HK # 250 trading days ~ 1 year
n <- nrow(returns_matrix)

step_size <- 1 # 5 for weekly                   # weekly rebalancing (every 5 trading days)
starts <- seq(1, n - win_len + 1, by = step_size)

n_cores <- parallel::detectCores() - 1
cl <- makeCluster(max(1, n_cores))
registerDoParallel(cl)

roll_norm_results <- foreach(s = starts, .combine = rbind,
                             .packages = "fPortfolio") %dopar% {
                               start <- s
                               end   <- s + win_len - 1
                               ret_win <- returns_matrix[start:end, , drop = FALSE]
                               
                               sh_p <- NA_real_
                               en_p <- NA_real_
                               
                               # Shapiro
                               sh_test <- try(assetsTest(ret_win, method = "shapiro"), silent = TRUE)
                               if (!inherits(sh_test, "try-error")) {
                                 sh_p <- sh_test$p.value
                               }
                               
                               # Energy
                               en_test <- try(assetsTest(ret_win, method = "energy"), silent = TRUE)
                               if (!inherits(en_test, "try-error")) {
                                 en_p <- en_test$p.value
                               }
                               
                               data.frame(
                                 window_start = start,
                                 window_end   = end,
                                 shapiro_p    = sh_p,
                                 shapiro_norm = ifelse(!is.na(sh_p) && sh_p > 0.05, "not rejected", "rejected"),
                                 energy_p     = en_p,
                                 energy_norm  = ifelse(!is.na(en_p) && en_p > 0.05, "not rejected", "rejected")
                               )
                             }

stopCluster(cl)

head(roll_norm_results)

# quantiles if you want
quantile(roll_norm_results$energy_p,
         probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
         na.rm = TRUE)
quantile(roll_norm_results$shapiro_p,
         probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
         na.rm = TRUE)

end=Sys.time()
end-strt
