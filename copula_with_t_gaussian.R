# Clear environment
rm(list = ls())
gc()

## load necessary libraries
library(copula)
library(sgt)
library(quantmod)
library(quadprog)
library(ADGofTest)## for ad.tes

#track running time
strt=Sys.time()

###### use the GitHub folder cluster_code/data
returns_matrix=readRDS("cluster_code/data/IN_7.rds")

###*** Fit empirical beta copula&&***
fit.empirical_beta_copula <- function(log_returns_matrix) {
  
  # Preallocate uniform marginals
  uniform_marginals <- matrix(NA, nrow = nrow(log_returns_matrix), 
                              ncol = d)
  
  # Fit marginals using sgt 
  est=matrix(NA,nrow=6,ncol=d)
  rownames(est)=c("mean","sigma","lambda","p","q","convergence_code")
  
  # sgt fit p-values using ad.test
  p_val=numeric(d)
  
  for (i in 1:d) {
    pp=log_returns_matrix[,i]
    sgt_fit <- sgt.mle(X.f = ~pp,start = list(mu = mean(pp), 
                                              sigma = sd(pp), 
                                              lambda = 0, p = 2, q = 2),
                       mean.cent = TRUE, var.adj = TRUE,itnmax=2000,method="BFGS")
    
    est[,i]=c(as.numeric(sgt_fit$estimate),sgt_fit$convcode)
    
    if(sgt_fit$convcode!=0) warning(paste("sgt non convergence for asset", i))
    
    params <- est[,i]
    
    uniform_marginals[, i] <- psgt(pp, mu = params[1], sigma = params[2], 
                                   lambda = params[3], p = params[4], 
                                   q = params[5])
    
    ad_test <- ad.test(pp, function(x) psgt(x, 
                                            mu = params[1], sigma = params[2], 
                                            lambda = params[3], p = params[4], 
                                            q = params[5]))
    
    p_val[i]=ad_test$p.value
  }
  
  # Fit the empirical beta copula
  copula_fit1 <- empCopula(uniform_marginals,smoothing="beta")

  # Fit a Gaussian (normal) copula
  copula_fit2 <- normalCopula(dim = d)           # specify dimension (e.g., 2 for bivariate)
  fit_gaussian <- fitCopula(copula_fit2, uniform_marginals, method = "ml")
  #fit_gaussian@copula
 
  # Fit a t copula
  copula_fit3 <- tCopula(dim = d,  df.fixed = T)  # df.fixed=FALSE lets df be estimated
  fit_t <- fitCopula(copula_fit3, uniform_marginals, method = "ml")
  
  return(list(beta_copula=copula_fit1,
              sgt_ad_pvalue=p_val,
              sgt_est=est,
              gaussian_copula=fit_gaussian,
              t_copula=fit_t))
}

#performance measures function for next day returns
next_day=function(weights1,roll_window, next_window){
  portfolio_return_nextday <- sum(weights1 * colMeans(next_window))
  portfolio_sd_nextday <- sqrt(t(weights1) %*% cov(next_window) %*% weights1)
  
  # Sharpe Ratio (assuming risk-free rate is 0)
  sharpe_ratio <- portfolio_return_nextday / (portfolio_sd_nextday)
  
  ## covariance risk budget
  risk_budg= weights1* (cov(next_window) %*% weights1)/as.numeric(portfolio_sd_nextday**2)
  
  list(weights=weights1,
       returns=portfolio_return_nextday,
       risk= as.numeric(portfolio_sd_nextday), 
       s_ratio=as.numeric(sharpe_ratio),
       cov_risk_budget=as.numeric(risk_budg))
}


##quadratic programming
convex_prog=function(Dmat,actual_returns){
  # Set up quadratic programming problem
  d=length(actual_returns)
  dvec <- rep(0, d)
  
  # Constraints: weights sum to 1 and expected return non-negative 
  ## optimal weight portfolio > equal weight portfolio
  Amat <- cbind(1, as.numeric(actual_returns), diag(d))
  bvec <- c(1, mean(actual_returns), rep(0, d))
  
  # Solve quadratic programming problem
  # Attempt to solve the quadratic programming problem
  opt_result <- solve.QP(2*Dmat, dvec, Amat, bvec, meq = 1)
  
}
### with  2 constraints
convex_prog1 <- function(Dmat, actual_returns) {
  d <- length(actual_returns)
  dvec <- rep(0, d)

  # Constraints: weights sum to 1 and no short selling
  Amat <- cbind(1, diag(d))  # Remove actual_returns column
  bvec <- c(1, rep(0, d))    # Remove mean(actual_returns) target

  # Solve the quadratic programming problem
  opt_result <- solve.QP(2 * Dmat, dvec, Amat, bvec, meq = 1)

}

####*** Portfolio optimization&&&****
portfolio.opt <- function(simulated_returns,roll_window, next_window) {
  cov_matrix <- cov(simulated_returns)
  actual_returns <- colMeans(roll_window)
  
  ## if one uses simply the covariance matrix
  cov_matrix_1 <- cov(roll_window)
  
  ## copula cov matrix
  opt_result=convex_prog(cov_matrix,actual_returns)
  
  ##copula cov and no additional constraint
  opt_result1=convex_prog1(cov_matrix,actual_returns)
  
  ## sample cov and all constraints
  opt_result2=convex_prog(cov_matrix_1,actual_returns)
  
  
  return(list(weights_copula_cov_3_constr = opt_result$solution, 
              weights_copula_cov_2_constr = opt_result1$solution,
              weights_data_cov_3_constr = opt_result2$solution))
}


####**** Function to process each rolling window&&***
rollapply_func <- function(roll_window, next_window,w) {
  s1=Sys.time()
  copula_fit <- fit.empirical_beta_copula(log_returns_matrix=roll_window)
  
  # Simulate returns from the Beta copula
  n_simulations <- m
  simulated_copula_beta <- rCopula(n_simulations, copula_fit$beta_copula)
  
  # Transform back to original marginals
  simulated_returns_beta <- sapply(1:d, function(i) {
    params <- copula_fit$sgt_est[,i]
    qsgt(simulated_copula_beta[, i], mu = params[1], sigma = params[2], 
         lambda = params[3], p = params[4], q = params[5])
  })
  
  # Simulate returns from the Beta copula
  simulated_copula_gaussian <- rCopula(n_simulations, copula_fit$gaussian_copula@copula)
  
  # Transform back to original marginals
  simulated_returns_gaussian <- sapply(1:d, function(i) {
    params <- copula_fit$sgt_est[,i]
    qsgt(simulated_copula_gaussian[, i], mu = params[1], sigma = params[2], 
         lambda = params[3], p = params[4], q = params[5])
  })
  
  # Simulate returns from the Beta copula
  simulated_copula_t <- rCopula(n_simulations, copula_fit$t_copula@copula)
  
  # Transform back to original marginals
  simulated_returns_t <- sapply(1:d, function(i) {
    params <- copula_fit$sgt_est[,i]
    qsgt(simulated_copula_t[, i], mu = params[1], sigma = params[2], 
         lambda = params[3], p = params[4], q = params[5])
  })
  
  
  
  max_eigen=max(eigen(cov(roll_window))$values)
  
  ## check if simulated return are infty or nan
  inf_rc_beta=which(is.infinite(simulated_returns_beta), arr.ind = TRUE)
  
  ##remove bad simulated r copula which are mostly when sgt.mle doesnot converge
  if(any(is.infinite(simulated_returns_beta))){
    simulated_returns_beta=simulated_returns_beta[-inf_rc_beta[,1],]
    warning(paste0("rolling window has some infty rcopula samples",w))
  }
  count_inf_rc_beta=length(inf_rc_beta[,1])
  
  ## check if simulated return are infty or nan
  inf_rc_t=which(is.infinite(simulated_returns_t), arr.ind = TRUE)
  
  ##remove bad simulated r copula which are mostly when sgt.mle doesnot converge
  if(any(is.infinite(simulated_returns_t))){
    simulated_returns_t=simulated_returns_t[-inf_rc_t[,1],]
    warning(paste0("rolling window has some infty rcopula samples",w))
  }
  count_inf_rc_t=length(inf_rc_t[,1])
  
  ##remove bad simulated r copula which are mostly when sgt.mle doesnot converge
  inf_rc_gaussian=which(is.infinite(simulated_returns_gaussian), arr.ind = TRUE)
  
  if(any(is.infinite(simulated_returns_gaussian))){
    simulated_returns_gaussian=simulated_returns_gaussian[-inf_rc_gaussian[,1],]
    warning(paste0("rolling window has some infty rcopula samples",w))
  }
  count_inf_rc_gaussian=length(inf_rc_gaussian[,1])
  
  # Optimize portfolio copula_cov method and data_cov
  optimized_portfolio_beta <- portfolio.opt(simulated_returns_beta,roll_window, next_window)
  
  # Optimize portfolio copula_cov method and data_cov
  optimized_portfolio_gaussian <- portfolio.opt(simulated_returns_gaussian,roll_window, next_window)
  
  # Optimize portfolio copula_cov method and data_cov
  optimized_portfolio_t <- portfolio.opt(simulated_returns_t,roll_window, next_window)
  
  Sys.time()-s1
  ## next day return performance measures
  result_copula_cov=next_day(optimized_portfolio_beta$weights_copula_cov_3_constr,
                             roll_window = roll_window ,
                             next_window =next_window )
  
  result_copula_cov1=next_day(optimized_portfolio_beta$weights_copula_cov_2_constr,
                             roll_window = roll_window ,
                             next_window =next_window )
  
  result_data_cov=next_day(optimized_portfolio_beta$weights_data_cov_3_constr,
                           roll_window = roll_window ,
                           next_window =next_window )
  result_tcopula_cov=next_day(optimized_portfolio_t$weights_copula_cov_3_constr,
                             roll_window = roll_window ,
                             next_window =next_window )
  result_gcopula_cov=next_day(optimized_portfolio_gaussian$weights_copula_cov_3_constr,
                             roll_window = roll_window ,
                             next_window =next_window )
  
  result_equal_weight=next_day(rep((1/d),d),
                               roll_window = roll_window ,
                               next_window =next_window )
  
  
  # Combine results for optimal portfolio return at t+1
  return(list(betacopula_cov_3_constr= result_copula_cov,
              betacopula_cov_2_constr= result_copula_cov1,
              data_cov_3_constr= result_data_cov,
              gcopula_cov_3_constr= result_gcopula_cov,
              tcopula_cov_3_constr= result_tcopula_cov,
              eq_weight= result_equal_weight,
              maxeigen=max_eigen,ad_pval= c(copula_fit[[2]]),
              sgt_lambda=copula_fit$sgt_est[3,],
              sgt_conv=copula_fit$sgt_est[6,],
              count_inf_rc=c(count_inf_rc_beta,count_inf_rc_t,
                             count_inf_rc_gaussian )))
}

####**** Run rolling window with array jobs&&&****

### window number and rebalancing frequency
#rebal_step=as.numeric(Sys.getenv("f"))

# Rolling window and t+1 window Array jobs
#w=as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

## for dev **uncomment above if you are running over cluster**
w=5 #

##for dev
rebal_step=f=5#1#5 #weekly/daily/fortnightly or monthly

##sample size m (for samples from copula) ## change based on economy
# based on Lu, Ghosh 2023 paper ~~ 10^5
m=10^5 ## 

## number of assets, number of years
d=ncol(returns_matrix)
years=7

## window_size and total windows
window_size=floor(nrow(returns_matrix)/years)-1
num_windows <- floor(nrow(returns_matrix) - window_size+rebal_step)/rebal_step
cat("total rolling windows=",floor(num_windows)-1,"\n")
cat("rolling windows#",w,"\n")
##total array jobs = num_windows-1

# Compute starting and ending indices
start_idx <- 1 + (w - 1) * rebal_step
end_idx <- start_idx + window_size - 1

# Extract current rolling window
roll_window <- returns_matrix[start_idx:end_idx, ]

# Extract next window (optional: next period for testing/forward evaluation)
if (rebal_step == 1) {
  next_window <- returns_matrix[(start_idx + rebal_step):(end_idx + rebal_step), ]
} else {
  next_window <- returns_matrix[(start_idx + rebal_step):(end_idx + rebal_step), ]
}

# Assume rollapply_func returns a list containing both numeric and character values
roll_result <- rollapply_func(roll_window, next_window,w)

end=Sys.time()
time=data.frame(difftime(end,strt,units="mins"),tail(rownames(next_window), 1))
colnames(time)=c("time_minutes","date")
time

# Format the warning as text (safe for no-warning case)
warn_msg <- if (length(warnings()) > 0) {
  paste(capture.output(warnings()), collapse = " | ")
} else {
  "No warning"
}


data=data.frame(c(unlist(roll_result),roll_mean=colMeans(roll_window),
                  roll_sd=apply(roll_window, 2, sd),time,
                  roll_window_no= w,rebalancing_freq=rebal_step,
                  warnings=warn_msg))


df_to_save <- as.data.frame(data)

rownames(df_to_save) <- paste0("roll_window_", w)
print(df_to_save)


## **uncomment this if you are running this over cluster for all windows**
# save file for each simu
# file_name = paste0("copula/data_temp/", "results_my_simu_",
#                    rebal_step,"_", w , ".rda")
# print(file_name)
# save(df_to_save, file = file_name)
# 
# # clean after simu
# rm(list=ls())


