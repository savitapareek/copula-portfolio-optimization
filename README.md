# Semiparametric Dynamic Copula Models using Rolling-window Portfolio Optimization
This project includes analysis of 20 selected stocks using daily adjusted log-differenced returns, covering the period from April 2018 to March 2025. The analysis is performed using both weekly and daily rolling windows. It implements a framework for **portfolio optimization** using **copula-based joint modeling**, fitted marginals from the **Skewed Generalized t-distribution (SGT)**, and comparison across multiple optimization strategies. The method allows evaluation of portfolio performance over time using a **rolling window approach**, incorporating **next day mean returns** and **risk decomposition**.

## Repository Overview

- **copula.R**: Runs the proposed copula-based analysis for a single rolling window.
- **copula_with_t_gaussian.R**: Runs the proposed analysis and compares Gaussian and Student-t copulas (single rolling window).

- **Parallel execution**: All rolling windows are run in parallel using HPC array jobs.  
  SLURM launch and recombination scripts are in `cluster_code/`.

- **Visualization (Section 2.4)**: R scripts are in `cluster_code/R_codes/`.
- **Data**: Stored in `cluster_code/data/`. Historical returns are fetched using `quantmod::getSymbols()` from Yahoo Finance. Analysed markets include 20 stocks from United States, India and Hong Kong.
                 


## Rolling Window Steps
For each rolling window:
- Fit **SGT marginals** to each asset  
- Fit **empirical beta copula or gaussian or t-copula** on transformed data  
- Simulate returns from copula and compute covariance  
- Optimize portfolio using:
  - beta_copula_cov_3constraint
  - data_cov_3constraint
  - copula_cov_2constraint
  - eq_weight
  - gaussian_copula_cov_3constraint
  - t_copula_cov_3constraint
- Evaluate next-day performance:
  - Return  
  - Standard deviation (risk)  
  - Sharpe Ratio   

---

## Optimization Methods

-  via `quadprog`   

## Output Contents
Each row in the final result corresponds to a rolling window and includes:
- Portfolio **weights** from all strategies  
- Portfolio **return**, **risk**, **Sharpe ratio** 
- **SGT skewness parameters** and **AD test p-values**  
- Diagnostics: **max eigenvalue**, **convergence flags**  
- **Runtime info** and **warning messages**  


## Citation
If you find this repository useful or it contributes to your research, please cite the following paper:

Pareek, S. and Ghosh, S. K. (2025). Semiparametric Dynamic Copula Models using Rolling-window Portfolio Optimization. Preprint available at: https://arxiv.org/abs/2504.12266
