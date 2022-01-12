# Code to generate Table 2:  The Integrated Bias and RMSE (multiplied by 10 for easier interpretation) and Average coverage.
library("parallel")
library("data.table")
library("xtable")
source("ts_ips_fun_v3.R")

# function to calculate the bias and mean squared errors
bias_mse = function(n, T, t_lag, model = "glm", sd = 1){
  set.seed(1)
  time <- rep(1:T, n)
  id <- rep(1:n, rep(T, n))
  x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))
  
  a_lag1 = rep(0, n * T)
  a = NULL
  for (i in 1:(n * T)) {
    a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
    a_lag1[i+1] <- a[i]
  }
  a_lag1 = a_lag1[1:(n * T)]
  a_lag1[seq(1,n*T,T)] <- 0
  
  diff_table = mcmapply(function(i){
    set.seed(i)
    y <- rnorm(mean= (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), sd = sd, n * T)
    dat = data.frame(time = time ,id = id, y = y, a = a)
    delta.seq <- seq(0.2, 5, length.out = 50)
    
    test = ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1, model = model)
    true = ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1)
    coverage = (colMeans(test$est.eff) - 1.96*1/(T-t_lag) * sqrt(colSums(test$est.var1.ipw)) <= colMeans(true$est.eff)) + 
      (colMeans(test$est.eff) + 1.96*1/(T-t_lag) * sqrt(colSums(test$est.var1.ipw)) >= colMeans(true$est.eff)) == 2
    
    return(colMeans(test$est.eff) - colMeans(true$est.eff))
  }, 1:50, mc.cores = 10)
  
  abs_bias = mean(abs(rowMeans(diff_table)))
  rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
  return(c(abs_bias, rmse))
}

n = 1
# calculate the bias and mean squared errors for each of the simulation senarios
# model = glm
# lag 1
diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag1_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse200_2 = (c(abs_bias, rmse))
bias_mse200_2
# lag 4
diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag4_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse200_5 = (c(abs_bias, rmse))
bias_mse200_5
# lag 9
diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag9_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse200_10 = (c(abs_bias, rmse))
bias_mse200_10

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag1_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse1000_2 = (c(abs_bias, rmse))
bias_mse1000_2

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag4_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse1000_5 = (c(abs_bias, rmse))
bias_mse1000_5

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag9_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse1000_10 = (c(abs_bias, rmse))
bias_mse1000_10

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag1_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse5000_2 = (c(abs_bias, rmse))
bias_mse5000_2

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag4_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse5000_5 = (c(abs_bias, rmse))
bias_mse5000_5

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag9_glm/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse5000_10 = (c(abs_bias, rmse))
bias_mse5000_10

# model = SL
diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag1_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse200_2_sl = (c(abs_bias, rmse))
bias_mse200_2_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag4_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse200_5_sl = (c(abs_bias, rmse))
bias_mse200_5_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag9_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse200_10_sl = (c(abs_bias, rmse))
bias_mse200_10_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag1_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse1000_2_sl = (c(abs_bias, rmse))
bias_mse1000_2_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag4_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse1000_5_sl = (c(abs_bias, rmse))
bias_mse1000_5_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag9_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse1000_10_sl = (c(abs_bias, rmse))
bias_mse1000_10_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag1_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse5000_2_sl = (c(abs_bias, rmse))
bias_mse5000_2_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag4_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse5000_5_sl = (c(abs_bias, rmse))
bias_mse5000_5_sl

diff_table = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag9_sl/mse",
                               pattern = "\\.rds",
                               full.names = TRUE), readRDS)
abs_bias = mean(abs(rowMeans(diff_table)))
rmse = mean(sqrt(rowMeans(diff_table^2)))*sqrt(n)
bias_mse5000_10_sl = (c(abs_bias, rmse))
bias_mse5000_10_sl


# calculate the coverage rate for each of the simulation senarios
coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag1_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate200_2 <- rowMeans(coverage)
ac200_2 <- mean(coverage_rate200_2)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag1_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate200_2_sl <- rowMeans(coverage)
ac200_2_sl <- mean(coverage_rate200_2_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag4_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate200_5 <- rowMeans(coverage)
ac200_5 <- mean(coverage_rate200_5)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag4_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate200_5_sl <- rowMeans(coverage)
ac200_5_sl <- mean(coverage_rate200_5_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag9_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate200_10 <- rowMeans(coverage)
ac200_10 <- mean(coverage_rate200_10)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag9_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate200_10_sl <- rowMeans(coverage)
ac200_10_sl <- mean(coverage_rate200_10_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag1_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate1000_2 <- rowMeans(coverage)
ac1000_2 <- mean(coverage_rate1000_2)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag1_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate1000_2_sl <- rowMeans(coverage)
ac1000_2_sl <- mean(coverage_rate1000_2_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag4_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate1000_5 <- rowMeans(coverage)
ac1000_5 <- mean(coverage_rate1000_5)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag4_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate1000_5_sl <- rowMeans(coverage)
ac1000_5_sl <- mean(coverage_rate1000_5_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag9_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate1000_10 <- rowMeans(coverage)
ac1000_10 <- mean(coverage_rate1000_10)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag9_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate1000_10_sl <- rowMeans(coverage)
ac1000_10_sl <- mean(coverage_rate1000_10_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag1_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate5000_2 <- rowMeans(coverage)
ac5000_2 <- mean(coverage_rate5000_2)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag1_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate5000_2_sl <- rowMeans(coverage)
ac5000_2_sl <- mean(coverage_rate5000_2_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag4_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate5000_5 <- rowMeans(coverage)
ac5000_5 <- mean(coverage_rate5000_5)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag4_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate5000_5_sl <- rowMeans(coverage)
ac5000_5_sl <- mean(coverage_rate5000_5_sl)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag9_glm/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate5000_10 <- rowMeans(coverage)
ac5000_10 <- mean(coverage_rate5000_10)

coverage = sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag9_sl/coverage",
                             pattern = "\\.rds",
                             full.names = TRUE), readRDS)
coverage_rate5000_10_sl <- rowMeans(coverage)
ac5000_10_sl <- mean(coverage_rate5000_10_sl)

# Table 2:  The Integrated Bias and RMSE (multiplied by 10 for easier interpretation) and average coverage
table2 = data.frame("Propensity Score Model" = c(rep("",4),"Logistic Regression",rep("",4),
                                                   rep("",4),"Super Learner",rep("",4)),
                      T = round(rep(c(200, 1000, 5000, 200, 1000, 5000), rep(3, 6)), 0),
                      t_0 = round(rep(c(1, 4, 9), 6), 0),
                      "Integrated Bias" = c(round(10*bias_mse200_2[1],2),
                                            round(10*bias_mse200_5[1],2),
                                            round(10*bias_mse200_10[1],2),
                                            round(10*bias_mse1000_2[1],2),
                                            round(10*bias_mse1000_5[1],2),
                                            round(10*bias_mse1000_10[1],2),
                                            round(10*bias_mse5000_2[1],2),
                                            round(10*bias_mse5000_5[1],2),
                                            round(10*bias_mse5000_10[1],2),
                                            round(10*bias_mse200_2_sl[1],2),
                                            round(10*bias_mse200_5_sl[1],2),
                                            round(10*bias_mse200_10_sl[1],2),
                                            round(10*bias_mse1000_2_sl[1],2),
                                            round(10*bias_mse1000_5_sl[1],2),
                                            round(10*bias_mse1000_10_sl[1],2),
                                            round(10*bias_mse5000_2_sl[1],2),
                                            round(10*bias_mse5000_5_sl[1],2),
                                            round(10*bias_mse5000_10_sl[1],2)),
                      "RMSE" = c(round(10*bias_mse200_2[2],2),
                                 round(10*bias_mse200_5[2],2),
                                 round(10*bias_mse200_10[2],2),
                                 round(10*bias_mse1000_2[2],2),
                                 round(10*bias_mse1000_5[2],2),
                                 round(10*bias_mse1000_10[2],2),
                                 round(10*bias_mse5000_2[2],2),
                                 round(10*bias_mse5000_5[2],2),
                                 round(10*bias_mse5000_10[2],2),
                                 round(10*bias_mse200_2_sl[2],2),
                                 round(10*bias_mse200_5_sl[2],2),
                                 round(10*bias_mse200_10_sl[2],2),
                                 round(10*bias_mse1000_2_sl[2],2),
                                 round(10*bias_mse1000_5_sl[2],2),
                                 round(10*bias_mse1000_10_sl[2],2),
                                 round(10*bias_mse5000_2_sl[2],2),
                                 round(10*bias_mse5000_5_sl[2],2),
                                 round(10*bias_mse5000_10_sl[2],2)),
                      "Average Coverage" = c(round(100*ac200_2,2),
                                             round(100*ac200_5,2),
                                             round(100*ac200_10,2),
                                             round(100*ac1000_2,2),
                                             round(100*ac1000_5,2),
                                             round(100*ac1000_10,2),
                                             round(100*ac5000_2,2),
                                             round(100*ac5000_5,2),
                                             round(100*ac5000_10,2),
                                             round(100*ac200_2_sl,2),
                                             round(100*ac200_5_sl,2),
                                             round(100*ac200_10_sl,2),
                                             round(100*ac1000_2_sl,2),
                                             round(100*ac1000_5_sl,2),
                                             round(100*ac1000_10_sl,2),
                                             round(100*ac5000_2_sl,2),
                                             round(100*ac5000_5_sl,2),
                                             round(100*ac5000_10_sl,2)))
                      
                      
print(xtable(table2, include.rownames = FALSE))
