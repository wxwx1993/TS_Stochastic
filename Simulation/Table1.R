# Code to generate Table 1:  The Integrated Bias and RMSE (multiplied by 10 for easier interpretation).
library("parallel")
library("data.table")
library("xtable")
source("ts_ips_fun2.R")

bias_mse <- function(n,
                     T,
                     t_lag,
                     threshold = 0,
                     model = "glm") {
  set.seed(1)
  time <- rep(1:T, n)
  id <- rep(1:n, rep(T, n))
  x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

  a_lag1 <- rep(0, n * T)
  a <- NULL
  for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10 * (rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  a_lag1[i + 1] <- a[i]
  }
  a_lag1 <- a_lag1[1:(n * T)]
  a_lag1[seq(1, n * T, T)] <- 0
  
  diff_table <- mcmapply(function(i) {
    set.seed(i)
    y <- rnorm(mean= (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
    dat <- data.frame(time = time , id = id, y = y, a = a)
    delta.seq <- seq(0.2, 5, length.out = 50)
    
    test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq, model = model)
    true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq)
    return(colMeans(test$est.eff) - colMeans(true$est.eff))
  }, 1:500, mc.cores = 10)
  abs_bias <- mean(abs(rowMeans(diff_table)))
  rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
  return(c(abs_bias, rmse))
}

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag1_glm",
                                pattern = "\\.rds",
                                full.names = TRUE), 
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse200_2 <- (c(abs_bias, rmse))
bias_mse200_2
# [1] 0.08460335 0.11690298

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag4_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse200_5 <- (c(abs_bias, rmse))
bias_mse200_5
# [1] 0.1775564 0.2116833

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag9_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse200_10 <- (c(abs_bias, rmse))
bias_mse200_10
# [1] 0.311797 0.345412

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag1_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse1000_2 <- (c(abs_bias, rmse))
# [1] 0.03568471 0.05139791

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag4_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse1000_5 <- (c(abs_bias, rmse))
# [1] 0.06708455 0.07898767

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag9_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse1000_10 <- (c(abs_bias, rmse))
# [1] 0.1352112 0.1434265

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag1_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse5000_2 <- (c(abs_bias, rmse))
bias_mse5000_2
# [1] 0.01108679 0.01910042

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag4_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse5000_5 <- (c(abs_bias, rmse))
bias_mse5000_5
# [1] 0.02130971 0.02766761

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag9_glm",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse5000_10 <- (c(abs_bias, rmse))
# [1] 0.04156531 0.04676075

# model = SL
diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag1_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse200_2_sl <- (c(abs_bias, rmse))
bias_mse200_2_sl
# [1] 0.06640545 0.10163787

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag4_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse200_5_sl <- (c(abs_bias, rmse))
bias_mse200_5_sl
# [1] 0.1265382 0.1667044

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T200_lag9_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse200_10_sl <- (c(abs_bias, rmse))
bias_mse200_10_sl
# [1] 0.2298254 0.2712149

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag1_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse1000_2_sl <- (c(abs_bias, rmse))
# [1] 0.01788181 0.03834685

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag4_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse1000_5_sl <- (c(abs_bias, rmse))
# [1] 0.03560165 0.05106423

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T1000_lag9_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse1000_10_sl <- (c(abs_bias, rmse))
# [1] 0.07891206 0.08918792

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag1_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse5000_2_sl <- (c(abs_bias, rmse))
# [1] 0.003260181 0.015506502

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag4_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse5000_5_sl <- (c(abs_bias, rmse))
bias_mse5000_5_sl
# [1] 0.008553281 0.019438153

diff_table <- sapply(list.files("/Users/apple/Dropbox/Incremental_TS/Simulation/n1_T5000_lag9_sl",
                                pattern = "\\.rds",
                                full.names = TRUE),
                     readRDS)
abs_bias <- mean(abs(rowMeans(diff_table)))
rmse <- mean(sqrt(rowMeans(diff_table^2))) * sqrt(n)
bias_mse5000_10_sl <- (c(abs_bias, rmse))
bias_mse5000_10_sl
# [1] 0.01580215 0.02919136

# Table 1:  The Integrated Bias and RMSE (multiplied by 10 for easier interpretation).
bias_mse = data.frame(T = round(rep(c(200, 1000, 5000), rep(3, 3)), 0),
                      t_0 = round(rep(c(2, 5, 10), 3), 0),
                      Logistic = c(paste0(round(10*bias_mse200_2[1],2),"(", round(10*bias_mse200_2[2],2),")"),
                                   paste0(round(10*bias_mse200_5[1],2),"(", round(10*bias_mse200_5[2],2),")"),
                                   paste0(round(10*bias_mse200_10[1],2),"(", round(10*bias_mse200_10[2],2),")"),
                                   paste0(round(10*bias_mse1000_2[1],2),"(", round(10*bias_mse1000_2[2],2),")"),
                                   paste0(round(10*bias_mse1000_5[1],2),"(", round(10*bias_mse1000_5[2],2),")"),
                                   paste0(round(10*bias_mse1000_10[1],2),"(", round(10*bias_mse1000_10[2],2),")"),
                                   paste0(round(10*bias_mse5000_2[1],2),"(", round(10*bias_mse5000_2[2],2),")"),
                                   paste0(round(10*bias_mse5000_5[1],2),"(", round(10*bias_mse5000_5[2],2),")"),
                                   paste0(round(10*bias_mse5000_10[1],2),"(", round(10*bias_mse5000_10[2],2),")")),
                      "Super Learner" = c(paste0(round(10*bias_mse200_2_sl[1],2),"(", round(10*bias_mse200_2_sl[2],2),")"),
                                          paste0(round(10*bias_mse200_5_sl[1],2),"(", round(10*bias_mse200_5_sl[2],2),")"),
                                          paste0(round(10*bias_mse200_10_sl[1],2),"(", round(10*bias_mse200_10_sl[2],2),")"),
                                          paste0(round(10*bias_mse1000_2_sl[1],2),"(", round(10*bias_mse1000_2_sl[2],2),")"),
                                          paste0(round(10*bias_mse1000_5_sl[1],2),"(", round(10*bias_mse1000_5_sl[2],2),")"),
                                          paste0(round(10*bias_mse1000_10_sl[1],2),"(", round(10*bias_mse1000_10_sl[2],2),")"),
                                          paste0(round(10*bias_mse5000_2_sl[1],2),"(", round(10*bias_mse5000_2_sl[2],2),")"),
                                          paste0(round(10*bias_mse5000_5_sl[1],2),"(", round(10*bias_mse5000_5_sl[2],2),")"),
                                          paste0(round(10*bias_mse5000_10_sl[1],2),"(", round(10*bias_mse5000_10_sl[2],2),")")))
print(xtable(bias_mse, include.rownames=FALSE))
