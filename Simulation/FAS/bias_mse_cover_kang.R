process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
source("/n/home01/xwu1993/ts_ips2/ts_ips_fun_v4.R")
library("parallel")

parameters <- expand.grid(c(5000), c(1,4,9), c("glm", "SL"), 1:500)[(process),]
n = 1
T = as.numeric(parameters[1])
t_lag = as.numeric(parameters[2])
model = as.character(parameters[3][[1]])
replicate = as.numeric(parameters[4])

  set.seed(1)
  time <- rep(1:T, n)
  id <- rep(1:n, rep(T, n))
  x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

  x.kang <- data.frame(matrix(rep(0, n * T * 5), nrow = n * T))
  x.kang[, 1] <- exp(x.trt[, 1]/2)
  x.kang[, 2] <- x.trt[, 2]/(1+exp(x.trt[, 1])) + 10
  x.kang[, 3] <- (x.trt[, 1] * x.trt[, 3]/25 + 0.6)^3
  x.kang[, 4] <- (x.trt[, 2] + x.trt[, 4] + 20)^2
  x.kang[, 5] <- x.trt[, 5]
  
  
  a_lag1 = rep(0, n * T)
  a = NULL
  for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i]  + 0.5)))
  a_lag1[i+1] <- a[i]
  }
  a_lag1 = a_lag1[1:(n * T)]
  a_lag1[seq(1,n*T,T)] <- 0


  set.seed(replicate)
  y <- rnorm(mean= (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
  dat <- data.frame(time = time ,id = id, y = y, a = a)
  delta.seq <- seq(0.2, 5, length.out = 50)
    
  test <- ts_ipsi.dr(dat = dat, x.kang, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1, model = model)
  true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1)
  diff <- (colMeans(test$est.eff) - colMeans(true$est.eff))
 coverage <- (colMeans(test$est.eff) - 1.96*sqrt(apply(test$est.eff, 2, var))/sqrt(T-t_lag) <= colMeans(true$est.eff)) + 
      (colMeans(test$est.eff) + 1.96*sqrt(apply(test$est.eff, 2, var))/sqrt(T-t_lag) >= colMeans(true$est.eff)) == 2
   
subDir <- paste0("/n/home01/xwu1993/ts_ips2/dr/n", n, "_T", T, "_lag", t_lag, "_", model, "_kang")
if (file.exists(subDir)){
  dir.create(file.path(paste0(subDir, "/mse")))
  dir.create(file.path(paste0(subDir, "/coverage")))
  saveRDS(diff, file = paste0(subDir, "/mse/", process, ".rds"))
  saveRDS(coverage, file = paste0(subDir, "/coverage/coverage", process, ".rds"))
} else {
  dir.create(file.path(subDir))
  dir.create(file.path(paste0(subDir, "/mse")))
  dir.create(file.path(paste0(subDir, "/coverage")))
  saveRDS(diff, file = paste0(subDir, "/mse/", process, ".rds"))
  saveRDS(coverage, file = paste0(subDir, "/coverage/coverage", process, ".rds"))
}



