process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

source("/n/home01/xwu1993/ts_ips/ts_ips_fun2.R")

library("parallel")

n = 1
T = 200
t_lag = 4
model = "SL"

  set.seed(1)
  time <- rep(1:T, n)
  id <- rep(1:n, rep(T, n))
  x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))
  x.out <- x.trt
  
  a_lag1 = rep(0, n * T)
  a = NULL
  for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  a_lag1[i+1] <- a[i]
  }
  a_lag1 = a_lag1[1:(n * T)]
  a_lag1[seq(1,n*T,T)] <- 0
  

  set.seed(process)
  y <- rnorm(mean= (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
  dat <- data.frame(time = time ,id = id, y = y, a = a)
  delta.seq <- seq(0.2, 5, length.out = 50)
    
  test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq, model = model)
  true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq)
  diff <- (colMeans(test$est.eff) - colMeans(true$est.eff))

subDir <- paste0("/n/home01/xwu1993/ts_ips/n", n, "_T", T, "_lag", t_lag, "_", model)
if (file.exists(subDir)){
  saveRDS(diff, file = paste0(subDir, "/", process, ".rds"))
} else {
  dir.create(file.path(subDir))
  saveRDS(diff, file = paste0(subDir, "/", process, ".rds"))
}



