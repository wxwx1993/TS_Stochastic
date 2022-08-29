process<-as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

source("/n/home01/xwu1993/ts_ips2/ts_ips_fun_v4.R")

library("parallel")

parameters <- expand.grid(c(200,1000,5000), c(1,4,9), c("glm", "SL"), 1:500)[(process),]


n = 1
T = as.numeric(parameters[1])
t_lag = as.numeric(parameters[2])
model = as.character(parameters[3][[1]])
replicate = as.numeric(parameters[4])

  set.seed(1)
  time <- rep(1:T, n)
  id <- rep(1:n, rep(T, n))
  x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))
  x.out <- x.trt
  
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
    
  test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1, model = model)
  true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1)
  diff <- (colMeans(test$est.eff) - colMeans(true$est.eff))
 coverage <- (colMeans(test$est.eff) - 1.96*sqrt(apply(test$est.eff, 2, var))/sqrt(T-t_lag) <= colMeans(true$est.eff)) + 
      (colMeans(test$est.eff) + 1.96*sqrt(apply(test$est.eff, 2, var))/sqrt(T-t_lag) >= colMeans(true$est.eff)) == 2

  alpha <- 0.05
  rho <- sqrt((-alpha^2 - 2*log(alpha)+log(-2*log(alpha)+1-alpha^2))/(T-t_lag))
  tu_cs <- sqrt((2*(T-t_lag)*colMeans(test$est.var1.ipw)*rho^2+1)/((T-t_lag)^2*rho^2)*log(sqrt((T-t_lag)*colMeans(test$est.var1.ipw)*rho^2+1)/alpha))

  tu_coverage <- (colMeans(test$est.eff) - tu_cs <= colMeans(true$est.eff)) + 
      (colMeans(test$est.eff) + tu_cs >= colMeans(true$est.eff)) == 2
   
subDir <- paste0("/n/home01/xwu1993/ts_ips2/ipw/n", n, "_T", T, "_lag", t_lag, "_", model)
if (file.exists(subDir)){
  dir.create(file.path(paste0(subDir, "/mse")))
  dir.create(file.path(paste0(subDir, "/coverage")))
  saveRDS(diff, file = paste0(subDir, "/mse/", process, ".rds"))
  saveRDS(coverage, file = paste0(subDir, "/coverage/coverage", process, ".rds"))
  saveRDS(tu_coverage, file = paste0(subDir, "/coverage/tu_coverage", process, ".rds"))
} else {
  dir.create(file.path(subDir))
  dir.create(file.path(paste0(subDir, "/mse")))
  dir.create(file.path(paste0(subDir, "/coverage")))
  saveRDS(diff, file = paste0(subDir, "/mse/", process, ".rds"))
  saveRDS(coverage, file = paste0(subDir, "/coverage/coverage", process, ".rds"))
  saveRDS(tu_coverage, file = paste0(subDir, "/coverage/tu_coverage", process, ".rds"))
}



