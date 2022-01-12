library(fst)
library(data.table)
library(parallel)
library(stringr)
library(dplyr)
library(timeDate)
library("ranger")
require("SuperLearner")
require("earth")
require("gam")
require("ranger")
require("rpart")
library("ggplot2")
library("grid")
library("gridExtra")


delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
sl.lib = c("SL.earth", "SL.gam", "SL.glm", "SL.glm.interaction", "SL.mean", "SL.ranger", "SL.rpart")

ts_ipsi.ipw <- function(dat, x.trt, t_lag = 1, delta.seq, nsplits = 1, model = "glm"){
  # setup storage
  ntimes <- length(table(dat$time)) 
  n <- length(unique(dat$id))
  k <- length(delta.seq)
  if (model == "glm") {
    trtmod <- glm(a ~ ., data=cbind(x.trt, a=dat$a), family=binomial())
    dat$ps <- predict(trtmod, data=x.trt, type = "response")
  } else if (model == "ranger") {
    # fit treatment model
    trtmod <- ranger(a ~ ., data=cbind(x.trt, a = dat$a)) 
    dat$ps <- predict(trtmod, data = x.trt)$predictions
  } else if (model == "SL") {
    trtmod <- SuperLearner(dat$a, x.trt,
                           newX = x.trt, 
                           SL.library = sl.lib, 
                           family = binomial)
    dat$ps <- trtmod$SL.predict
  }
  end = max(dat$time)
  est.eff.ipw <- matrix(NA, nrow = n*(end-t_lag), ncol = k)
  for (j in 1:k){
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        est.eff.ipw[(n_i-1)*(ntimes-t_lag) + t,j] = prod(sapply(t:(t+t_lag),function(s){
          delta_t = delta.seq[j]
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
        }))*dat$y[(n_i-1)*(ntimes-t_lag) + t + t_lag]
      }
    }
  }
  est.var1.ipw <- matrix(NA, nrow = n*(end-t_lag), ncol = k)

  for (j in 1:k){
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        est.var1.ipw[(n_i-1)*(ntimes-t_lag) + t, j] = prod(sapply(t:(t+t_lag), function(s){
          delta_t = delta.seq[j]
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t^2 + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])^2
        }))*dat$y[(n_i-1)*(ntimes-t_lag) + t + t_lag]^2

      }
    }
  }
  return(list(est.eff = est.eff.ipw, 
              est.var1.ipw = est.var1.ipw)) 
}
