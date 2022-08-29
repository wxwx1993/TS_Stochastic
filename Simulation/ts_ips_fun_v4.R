library("ranger")
library("locfit")
require("SuperLearner")
require("earth")
require("gam")
require("ranger")
require("rpart")
require("xgboost")

sl.lib = c("SL.earth", "SL.gam", "SL.glm", "SL.glm.interaction", "SL.mean", "SL.ranger", "SL.rpart", "SL.xgboost")

ts_ipsi_true.ipw <- function(dat, x.trt, t_lag = 1, delta.seq, nsplits = 1){
  # setup storage
  ntimes <- length(table(dat$time))
  n <- length(unique(dat$id))
  T <- max(dat$time)
  k <- length(delta.seq)
  a_lag1 <- dat$a
  a_lag1[rep(2:T, n) + rep(seq(0, (n-1)*T,T),rep(T-1, n))] <- dat$a[-seq(T,n*T,T)]
  a_lag1[seq(0,(n*T-1),T) + 1] <- 0

  dat$ps <- expit(10*(rowMeans(x.trt) - a_lag1 + 0.5))
  end = max(dat$time)
  est.eff.ipw <- matrix(NA, nrow = n*(end-t_lag), ncol = k)
 
  for (j in 1:k){
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        est.eff.ipw[(n_i-1)*(ntimes-t_lag) + t, j] = prod(sapply(t:(t+t_lag),function(s){
          delta_t = delta.seq[j]
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
        }))* ((dat$a*3 + a_lag1*1 + rowMeans(x.trt))[(n_i-1)*(ntimes-t_lag)+t+t_lag]) 
      }
    }
  }
  
  return(list(est.eff = est.eff.ipw))
}


ts_ipsi_true.mis <- function(dat, x.trt, t_lag = 1, delta.seq, nsplits = 1){
  # setup storage
  ntimes <- length(table(dat$time))
  n <- length(unique(dat$id))
  T <- max(dat$time)
  k <- length(delta.seq)
  a_lag1 <- dat$a
  a_lag1[rep(2:T, n) + rep(seq(0, (n-1)*T,T),rep(T-1, n))] <- dat$a[-seq(T,n*T,T)]
  a_lag1[seq(0,(n*T-1),T) + 1] <- 0
  
  dat$ps <- expit(5*expit(10*(rowMeans(x.trt) - a_lag1))-3)
  end = max(dat$time)
  est.eff.ipw <- matrix(NA, nrow = n*(end-t_lag), ncol = k)
  
  for (j in 1:k){
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        est.eff.ipw[(n_i-1)*(ntimes-t_lag) + t, j] = prod(sapply(t:(t+t_lag),function(s){
          delta_t = delta.seq[j]
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
        }))* ((dat$a*3 + a_lag1*1 + rowMeans(x.trt))[(n_i-1)*(ntimes-t_lag)+t+t_lag])
      }
    }
  }
  
  return(list(est.eff = est.eff.ipw))
}


ts_ipsi.ipw <- function(dat, x.trt, t_lag = 1, delta.seq, nsplits = 1, model = "glm"){
  # setup storage
  ntimes <- length(table(dat$time)) 
  n <- length(unique(dat$id))
  T <- max(dat$time)
  k <- length(delta.seq)
  a_lag1 <- dat$a
  a_lag1[rep(2:T, n) + rep(seq(0, (n-1)*T,T),rep(T-1, n))] <- dat$a[-seq(T, n * T, T)]
  a_lag1[seq(0,(n*T-1),T) + 1] <- 0

  if (model == "glm") {
    trtmod <- glm(a ~ ., data=cbind(x.trt, a = dat$a, a_lag1 = a_lag1), family=binomial())
    dat$ps <- predict(trtmod, data=cbind(x.trt, a_lag1 = a_lag1), type = "response")
  } else if (model == "true") {
    # true model
    dat$ps <- expit(10*(rowMeans(x.trt) - a_lag1 + 0.5))
  } else if (model == "SL") {
    trtmod <- SuperLearner(dat$a, cbind(x.trt, a_lag1 = a_lag1),
                           newX = cbind(x.trt, a_lag1 = a_lag1), 
                           SL.library = sl.lib, 
                           family = binomial)
    dat$ps <- trtmod$SL.predict
  }
  end = max(dat$time)
  est.eff.ipw <- matrix(NA, nrow = n*(end - t_lag), ncol = k)
  for (j in 1:k){
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        est.eff.ipw[(n_i-1)*(ntimes-t_lag) + t,j] = prod(sapply(t:(t+t_lag),function(s){
          delta_t = delta.seq[j]
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
             (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
        }))*dat$y[(n_i-1)*(ntimes-t_lag)+t+t_lag]
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
        }))*dat$y[(n_i-1)*(ntimes-t_lag)+t+t_lag]^2 
      }
    }
  }
  
  return(list(est.eff = est.eff.ipw, 
              est.var1.ipw = est.var1.ipw))
}

ts_ipsi.dr <- function(dat, x.trt, t_lag = 1, delta.seq, nsplits = 1, model = "glm"){
  ntimes <- length(table(dat$time)) 
  n <- length(unique(dat$id))
  T <- max(dat$time)
  k <- length(delta.seq)
  a_lag1 <- dat$a
  a_lag1[rep(2:T, n) + rep(seq(0, (n-1)*T,T),rep(T-1, n))] <- dat$a[-seq(T, n * T, T)]
  a_lag1[seq(0,(n*T-1),T) + 1] <- 0
  
  if (model == "glm") {
    trtmod <- glm(a ~ ., data=cbind(x.trt, a = dat$a, a_lag1 = a_lag1), family=binomial())
    dat$ps <- predict(trtmod, data=cbind(x.trt, a_lag1 = a_lag1), type = "response")
  } else if (model == "true") {
    # true model
    dat$ps <- expit(10*(rowMeans(x.trt) - a_lag1 + 0.5))
  } else if (model == "SL") {
    trtmod <- SuperLearner(dat$a, cbind(x.trt, a_lag1 = a_lag1),
                           newX = cbind(x.trt, a_lag1 = a_lag1), 
                           SL.library = sl.lib, 
                           family = binomial)
    dat$ps <- trtmod$SL.predict
  }
  # fit outcome models
  # counterfactual case for treatment: A = 1
  newx1 <- x.trt
  newx1$a <- 1
  newx1$a_lag1 <- a_lag1
  
  # counterfactual case for no treatment: A = 0
  newx0 <- x.trt
  newx0$a <- 0
  newx0$a_lag1 <- a_lag1
  
  if (model=="glm"){
    outmod <- glm(y ~ ., data=cbind(y = y, x.trt, a = dat$a, a_lag1 = a_lag1), family=gaussian())
    m1 <- predict(outmod, data = newx1)
    m0 <- predict(outmod, data = newx0)
  }
  
  if (model=="SL"){
    outmod <- SuperLearner(dat$y, cbind(x.trt, a = a, a_lag1 = a_lag1),
                           newX = rbind(newx1, newx0), 
                           SL.library = sl.lib)
    
    m1 <- outmod$SL.predict[1:dim(newx1)[1]]
    m0 <- outmod$SL.predict[(dim(newx1)[1]+1):(dim(newx1)[1]+dim(newx0)[1])]
  }
  
  end = max(dat$time)
  cumwt <- matrix(NA, nrow = n*(end - t_lag), ncol = k)
  cumout <- matrix(NA, nrow = n*(end - t_lag), ncol = k)
  est.eff.dr <- matrix(NA, nrow = n*(end - t_lag), ncol = k)
  est.eff.ipw2 <- matrix(NA, nrow = n*(end - t_lag), ncol = k)
  for (j in 1:k){
    delta_t = delta.seq[j]
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        cumwt[(n_i-1)*(ntimes-t_lag) + t,j] = prod(sapply(t:(t+t_lag),function(s){
          wt <- (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
                   (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
          return(wt)
        }))
        
        cumout[(n_i-1)*(ntimes-t_lag) + t,j] <- sum(sapply(t:(t+t_lag),function(s){
          rt <- (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]*m1[(n_i-1)*(ntimes-t_lag)+s] +
                   (1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])*m0[(n_i-1)*(ntimes-t_lag)+s]) /
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
          
          vt <- (dat$a[(n_i-1)*(ntimes-t_lag)+s]*(1-dat$ps[(n_i-1)*(ntimes-t_lag)+s]) -
                   (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])*delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s])/(delta_t/(1-delta_t))

          cumwt1 = prod(sapply(1:s,function(s1){
            wt <- (dat$a[(n_i-1)*(ntimes-t_lag)+s1]*delta_t + 
                     (1-dat$a[(n_i-1)*(ntimes-t_lag)+s1])) / 
              (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s1]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s1])
            return(wt)
          }))
          return(rt*vt*cumwt1)
        }))
        est.eff.dr[(n_i-1)*(ntimes-t_lag) + t,j] <- cumwt[(n_i-1)*(ntimes-t_lag) + t,j]*dat$y[(n_i-1)*(ntimes-t_lag)+t+t_lag] +
          cumout[(n_i-1)*(ntimes-t_lag) + t,j]
      #  est.eff.ipw2[(n_i-1)*(ntimes-t_lag) + t,j] <- cumwt[(n_i-1)*(ntimes-t_lag) + t,j]*dat$y[(n_i-1)*(ntimes-t_lag)+t+t_lag]
      }
    }
  }
  
  return(list(est.eff = est.eff.dr))
}

ts_ipsi_true.kang <- function(dat, x.trt, t_lag = 1, delta.seq, nsplits = 1){
  # setup storage
  ntimes <- length(table(dat$time))
  n <- length(unique(dat$id))
  T <- max(dat$time)
  k <- length(delta.seq)
  a_lag1 <- dat$a
  a_lag1[rep(2:T, n) + rep(seq(0, (n-1)*T,T),rep(T-1, n))] <- dat$a[-seq(T,n*T,T)]
  a_lag1[seq(0,(n*T-1),T) + 1] <- 0

  dat$ps <- expit(10*(rowMeans(x.trt) - a_lag1 + 0.5))
  end = max(dat$time)
  est.eff.ipw <- matrix(NA, nrow = n*(end-t_lag), ncol = k)
 
  for (j in 1:k){
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        est.eff.ipw[(n_i-1)*(ntimes-t_lag) + t, j] = prod(sapply(t:(t+t_lag),function(s){
          delta_t = delta.seq[j]
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
        }))* ((dat$a*3 + a_lag1*1 + rowMeans(x.trt))[(n_i-1)*(ntimes-t_lag)+t+t_lag]) 
      }
    }
  }
  
  return(list(est.eff = est.eff.ipw))
}
