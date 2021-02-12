# Code to conduct random-effect meta analysis to data applications.
library(parallel)
library(meta)
library(metafor)
library(dplyr)

t_lag <- 2

for (i in c("deaths", 17:21)) {
  load(paste0(Dir,i,"_t",t_lag, "_SL.RData"))
  mean.est.eff <- do.call("rbind", lapply(est.eff, `[[`, 1))
  mean.var1 <- do.call("rbind", lapply(est.eff, `[[`, 2))
  T <- 214
  N <- length(est.eff)
  sd_true <- sqrt((mean.var1 - (mean.est.eff)^2)[1:50, ] / (T))
  
  curves <- as.data.frame(t(mcmapply(function(delta) {
    madata <- as.data.frame(cbind(TE = mean.est.eff[, delta],
                                  seTE = sd_true[, delta]))
    m <- metagen(TE,
                 seTE,
                 data=madata,
                 comb.fixed = TRUE,
                 comb.random = TRUE,
                 prediction = FALSE)
    return(c(m$TE.fixed, m$lower.fixed, m$upper.fixed, 
             m$TE.random, m$lower.random, m$upper.random))
    
  }, 1:50, mc.cores = 10)))
  delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
  
  Results = data.frame(logOR = log(delta.seq),
                       estimated = curves$V4, 
                       lower = curves$V5,
                       upper = curves$V6,
                       fixed.estimated = curves$V1, 
                       fixed.lower = curves$V2,
                       fixed.upper = curves$V3)
  save(curves, Results, name_disease, file = paste0(Dir, "meta_result/", i, "_", t_lag, "_meta.RData"))
}

