library(fst)
library(data.table)
library(parallel)
library(stringr)
library(dplyr)
library(timeDate)
library("ranger")
library("locfit")
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

ts_ipsi.ipw <- function(dat, x.trt, t_lag = 1, threshold = 100, delta.seq, nsplits = 1, model = "glm"){
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
          #delta_t = delta.seq[j]
          delta_t = ifelse(x.trt$HImaxF_PopW[(n_i-1)*(ntimes-t_lag)+s] > threshold, delta.seq[j], 1)
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
        }))*dat$y[(n_i-1)*(ntimes-t_lag)+t]
      }
    }
  }
  est.var1.ipw <- matrix(NA, nrow = n*(end-t_lag), ncol = k)
  est.var2.ipw <- matrix(NA, nrow = n*(end-t_lag), ncol = k)
  for (j in 1:k){
    for (n_i in 1:n){
      for (t in 1:(ntimes-t_lag)){
        est.var1.ipw[(n_i-1)*(ntimes-t_lag) + t, j] = prod(sapply(t:(t+t_lag), function(s){
          #delta_t = delta.seq[j]
          delta_t = ifelse(x.trt$HImaxF_PopW[(n_i-1)*(ntimes-t_lag)+s] > threshold, delta.seq[j], 1)
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t^2 + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])^2
        }))*dat$y[(n_i-1)*(ntimes-t_lag)+t]^2 #- (est.eff.ipw[(n_i-1)*(ntimes-t_lag) + t, j])^2
        est.var2.ipw[(n_i-1)*(ntimes-t_lag) + t, j] = prod(sapply(t:(t+t_lag), function(s){
          delta_t = delta.seq[j]
          (dat$a[(n_i-1)*(ntimes-t_lag)+s]*delta_t + 
              (1-dat$a[(n_i-1)*(ntimes-t_lag)+s])) / 
            (delta_t*dat$ps[(n_i-1)*(ntimes-t_lag)+s]+1-dat$ps[(n_i-1)*(ntimes-t_lag)+s])
        }))*dat$y[(n_i-1)*(ntimes-t_lag)+t]^2
      }
    }
  }
  return(list(est.eff = est.eff.ipw, 
              est.var1.ipw = est.var1.ipw,
              est.var2.ipw = est.var2.ipw)) 
}

fips_popsize <- read.csv("/nfs/home/X/xwu/shared_space/ci3_xwu/ts_ips/matched_county_2010_popsize.csv")
colnames(fips_popsize)[1] <- "fips"
fips_popsize$fips <- str_pad(fips_popsize$fips, 5, pad = "0")

f <- list.files("/nfs/nsaph_ci3/ci3_health_data/medicare/heat_related/2006_2016/county_ccs_hosps/data/",
                pattern = "\\.fst",
                full.names = TRUE)
myvars <- c("fips","day","ccs_55","ccs_157","ccs_159","ccs_2","ccs_244","ccs_114","ccs_50")

daily_fips_hosp <- rbindlist(lapply(f,
                                    read_fst,
                                    columns = myvars,
                                    as.data.table=TRUE))
daily_fips_hosp$fips <- str_pad(daily_fips_hosp$fips, 5, pad = "0")
colnames(daily_fips_hosp)[2] <- "Date"
daily_fips_deaths <- read_fst("/nfs/home/X/xwu/shared_space/ci3_xwu/ts_ips/daily_fips_deaths_1999_2016.fst")
daily_fips_deaths$fips <- str_pad(daily_fips_deaths$fips, 5, pad = "0")
colnames(daily_fips_deaths)[1] <- "Date"
daily_fips_heat <- readRDS("/nfs/home/X/xwu/shared_space/ci3_xwu/ts_ips/ts_heatindex_heatwarnings_byFIPS_2006-2016.RDS")
colnames(daily_fips_heat)[1] <- "fips"
daily_fips_heat$fips <- str_pad(daily_fips_heat$fips, 5, pad = "0")
holidays <- c(as.Date(USMemorialDay(2006:2016)),
              as.Date(USIndependenceDay(2006:2016)),
              as.Date(USLaborDay(2006:2016)),
              as.Date(USColumbusDay(2006:2016)))
daily_fips_heat$holiday <- ifelse(daily_fips_heat$Date %in% holidays, 1, 0)
daily_fips_heat$year <- year(daily_fips_heat$Date)
#[1] 6678298 = 214*11*2837

daily_fips_heat <- merge(daily_fips_heat,
                         fips_popsize,
                         by = "fips",
                         all.x = TRUE)

daily_fips_heat_populous <- subset(daily_fips_heat, CENSUS2010POP > 100000)
# 1294700 = 214*11*550
daily_fips_main <- merge(daily_fips_heat_populous,
                         daily_fips_deaths,
                         by = c("fips", "Date"),
                         all.x = TRUE)

daily_fips_suff <- subset(daily_fips_main, 
                          fips == unique(daily_fips_main$fips)[1])
dat_fips_1 <- subset(daily_fips_suff)#, Date > "2016-01-01")

#dat_fips_1[dat_fips_1$alert==0,]$N =10
#dat_fips_1[dat_fips_1$alert==1,]$N =1

unit <- nrow(dat_fips_1)/214
dat_fips_1$time = rep(1:(nrow(dat_fips_1)/unit),unit)
dat_fips_1$id = rep(1:unit,rep(nrow(dat_fips_1)/unit,unit))
dat_fips_1$HI_lag1 = c(rep(median(dat_fips_1$HImaxF_PopW),1),
                       dat_fips_1$HImaxF_PopW[1:(nrow(dat_fips_1)-1)])
dat_fips_1$HI_lag2 = c(rep(median(dat_fips_1$HImaxF_PopW),2),
                       dat_fips_1$HImaxF_PopW[1:(nrow(dat_fips_1)-2)])
#dat_fips_1$HI_mean = cumsum(dat_fips_1$HImaxF_PopW) / seq_along(dat_fips_1$HImaxF_PopW) 
dat_fips_1$alert_lag1 = c(rep(0,1),dat_fips_1$alert[1:(nrow(dat_fips_1)-1)])
dat_fips_1$alert_lag2 = c(rep(0,2),dat_fips_1$alert[1:(nrow(dat_fips_1)-2)])
#dat_fips_1$alert_sum = cumsum(dat_fips_1$alert)

dat_fips_1 <- dat_fips_1 %>%
  group_by(id, year) %>%
  mutate(HI_mean = cummean(ifelse(is.na(HImaxF_PopW), 0, HImaxF_PopW)),
         alert_sum = cumsum(ifelse(is.na(alert), 0, alert)),
         death_mean = cummean(ifelse(is.na(N), 0, N)))


num_county = length(table(daily_fips_main$fips))
mean.est.eff = mcmapply(function(i) {
  #for (i in 1:100){
  dat_fips_1 <- subset(daily_fips_main, 
                       fips == unique(daily_fips_main$fips)[i])
  dat_fips_2 <- dat_fips_1 %>%
    group_by(year) %>%
    mutate(HImaxF_median = median(HImaxF_PopW, na.rm = T))
  dat_fips_1$HImaxF_PopW[is.na(dat_fips_1$HImaxF_PopW)] <- dat_fips_2$HImaxF_median[is.na(dat_fips_1$HImaxF_PopW)] 
  
  unit <- nrow(dat_fips_1)/214
  dat_fips_1$time = rep(1:(nrow(dat_fips_1)/unit),unit)
  dat_fips_1$id = rep(1:unit,rep(nrow(dat_fips_1)/unit,unit))
  dat_fips_1$HI_lag1 = c(rep(median(dat_fips_1$HImaxF_PopW),1),
                         dat_fips_1$HImaxF_PopW[1:(nrow(dat_fips_1)-1)])
  dat_fips_1$HI_lag2 = c(rep(median(dat_fips_1$HImaxF_PopW),2),
                         dat_fips_1$HImaxF_PopW[1:(nrow(dat_fips_1)-2)])
  
  dat_fips_1$alert_lag1 = c(rep(0,1),dat_fips_1$alert[1:(nrow(dat_fips_1)-1)])
  dat_fips_1$alert_lag2 = c(rep(0,2),dat_fips_1$alert[1:(nrow(dat_fips_1)-2)])
  
  dat_fips_1 <- dat_fips_1 %>%
    group_by(id, year) %>%
    mutate(HI_mean = cummean(ifelse(is.na(HImaxF_PopW), 0, HImaxF_PopW)),
           alert_sum = cumsum(ifelse(is.na(alert), 0, alert)),
           death_mean = cummean(ifelse(is.na(N), 0, N)))
  # Model
  x.trt = dat_fips_1[,c("HImaxF_PopW", 
                        "holiday",
                        "HI_lag1",
                        "HI_lag2",
                        "alert_lag1",
                        "alert_lag2",
                        "HI_mean",
                        "alert_sum",
                        "death_mean")]
  dat = dat_fips_1[,c("time",
                      "id",
                      "N",
                      "alert")]
  colnames(dat)[4] = "a"
  colnames(dat)[3] = "y"
  
  test = ts_ipsi.ipw(dat = dat, x.trt, t_lag = 2, threshold = 105, delta.seq = delta.seq, nsplits = 1, model = "SL")
  
  #mean.est.eff[i,] =  colMeans(test$est.eff, na.rm = TRUE)
  #mean.var1[i,] <- colMeans(test$est.var1.ipw, na.rm = TRUE)
  return(c(colMeans(test$est.eff, na.rm = TRUE), colMeans(test$est.var1.ipw, na.rm = TRUE)))
},1:10,mc.cores = 16)
