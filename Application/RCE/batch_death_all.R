source("ts_ipw_function.R")

# input raw data: US census
fips_popsize <- read.csv("/nfs/home/X/xwu/shared_space/ci3_xwu/ts_ips/matched_county_2010_popsize.csv")
colnames(fips_popsize)[1] <- "fips"
fips_popsize$fips <- str_pad(fips_popsize$fips, 5, pad = "0")

# input raw data: Medicare mortality
daily_fips_deaths <- read_fst("/nfs/home/X/xwu/shared_space/ci3_xwu/ts_ips/daily_fips_deaths_1999_2016.fst")
daily_fips_deaths$fips <- str_pad(daily_fips_deaths$fips, 5, pad = "0")
colnames(daily_fips_deaths)[1] <- "Date"

# input raw data: Heat alert and heat index
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

daily_fips_main <- data.frame(merge(daily_fips_heat,
                                    daily_fips_deaths,
                                    by = c("fips", "Date"),
                                    all.x = TRUE))

# implement the proposed TS_incremental approach
model <- "SL"
t_lag <- c(0:5)[as.integer(as.character(commandArgs(trailingOnly = TRUE))) + 1]
num_county <- length(table(daily_fips_main$fips))

est.eff <- lapply(1:num_county, function(i, type_disease, t_lag = t_lag, model = model) {

  # select data for each country
  dat_fips_1 <- subset(daily_fips_main, 
                       fips == unique(daily_fips_main$fips)[i])
  colnames(dat_fips_1)[17] <- "N"
  dat_fips_2 <- dat_fips_1 %>%
    group_by(year) %>%
    mutate(HImaxF_median = median(HImaxF_PopW, na.rm = T))
  dat_fips_1$HImaxF_PopW[is.na(dat_fips_1$HImaxF_PopW)] <- dat_fips_2$HImaxF_median[is.na(dat_fips_1$HImaxF_PopW)] 
  
  unit <- nrow(dat_fips_1) / 214
  dat_fips_1$time <- rep(1:(nrow(dat_fips_1)/unit), unit)
  dat_fips_1$id <- rep(1:unit,rep(nrow(dat_fips_1)/unit, unit))
  dat_fips_1$HI_lag1 <- c(rep(median(dat_fips_1$HImaxF_PopW), 1),
                          dat_fips_1$HImaxF_PopW[1:(nrow(dat_fips_1) - 1)])
  dat_fips_1$HI_lag2 <- c(rep(median(dat_fips_1$HImaxF_PopW),2),
                          dat_fips_1$HImaxF_PopW[1:(nrow(dat_fips_1) - 2)])
  
  dat_fips_1$alert_lag1 <- c(rep(0,1), dat_fips_1$alert[1:(nrow(dat_fips_1) - 1)])
  dat_fips_1$alert_lag2 <- c(rep(0,2), dat_fips_1$alert[1:(nrow(dat_fips_1) - 2)])
  
  dat_fips_1 <- dat_fips_1 %>%
    group_by(id, year) %>%
    mutate(HI_mean = cummean(ifelse(is.na(HImaxF_PopW), 0, HImaxF_PopW)),
           alert_sum = cumsum(ifelse(is.na(alert), 0, alert)),
           death_mean = cummean(ifelse(is.na(N), 0, N)))
  
  # prepare data for model input
  x.trt <- dat_fips_1[, c("HImaxF_PopW", 
                          "holiday",
                          "HI_lag1",
                          "HI_lag2",
                          "alert_lag1",
                          "alert_lag2",
                          "HI_mean",
                          "alert_sum",
                          "death_mean")]
  dat <- dat_fips_1[, c("time",
                        "id",
                        "N",
                        "alert")]
  colnames(dat)[4] <- "a"
  colnames(dat)[3] <- "y"
  
  result <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq, model = model)
  return(list(colMeans(result$est.eff, na.rm = TRUE), colMeans(result$est.var1.ipw, na.rm = TRUE)))
  
}, type_disease = type_disease, t_lag = t_lag, model = model)


mean.est.eff <- do.call("rbind", lapply(est.eff, `[[`, 1))
mean.var1 <- do.call("rbind", lapply(est.eff, `[[`, 2))
T <- 214
N <- length(est.eff)
var_true <- (mean.var1 - (mean.est.eff)^2)[1:50,] / (T * N)
var_pooled <- 1 / colMeans(1 / var_true)

estimated <- data.frame(logOR = log(delta.seq),
                        estimated = colMeans(mean.est.eff), 
                        sd_true = sqrt(var_pooled))

save(est.eff, estimated, file = paste0("~/shared_space/ci3_xwu/ts_ips/result_hosp_all/deaths",
                                       "_t", t_lag, "_", model, ".RData"))
