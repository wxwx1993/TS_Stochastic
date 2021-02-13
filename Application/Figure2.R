# Code to generate Figure 2
library(dplyr)

# Figure 4: NWS-issued heat alerts for Los Angeles county 2006-2016.
daily_LA_main <- subset(daily_fips_main, fips == "06037")

dat_fips_1 <- daily_LA_main
colnames(dat_fips_1)[17] <- "N"
dat_fips_2 <- dat_fips_1 %>%
  group_by(year) %>%
  mutate(HImaxF_median = median(HImaxF_PopW, na.rm = T))
dat_fips_1$HImaxF_PopW[is.na(dat_fips_1$HImaxF_PopW)] <- dat_fips_2$HImaxF_median[is.na(dat_fips_1$HImaxF_PopW)] 

unit <- nrow(dat_fips_1)/214
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
# Model
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

threshold <- 0
ntimes <- length(table(dat$time)) 
n <- length(unique(dat$id))

trtmod <- SuperLearner(dat$a, x.trt,
                       newX = x.trt, 
                       SL.library = sl.lib, 
                       family = binomial)
dat$ps <- trtmod$SL.predict
end <- max(dat$time)
ps_inc <- matrix(NA, nrow = n * (end), ncol = 1)
for (n_i in 1:n){
  for (t in 1:(ntimes)){
    ps_inc[(n_i-1)*(ntimes) + t] <- sapply(t, function(s) {
      delta_t <- delta.seq[50]
      (delta_t * dat$ps[(n_i - 1) * (ntimes) + s]) / 
        (delta_t * dat$ps[(n_i - 1) * (ntimes) + s] + 1 - dat$ps[(n_i - 1) * (ntimes) + s])
    })
  }
}

dat_fips_1$ps_inc <- ps_inc
dat_fips_1$a_inc <- sapply(1:2354, 
                           function(i) sample(c(0, 1),
                                              1,
                                              prob = c(1 - dat_fips_1$ps_inc[i], dat_fips_1$ps_inc[i])))

plot(dat_fips_1$Date, dat_fips_1$HImaxF_PopW, type = "l", ylab = "Heat Index", xlab = "Date")
a <- subset(dat_fips_1, a_inc == 1)
lines(a$Date, a$HImaxF_PopW, type = "p", col = "red")

b <- subset(dat_fips_1, alert == 1)
lines(b$Date, b$HImaxF_PopW, type = "p", col = "blue", pch = 4)
