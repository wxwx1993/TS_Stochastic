# Code to generate Figure 4
library("ggplot2")
library("grid")
library("gridExtra")

set.seed(3)
t_lag <- 1
n <- 1

# senario with time length 200
T <- 200
model <- "SL"
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
y <- rnorm(mean = (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
dat <- data.frame(time = time ,id = id, y = y, a = a)
delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq, model = model)
true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq)

Sim100 <- data.frame(logOR = log(delta.seq),
                     estimated = colMeans(test$est.eff), 
                     true = colMeans(true$est.eff), 
                     sd_true = sqrt((colMeans(test$est.var1.ipw) - colMeans(test$est.eff)^2) / T),
                     sd_upper = sqrt((colMeans(test$est.var2.ipw)) / T))

# senario with time length 1000
T <- 1000
model <- "SL"
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
y <- rnorm(mean = (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
dat <- data.frame(time = time, id = id, y = y, a = a)
delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq, model = model)
true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq)

Sim1000 <- data.frame(logOR = log(delta.seq),
                      estimated = colMeans(test$est.eff), 
                      true = colMeans(true$est.eff), 
                      sd_true = sqrt((colMeans(test$est.var1.ipw) - colMeans(test$est.eff)^2) / T),
                      sd_upper = sqrt((colMeans(test$est.var2.ipw)) / T))

# senario with time length 5000
T <- 5000
model <- "SL"
time <- rep(1:T, n)
id <- rep(1:n, rep(T, n))
x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

a_lag1 <- rep(0, n * T)
a <- NULL
for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  a_lag1[i + 1] <- a[i]
}
a_lag1 <- a_lag1[1:(n * T)]
a_lag1[seq(1, n*T, T)] <- 0
y <- rnorm(mean = (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
dat <- data.frame(time = time, id = id, y = y, a = a)
delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq, model = model)
true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, threshold = 0, delta.seq = delta.seq)

Sim5000 <- data.frame(logOR = log(delta.seq),
                      estimated = colMeans(test$est.eff), 
                      true = colMeans(true$est.eff), 
                      sd_true = sqrt((colMeans(test$est.var1.ipw) - colMeans(test$est.eff)^2) / T),
                      sd_upper = sqrt((colMeans(test$est.var2.ipw)) / T))

# Figure 2
p100 <- ggplot(data = Sim100, aes(x = logOR, y = estimated)) +
  geom_line(aes(x = logOR, y = estimated), color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = true), color = "blue", lwd = 1.2) +
  
  geom_line(aes(x = logOR, y = estimated + 1.96 * sd_true), linetype = "dashed", color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = estimated - 1.96 * sd_true), linetype = "dashed", color = "red", lwd = 1.2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.2),
        text = element_text(size = 20),
        axis.title.x = element_text(size = 22)) +
  ggtitle(expression(paste("T = 200"))) + 
  xlab(expression(paste("log odds ratio ", delta))) +
  ylab("") 

p1000 <- ggplot(data = Sim1000, aes(x = logOR, y = estimated)) +
  geom_line(aes(x = logOR, y = estimated), color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = true), color = "blue", lwd = 1.2) +
  
  geom_line(aes(x = logOR, y = estimated + 1.96 * sd_true), linetype = "dashed", color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = estimated - 1.96 * sd_true), linetype = "dashed", color = "red", lwd = 1.2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.2),
        text = element_text(size = 20),
        axis.title.x = element_text(size = 22)) +
  ggtitle(expression(paste("T = 1000"))) + 
  xlab(expression(paste("log odds ratio ", delta))) +
  ylab("") 

p5000 <- ggplot(data = Sim5000, aes(x = logOR, y = estimated)) +
  geom_line(aes(x = logOR, y = estimated), color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = true), color = "blue", lwd = 1.2) +
  
  geom_line(aes(x = logOR, y = estimated + 1.96 * sd_true), linetype = "dashed", color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = estimated - 1.96 * sd_true), linetype = "dashed", color = "red", lwd = 1.2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.2),
        text = element_text(size = 20),
        axis.title.x = element_text(size = 22)) +
  ggtitle(expression(paste("T = 5000"))) + 
  xlab(expression(paste("log odds ratio ", delta))) +
  ylab("") 


grid.arrange(p100, p1000, p5000, 
             nrow = 1, 
             top = textGrob(expression(paste("Causal Effect Curve")), 
                            gp = gpar(fontsize=28)))

save(Sim100, Sim1000, Sim5000, file = paste0("Sim_", t_lag, ".RData"))
