# Code to generate Figure 2
library("ggplot2")
library("grid")
library("gridExtra")

set.seed(3)
t_lag <- 1
n = 1
T = 200
model = "SL"
time <- rep(1:T, n)
id <- rep(1:n, rep(T, n))
x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

a_lag1 = rep(0, n * T)
a = NULL
ps = NULL
for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  ps[i] <- expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5))
  a_lag1[i+1] <- a[i]
}
a_lag1 = a_lag1[1:(n * T)]
a_lag1[seq(1,n*T,T)] <- 0
y <- rnorm(mean= (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
dat <- data.frame(time = time ,id = id, y = y, a = a)
delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1, model = model)
true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1)

Sim100 = data.frame(logOR = log(delta.seq),
                    estimated = colMeans(test$est.eff), 
                    true = colMeans(true$est.eff), 
                    sd_true = 1/(T-t_lag) * sqrt(colSums(test$est.var1.ipw)))

n <- 1
T <- 1000
model = "SL"
time <- rep(1:T, n)
id <- rep(1:n, rep(T, n))
x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

a_lag1 = rep(0, n * T)
a = NULL
for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  a_lag1[i+1] <- a[i]
}
a_lag1 = a_lag1[1:(n * T)]
a_lag1[seq(1,n*T,T)] <- 0
y <- rnorm(mean= (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
dat <- data.frame(time = time ,id = id, y = y, a = a)
delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1, model = model)
true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1)

Sim1000 = data.frame(logOR = log(delta.seq),
                     estimated = colMeans(test$est.eff), 
                     true = colMeans(true$est.eff), 
                     sd_true = 1/(T-t_lag) * sqrt(colSums(test$est.var1.ipw)))

n <- 1
T <- 5000
model = "SL"
time <- rep(1:T, n)
id <- rep(1:n, rep(T, n))
x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

a_lag1 = rep(0, n * T)
a = NULL
for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  a_lag1[i+1] <- a[i]
}
a_lag1 = a_lag1[1:(n * T)]
a_lag1[seq(1,n*T,T)] <- 0
y <- rnorm(mean= (a * 3 + a_lag1 * 1 + rowMeans(x.trt)), n * T)
dat <- data.frame(time = time ,id = id, y = y, a = a)
delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
test <- ts_ipsi.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1, model = model)
true <- ts_ipsi_true.ipw(dat = dat, x.trt, t_lag = t_lag, delta.seq = delta.seq, nsplits = 1)

Sim5000 = data.frame(logOR = log(delta.seq),
                     estimated = colMeans(test$est.eff), 
                     true = colMeans(true$est.eff), 
                     sd_true = 1/(T-t_lag) * sqrt(colSums(test$est.var1.ipw)))


p100 = ggplot(data=Sim100, aes(x = logOR, y = estimated)) +
  geom_line(aes(x= logOR, y= estimated) ,color="red", lwd=1.2) +
  geom_line(aes(x= logOR, y= true) ,color="blue", lwd=1.2) +
  
  geom_line(aes(x= logOR, y= estimated+1.96*sd_true),linetype="dashed" ,color="red", lwd=1.2) +
  geom_line(aes(x= logOR, y= estimated-1.96*sd_true),linetype="dashed" ,color="red" , lwd=1.2) +
  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.2),
        text = element_text(size=20),
        axis.title.x = element_text(size = 22)) +
  ggtitle(expression(paste("T = 200"))) + 
  xlab(expression(paste("Log ", delta))) +
  ylab("") +
  ylim(1.45, 2.35)

p1000 = ggplot(data=Sim1000, aes(x = logOR, y = estimated)) +
  geom_line(aes(x= logOR, y= estimated) ,color="red", lwd=1.2) +
  geom_line(aes(x= logOR, y= true) ,color="blue", lwd=1.2) +
  
  geom_line(aes(x= logOR, y= estimated+1.96*sd_true),linetype="dashed" ,color="red", lwd=1.2) +
  geom_line(aes(x= logOR, y= estimated-1.96*sd_true),linetype="dashed" ,color="red" , lwd=1.2) +

  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.2),
        text = element_text(size=20),
        axis.title.x = element_text(size = 22)) +

  ggtitle(expression(paste("T = 1000"))) + 
  xlab(expression(paste("Log ", delta))) +
  ylab("")  +
  ylim(1.45, 2.35)

p5000 = ggplot(data=Sim5000, aes(x = logOR, y = estimated)) +
  geom_line(aes(x= logOR, y= estimated) ,color="red", lwd=1.2) +
  geom_line(aes(x= logOR, y= true) ,color="blue", lwd=1.2) +
  
  geom_line(aes(x= logOR, y= estimated+1.96*sd_true),linetype="dashed" ,color="red", lwd=1.2) +
  geom_line(aes(x= logOR, y= estimated-1.96*sd_true),linetype="dashed" ,color="red" , lwd=1.2) +

  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.2),
        text = element_text(size=20),
        axis.title.x = element_text(size = 22)) +

  ggtitle(expression(paste("T = 5000"))) + 
  xlab(expression(paste("Log ", delta))) +
  ylab("")  +
  ylim(1.45, 2.35)


grid.arrange(p100, p1000, p5000, 
             nrow = 1)

save(Sim100, Sim1000, Sim5000, file = paste0("Sim_", t_lag, ".RData"))


# Code to generate Figure S1
set.seed(3)
n = 1
T = 200
model = "SL"
time <- rep(1:T, n)
id <- rep(1:n, rep(T, n))
x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

a_lag1 = rep(0, n * T)
a = NULL
ps = NULL
for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  ps[i] <- expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5))
  a_lag1[i+1] <- a[i]
}
ps_a = data.frame(cbind(a, ps))
colnames(ps_a) <- c("Treatment", "ps")
ps_a$Treatment[ps_a$Treatment == 1] <- "treated"
ps_a$Treatment[ps_a$Treatment == 0] <- "untreated"

q1 <- ggplot(ps_a, aes(x = ps, fill = Treatment)) +
  geom_density() +
  theme_bw() +

  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.8),
        text = element_text(size=20),
        axis.title.x = element_text(size = 20)) +

  ggtitle(expression(paste("T = 200"))) + 
  xlab("time-varying propensity scores")

n = 1
T = 1000
model = "SL"
time <- rep(1:T, n)
id <- rep(1:n, rep(T, n))
x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

a_lag1 = rep(0, n * T)
a = NULL
ps = NULL
for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  ps[i] <- expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5))
  a_lag1[i+1] <- a[i]
}
ps_a = data.frame(cbind(a, ps))
colnames(ps_a) <- c("Treatment", "ps")
ps_a$Treatment[ps_a$Treatment == 1] <- "treated"
ps_a$Treatment[ps_a$Treatment == 0] <- "untreated"

q2 <- ggplot(ps_a, aes(x = ps, fill = Treatment)) +
  geom_density() +
  theme_bw() +

  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.8),
        text = element_text(size=20),
        axis.title.x = element_text(size = 20)) +

  ggtitle(expression(paste("T = 1000"))) + 
  xlab("time-varying propensity scores")

n = 1
T = 5000
model = "SL"
time <- rep(1:T, n)
id <- rep(1:n, rep(T, n))
x.trt <- data.frame(matrix(rnorm(n * T * 5), nrow = n * T))

a_lag1 = rep(0, n * T)
a = NULL
ps = NULL
for (i in 1:(n * T)) {
  a[i] <- rbinom(1, 1, expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5)))
  ps[i] <- expit(10*(rowMeans(x.trt)[i] - a_lag1[i] + 0.5))
  a_lag1[i+1] <- a[i]
}
ps_a = data.frame(cbind(a, ps))
colnames(ps_a) <- c("Treatment", "ps")
ps_a$Treatment[ps_a$Treatment == 1] <- "treated"
ps_a$Treatment[ps_a$Treatment == 0] <- "untreated"

q3 <- ggplot(ps_a, aes(x = ps, fill = Treatment)) +
  geom_density() +
  theme_bw() +

  theme(plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = c(0.7, 0.8),
        text = element_text(size=20),
        axis.title.x = element_text(size = 20)) +

  ggtitle(expression(paste("T = 5000"))) + 
  xlab("time-varying propensity scores")


grid.arrange(q1, q2, q3, 
             nrow = 1, 
             top = textGrob(expression(paste()), gp = gpar(fontsize=28)))


