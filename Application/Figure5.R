library("ggplot2")
library("grid")
library("gridExtra")

Dir <- "~/Dropbox/Incremental_TS/Data/result_hosp_all/"
t_lag <- 2

k <- 1
for (i in c(17:21)) {
  load(paste0(Dir, "meta_result/", i, "_", t_lag, "_meta.RData"))
  letter <- c("f","b","c","d","e")[k]
  Sim2 = data.frame(logOR = Sim$logOR,
                    estimated = Sim$estimated - Sim$estimated[25], 
                    lower = Sim$estimated - Sim$estimated[25] - sqrt(((Sim$estimated- Sim$lower) / 1.96)^2 + ((Sim$estimated[25] - Sim$lower[25]) / 1.96)^2),
                    upper = Sim$estimated - Sim$estimated[25] + sqrt(((Sim$upper - Sim$estimated) / 1.96)^2 + ((Sim$upper[25] - Sim$estimated[25]) / 1.96)^2))
  pdf(file = paste0(Dir, "meta_result/", i, "_", t_lag, "_", name_disease,"_diff.pdf"), height = 8.5, width = 14)                  
  a = ggplot(data = Sim2, aes(x = logOR, y = estimated)) +
    geom_ribbon(data = Sim2[25:50, ], aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.7) +
    geom_line(aes(x = logOR, y = estimated), color = "red", lwd = 1.2) +
    geom_line(aes(x = logOR, y = lower), linetype = "dashed", color = "red", lwd = 1.2) +
    geom_line(aes(x = logOR, y = upper), linetype = "dashed", color = "red", lwd = 1.2) +
    geom_line(data = data.frame(x = c(Sim2$logOR[50], Sim2$logOR[50]), y = c(Sim2$lower[50], Sim2$upper[50])), aes(x = x, y = y), lwd = 1.2) +
    ylim(1.5 * min(Sim2$lower) - 0.5 * max(Sim2$upper), 1.5 * max(Sim2$upper)- 0.5 * min(Sim2$lower)) +
    geom_text(x = Sim2$logOR[1], y = 1.5 * max(Sim2$upper) - 0.5 * min(Sim2$lower), label = letter, size = 16) + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 32),
          legend.position = c(0.7, 0.2),
          text = element_text(size = 28),
          axis.title.x = element_text(size = 28),
          axis.title.y = element_text(size = 28)) +
    ggtitle(bquote("Diff in"~ .(name_disease)~"hosps as"~delta~"varies vs."~ delta~" = 0")) + 
    xlab(expression(paste("Log odds ratio ", delta))) +
    ylab("Counts per day per county") 
  print(a)
  dev.off()
}

i <- "deaths"
load(paste0(Dir, "meta_result/", i, "_", t_lag, "_meta.RData"))
name_disease <- "Deaths"
Sim2 = data.frame(logOR = Sim$logOR,
                  estimated = Sim$estimated - Sim$estimated[25], 
                  lower = Sim$estimated - Sim$estimated[25] - sqrt(((Sim$estimated - Sim$lower) / 1.96)^2 + ((Sim$estimated[25] - Sim$lower[25]) / 1.96)^2),
                  upper = Sim$estimated - Sim$estimated[25] + sqrt(((Sim$upper - Sim$estimated) / 1.96)^2 + ((Sim$upper[25] - Sim$estimated[25]) / 1.96)^2))

pdf(file = paste0(Dir, "meta_result/", i, "_", t_lag, "_", name_disease, "_diff.pdf"), height = 8.5, width = 14)                  
a <- ggplot(data = Sim2, aes(x = logOR, y = estimated)) +
  geom_ribbon(data = Sim2[25:50, ], aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.7) +
  geom_line(aes(x = logOR, y = estimated), color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = lower), linetype = "dashed", color = "red", lwd = 1.2) +
  geom_line(aes(x = logOR, y = upper), linetype = "dashed", color = "red", lwd = 1.2) +
  geom_line(data = data.frame(x = c(Sim2$logOR[50], Sim2$logOR[50]), y = c(Sim2$lower[50], Sim2$upper[50])), aes(x = x , y = y), lwd = 1.2) +
  ylim(1.5 * min(Sim2$lower) - 0.5 * max(Sim2$upper), 1.5 * max(Sim2$upper) - 0.5 * min(Sim2$lower)) +
  geom_text(x = Sim2$logOR[1], y = 1.5 * max(Sim2$upper) - 0.5 * min(Sim2$lower), label = "a", size = 16) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 32),
        legend.position = c(0.7, 0.2),
        text = element_text(size = 28),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28)) +
  ggtitle(bquote("Diff in"~ .(name_disease)~"as"~delta~"varies vs."~ delta~" = 0")) + 
  xlab(expression(paste("Log ", delta))) +
  ylab("Counts per day per county") 
print(a)
dev.off()
