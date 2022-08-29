# Code to generate Figure 3
library(spdep)
library(USAboundaries)
library(coda)
library(tidyverse)
library(sp)
library("raster")
library("dplyr")
library("sf")
library("stringr")
library("ggplot2")
library(parallel)
library("grid")
library("gridExtra")
library(meta)
library(metafor)
library(rgeos)
library(ape)

Dir = "~/Dropbox/Incremental_TS/Data/result_hosp/"
heat_us <- readRDS("heat_us.rds")

# Point-wise + Time-Uniform
t_lag = 2
for (i in c("deaths", 18:24)) {
  T = 214
  load(paste0(Dir, "meta_result/", i, "_", t_lag, "_meta_sp.RData"))
  name_disease = "Deaths"
  #  pdf(file = paste0(Dir, "meta_result/", i, "_", t_lag, "_", name_disease,".pdf"), height = 8.5, width = 14)
  pw_sd = c(sqrt(((Sim_sp$estimated- Sim_sp$lower)/1.96)^2 + ((Sim_sp$estimated[25] - Sim_sp$lower[25])/1.96)^2), 
            sqrt(((Sim_sp$upper - Sim_sp$estimated)/1.96)^2 + ((Sim_sp$upper[25] - Sim_sp$estimated[25])/1.96)^2))
  alpha <- 0.05
  rho <- sqrt((-alpha^2 - 2*log(alpha)+log(-2*log(alpha)+1-alpha^2))/(T-t_lag))
  Vt = pw_sd^2*(T-t_lag)*11
  tu_cs = sqrt((2*(T-t_lag)*11*Vt*rho^2+1)/(((T-t_lag)*11)^2*rho^2)*log(sqrt((T-t_lag)*11*Vt*rho^2+1)/alpha))
  
  Sim2 = data.frame(logOR = Sim_sp$logOR,
                    estimated = c(Sim_sp$estimated - Sim_sp$estimated[25],
                                  Sim_sp$estimated - Sim_sp$estimated[25]), 
                    lower = c(Sim_sp$estimated - Sim_sp$estimated[25] - tu_cs[1:50],
                              Sim_sp$estimated - Sim_sp$estimated[25] - 1.96*pw_sd[1:50]),
                    upper = c(Sim_sp$estimated - Sim_sp$estimated[25] + tu_cs[51:100],
                              Sim_sp$estimated - Sim_sp$estimated[25] + 1.96*pw_sd[51:100]),
                    Methods = c(rep("Time-uniform CSs", 50), rep("Point-wise CIs", 50)))
  
  c =   ggplot(data = Sim2[c(25:50, 75:100),], aes(x = logOR, y = estimated, color = Methods)) +
    geom_ribbon(data = Sim2[25:50,], aes(ymin = lower, ymax = upper), fill = "grey70", alpha=0.3) +
    geom_ribbon(data = Sim2[75:100,], aes(ymin = lower, ymax = upper), fill = "grey70", alpha=0.7) +
    geom_line(aes(x= logOR, y= estimated), lwd=1.2, color = "red") +
    geom_line(aes(x= logOR, y= lower), linetype="dashed" , lwd=1.2) +
    geom_line(aes(x= logOR, y= upper), linetype="dashed" , lwd=1.2) +
    geom_line(data = data.frame(x = c(Sim2$logOR[100], Sim2$logOR[100]), y = c(Sim2$lower[100], Sim2$upper[100]), Methods = NA), aes(x = x , y = y), lwd=1.2, color = "black") +
    ylim(1.5*min(Sim2$lower) - 0.5*max(Sim2$upper), 1.5*max(Sim2$upper)- 0.5*min(Sim2$lower)) +
    geom_text(x = Sim2$logOR[1], y = 1.5*max(Sim2$upper)- 0.5*min(Sim2$lower), label="a", size = 16, color = "black") + 
    scale_color_manual(values=c("red", "blue")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 32),
          legend.position = c(0.2, 0.2),
          text = element_text(size=28),
          axis.title.x = element_text(size = 28),
          axis.title.y = element_text(size = 28)) +
    ggtitle(bquote("Diff in"~ .(name_disease)~"as"~delta~"varies vs."~ delta~" = 1")) + 
    xlab(expression(paste("Log ", delta))) +
    ylab("Counts per day per county") 
  pdf(file = paste0(Dir, "meta_result/", i, "_", t_lag, "_", name_disease,"_diff_compare_tu.pdf"), height = 8.5, width = 14)                  
  print(c)
  dev.off()
}
