# Code to generate Figure 3
library(tidyverse)
library(sp)
library("raster")
library("dplyr")
library("sf")
library("stringr")
library("ggplot2")

us <- map_data('state')
states <- st_read("/nfs/home/X/xwu/shared_space/ci3_xwu/ts_ips/cb_2017_us_county_500k/cb_2017_us_county_500k.shp")

aggregate_fips_heat <- daily_fips_heat %>%
  filter(alert == 1) %>%
  group_by(fips) %>%
  summarise(min_HI = min(HImaxF_PopW, na.rm = TRUE),
            mean_HI = mean(HImaxF_PopW, na.rm = TRUE),
            max_HI = max(HImaxF_PopW, na.rm = TRUE),
            num_alert = sum(alert, na.rm = TRUE))
  
heat_us <- mutate(aggregate_fips_heat,
             STATEFP = str_sub(fips, 1, 2),
             COUNTYFP = str_sub(fips, 3, 5))
str(heat_us)
str(states)
states$STATEFP <- as.character(states$STATEFP)
states$COUNTYFP <- as.character(states$COUNTYFP)
states_heat <- left_join(states, heat_us, by = c("STATEFP", "COUNTYFP"))

g1 <- ggplot(states_heat) +
  xlim(-125, -65) +
  ylim(25,50) +
  geom_sf(aes(fill = min_HI), color='grey', size = 0.005) +
  scale_fill_gradient2(expression(paste("Temperature (°F)")), low = "#1e90ff", mid = "#ffffba", high = "#8b0000", midpoint = 80,
                       breaks = c(60, 70, 80, 90, 100), limits = c(55, 110), na.value = "grey") +
  labs(title = expression(paste("Threshold Heat Index of Heat Alert Days in All Counties 2006-2016"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30*2,hjust = 0.5, vjust = -3),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60, size = 20 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(150 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

jpeg("allcounty_alert_heat_min.jpeg", height = 1024 * 0.6 * 2 * 5, width = 1024 * 2 * 5, res = 72 * 5)
g1
dev.off()

states_heat[which(states_heat$num_alert > 200), ]$num_alert = 200
g2 <- ggplot(states_heat) +
  xlim(-125, -65) +
  ylim(25, 50) +
  geom_sf(aes(fill = num_alert), color = 'grey', size = 0.005) +
  scale_fill_gradient2(expression(paste("Number of Heat Alert")), low  = "#ffffba", mid = "#ff9d00", high = "#8b0000", midpoint = 100,
                       breaks = c(50, 100, 150, 200), limits = c(0, 200), na.value = "grey") +
  labs(title = expression(paste("Number of Heat Alert Days in All Counties 2006-2016"))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30 * 2, hjust = 0.5, vjust = -3),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 20 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 18 * 2),
        legend.key.width = unit(150 * 2, "points"),
        panel.grid.major = element_line(colour = "transparent"))

jpeg("allcounty_alert_num.jpeg", height = 1024 * 0.6 * 2 * 5, width = 1024 * 2 * 5, res = 72 * 5)
g2
dev.off()
