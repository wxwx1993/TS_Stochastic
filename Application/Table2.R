# Code to generate Table 2
library(dplyr)

# Table 2:  characteristics for NWS-issued heat alerts, all-cause deaths among Medicare enrollees, 
# and cause-specific hospitalizations for five heat-related diseases among Medicare FFS enrollees across April-October of 2006-2016
test_alert <- daily_fips_main %>% 
  group_by(fips, year) %>%
  summarise(alert_mean = mean(alert))

test <- daily_fips_main %>% 
  group_by(fips, alert) %>%
  summarise(HI_mean = mean(HImaxF_PopW))

hospital_disease <- daily_fips_main %>% 
  group_by(fips, alert) %>%
  summarise_at(17:23, mean)

mean(as.data.frame(subset(hospital_disease, alert == 0))[,3])
mean(as.data.frame(subset(hospital_disease, alert == 1))[,3])

mean(as.data.frame(subset(hospital_disease, alert == 0))[,4])
mean(as.data.frame(subset(hospital_disease, alert == 1))[,4])

mean(as.data.frame(subset(hospital_disease, alert == 0))[,5])
mean(as.data.frame(subset(hospital_disease, alert == 1))[,5])

mean(as.data.frame(subset(hospital_disease, alert == 0))[,6])
mean(as.data.frame(subset(hospital_disease, alert == 1))[,6])

mean(as.data.frame(subset(hospital_disease, alert == 0))[,7])
mean(as.data.frame(subset(hospital_disease, alert == 1))[,7])

mean(as.data.frame(subset(hospital_disease, alert == 0))[,8])
mean(as.data.frame(subset(hospital_disease, alert == 1))[,8])

mean(as.data.frame(subset(hospital_disease, alert == 0))[,9])
mean(as.data.frame(subset(hospital_disease, alert == 1))[,9])

