# Code to conduct spatial random-effect meta analysis to data applications.
library(parallel)
library(USAboundaries)
library(meta)
library(metafor)
library(dplyr)
library(sp)
library("raster")
library("sf")
library(rgeos)

Dir = "~/Dropbox/Incremental_TS/Data/result_hosp_all/"
t_lag <- 2
heat_us <- readRDS("heat_us.rds")

i = "deaths"
load(paste0(Dir,i,"_t",t_lag, "_SL.RData"))
mean.est.eff <- do.call("rbind",lapply(est.eff, `[[`, 1))
mean.var1 <- do.call("rbind",lapply(est.eff, `[[`, 2))
sum.var1 <- do.call("rbind",lapply(est.eff, `[[`, 3))
T = 214
N = length(est.eff)
sd_bound <- 1/((T-t_lag)*11)*sqrt(sum.var1)
states <- st_read("cb_2013_us_county_500k/cb_2013_us_county_500k.shp")


curves_sp = as.data.frame(t(mcmapply(function(delta) {
  madata = as.data.frame(cbind(TE = log(mean.est.eff[, delta]),
                               seTE = sd_bound[, delta]/(mean.est.eff[, delta]),
                               fips = heat_us$fips),
                         stringsAsFactors = FALSE)
  madata_geo <- right_join(states, madata, by = c("GEOID" = "fips"))
  madata_geo$TE <- as.numeric(madata_geo$TE)
  madata_geo$seTE <- as.numeric(madata_geo$seTE)
  
  madata_geo[, c("longitude","latitude")] <- as.data.frame(st_coordinates(st_centroid(st_geometry(madata_geo))))
  madata_geo$outer <- 1
  
  m_sp <- rma.mv(yi = as.numeric(TE),
                V = as.numeric(seTE)^2,
                random = ~ longitude + latitude|outer,
                struct="SPGAU",
                data = madata_geo)
  
  return(c(m_sp$beta, m_sp$ci.lb, m_sp$ci.ub))
  
}, 1:50, mc.cores = 10)))
delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))

Sim_sp = data.frame(logOR = log(delta.seq),
                 estimated = exp(curves_sp$V1), 
                 lower = exp(curves_sp$V2),
                 upper = exp(curves_sp$V3))
name_disease = "Deaths"
save(curves_sp, Sim_sp, name_disease, file = paste0(Dir, "meta_result/", i, "_", t_lag, "_meta_sp.RData"))

# Hospitalization
for (i in 18:24) {
  t_lag = 2
  load(paste0(Dir,i,"_t",t_lag, "_SL.RData"))
  mean.est.eff <- do.call("rbind",lapply(est.eff, `[[`, 1))
  mean.var1 <- do.call("rbind",lapply(est.eff, `[[`, 2))
  sum.var1 <- do.call("rbind",lapply(est.eff, `[[`, 3))
  T = 214
  N = length(est.eff)

  sd_bound <- 1/((T-t_lag)*11)*sqrt(sum.var1)
  
  curves_sp = as.data.frame(t(mcmapply(function(delta) {
    madata = as.data.frame(cbind(TE = log(mean.est.eff[, delta]),
                                 seTE = sd_bound[, delta]/(mean.est.eff[, delta]),
                                 fips = heat_us$fips),
                           stringsAsFactors = FALSE)
    madata_geo <- right_join(states, madata, by = c("GEOID" = "fips"))
    madata_geo$TE <- as.numeric(madata_geo$TE)
    madata_geo$seTE <- as.numeric(madata_geo$seTE)
    
    madata_geo[, c("longitude","latitude")] <- as.data.frame(st_coordinates(st_centroid(st_geometry(madata_geo))))
    madata_geo$outer <- 1
    
    m_sp <- rma.mv(yi = as.numeric(TE),
                   V = as.numeric(seTE)^2,
                   random = ~ longitude + latitude|outer,
                   struct="SPGAU",
                   data = madata_geo)
    return(c(m_sp$beta, m_sp$ci.lb, m_sp$ci.ub))
    
  }, 1:50, mc.cores = 10)))
  delta.seq <- exp(seq(-2.3, 2.3, length.out = 50))
  
  Sim_sp = data.frame(logOR = log(delta.seq),
                      estimated = exp(curves_sp$V1), 
                      lower = exp(curves_sp$V2),
                      upper = exp(curves_sp$V3))
  save(curves_sp, Sim_sp, name_disease, file = paste0(Dir, "meta_result/", i, "_", t_lag, "_meta_sp.RData"))
}
