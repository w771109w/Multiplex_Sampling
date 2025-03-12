library(haven)
library(dplyr)
rm(list=ls())
dhs_data <- read_dta('/Users/weiliu/Library/CloudStorage/OneDrive-JohnsHopkins/JHU/Project/Aim 2/Spatial clustering/TZKR63FL.DTA')
library(sf)
gis_data <- st_read("/Users/weiliu/Library/CloudStorage/OneDrive-JohnsHopkins/JHU/Project/Aim 2/Spatial clustering/TZGE61FL.dbf")
head(gis_data)

setwd('/Users/weiliu/Library/CloudStorage/OneDrive-JohnsHopkins/JHU/Project/Aim 2/Spatial clustering/data') 

# tz_dhs <- dhs.data.import("TZKR63FL.DTA",
#   youngest.age = 0, oldest.age = 60, country = "TZ",
#   model.type = "all", data.year=2010)
# 
# 
# data.info <- data.frame(dbfName = "data/TZGE61FL.dbf")  # 這是你的 GPS 檔案名稱
# 
# tz_clean_data <- read.and.clean.data(
#   country = "TZ",
#   year = 2010,
#   country.dhs = "TZKR63FL.DTA",
#   country.gps = "TZGE61FL.dbf")



tz_data <- dhs.data.import(
  country = "Tanzania", 
  data.year = 2010, 
  filename = "TZKR63FL.DTA",
  filename.gps = "TZGE61FL.dbf"
)




dat.clust <- aggr.dhs.clusters(tz_data$data, vaccbreaks=seq(0,1,.1), treat_NAs="unvacc")

dat.sp.clust <- DHS.to.spatialtau.clust(dat.clust)

dat.jit <- jitter_dhs(dat.sp.clust)

dat.clust <- dat.clust |>
  rename("weight" = "sampwt")

tau.para <- calc.tau.dhs.data(dat.clust, country = "Tanzania", data.year = 2010)

dat.jit.tau <- jittered_tau_hh(dat.jit)


