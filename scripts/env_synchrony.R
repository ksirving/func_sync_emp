## calulate synchriny between flow and temp anomolies ~ raw values
library(ncdf4); library(sp); library(raster); library(tidyverse); library(tidyr)
## work flow
# 1) get raw value and anaomlies
# 2) match to new fish sites
# 3) subset to France bio sites
# 4) synchrony between raw and anomlie values

## upload sites 
sites = read.csv("/Users/katieirving/Documents/sYNGEO/func_emp/data/Data_10262020/fishdata_selection_basins_same_time_window_10262020.csv",h=T)
dim(sites)

## subset to France
sites <- sites %>%
  dplyr::filter(Country == "FRA")

## make spatial
coordinates(sites)<- c("Longitude","Latitude")
head(sites)
unique(sites$SiteID)
## upload env data

# anomolies

load(file="output_data/clim_data_melt.RData") ## melt_clim
str(melt_clim)
unique(melt_clim$sYNGEO_ID)
head(melt_clim)
load(file="output_data/flow_data_melt.RData") ## melt_flow

## extract env data at france sites

ClimAnomFrance <- raster::extract(melt_clim, sites)

## raw values

## climate

clim_tmean <- read.csv("input_data/Env/Sites_tmean_av.csv")
clim_tmin <- read.csv("input_data/Env/Sites_tmin_av.csv")
clim_tmax <- read.csv("input_data/Env/Sites_tmax_av.csv")
  
## flow
  
load(file="output_data/flow_min_max_values_extracted.RData")
  # str(qmin_av)
qmin_av <- as.data.frame(qmin_av)
qmax_av <- as.data.frame(qmax_av)
colnames(qmin_av) = 2003:2014
colnames(qmax_av) = 2003:2014
head(qmax_av)

qmin_av = data.frame(sYNGEO_ID = sites$SiteID,qmin_av)
qmax_av = data.frame(sYNGEO_ID = sites$SiteID,qmax_av)
qmean = (qmin_av[,2:12] + qmax_av[,2:12])/2
qmean = data.frame(SiteID = sites$SiteID,qmean)
dim(qmin_av)
