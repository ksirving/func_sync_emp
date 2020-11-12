## calulate synchriny between flow and temp anomolies ~ raw values
library(ncdf4); library(sp); library(raster); library(tidyverse); library(tidyr)
## work flow
# 1) get raw value and anaomlies
# 2) match to new fish sites
# 3) subset to France bio sites
# 4) synchrony between raw and anomlie values



# anomolies

# climate
# clim_tmax <- read.csv("output_data/Sites_anomalies_tmax_av_new_sites.csv")
# clim_tmin <- read.csv("output_data/Sites_anomalies_tmin_av_new_sites.csv")
# clim_tmean <- read.csv("output_data/Sites_anomalies_tmean_av_new_sites.csv")
# 
# save(clim_tmax, clim_tmean, clim_tmin, file="output_data/Site_anomolies_climate_av_new_sites.RData")

load(file="output_data/Site_anomolies_climate_av_new_sites.RData")
head(clim_tmax)
clim_tmax$env_var <- "clim_max_anom"
clim_tmin$env_var <- "clim_min_anom"
clim_tmean$env_var <- "clim_mean_anom"

### flow

# load(file="output_data/Sites_anomolies_qmin_av_flow_new_sites.RData")
# load(file="output_data/Sites_anomolies_qmax_av_flow_new_sites.RData")
# load(file="output_data/Sites_anomolies_qmean_av_flow_new_sites.RData")
# 
# save(anom_qmin_av, anom_qmax_av,anom_qmean, file="output_data/flow_anomolies_min_max_mean_extracted_new_sites.RData")

load(file="output_data/flow_anomolies_min_max_mean_extracted_new_sites.RData")
anom_qmin_av$env_var <- "qmin_anom"
anom_qmax_av$env_var <- "qmax_anom"
anom_qmean$env_var <- "qmean_anom"

# Raw values --------------------------------------------------------------


## climate

clim_tmean_raw <- read.csv("output_data/Sites_tmean_av_new_sites.csv")
clim_tmin_raw <- read.csv("output_data/Sites_tmin_av_new_sites.csv")
clim_tmax_raw <- read.csv("output_data/Sites_tmax_av_new_sites.csv")

clim_tmax_raw$env_var <- "clim_max_raw"
clim_tmin_raw$env_var <- "clim_min_raw"
clim_tmean_raw$env_var <- "clim_mean_raw"

## flow
  
load("output_data/flow_min_max_mean_extracted_new_sites_raw.RData")
qmin_av$env_var <- "qmin_raw"
qmax_av$env_var <- "qmax_raw"
qmean$env_var <- "qmean_raw"



# Format data and combine -------------------------------------------------


## format data
## melt climate data anomolies and combine

melt_clim_max <- reshape2::melt(clim_tmax, id=c("SiteID", "env_var")) 
melt_clim_max  <- rename(melt_clim_max , year = variable,
                         anomolie = value)

melt_clim_min <- reshape2::melt(clim_tmin, id=c("SiteID", "env_var")) 
melt_clim_min  <- rename(melt_clim_min , year = variable,
                         anomolie = value)

melt_clim_mean <- reshape2::melt(clim_tmean, id=c("SiteID", "env_var")) 
melt_clim_mean  <- rename(melt_clim_mean , year = variable,
                          anomolie = value)

melt_clim_anom <- rbind(melt_clim_max, melt_clim_min, melt_clim_mean)
head(melt_clim_anom)

## remove X from year column

melt_clim_anom$year <- gsub("X", "", melt_clim_anom$year)
save(melt_clim_anom, file="output_data/clim_data_melt_anomolies_new_sites.RData")


## melt climate data raw and combine
melt_clim_max_raw <- reshape2::melt(clim_tmax_raw, id=c("SiteID", "env_var")) 
melt_clim_max_raw  <- rename(melt_clim_max_raw , year = variable,
                             Temp = value)

melt_clim_min_raw <- reshape2::melt(clim_tmin_raw, id=c("SiteID", "env_var")) 
melt_clim_min_raw  <- rename(melt_clim_min_raw , year = variable,
                             Temp = value)

melt_clim_mean_raw <- reshape2::melt(clim_tmean_raw, id=c("SiteID", "env_var")) 
melt_clim_mean_raw  <- rename(melt_clim_mean_raw , year = variable,
                          Temp = value)

melt_clim_raw <- rbind(melt_clim_max_raw, melt_clim_min_raw, melt_clim_mean_raw)
head(melt_clim_raw)

## remove X from year column

melt_clim_raw$year <- gsub("X", "", melt_clim_raw$year)
save(melt_clim_raw, file="output_data/clim_data_melt_raw_new_sites.RData")

### # melt flow data anomolies and combine

melt_anom_qmin_av <- reshape2::melt(anom_qmin_av, id=c("SiteID", "env_var")) 
melt_anom_qmin_av  <- rename(melt_anom_qmin_av , year = variable,
                         anomolie = value)

melt_anom_qmax_av <- reshape2::melt(anom_qmax_av, id=c("SiteID", "env_var")) 
melt_anom_qmax_av  <- rename(melt_anom_qmax_av , year = variable,
                         anomolie = value)

melt_anom_qmean <- reshape2::melt(anom_qmean, id=c("SiteID", "env_var")) 
melt_anom_qmean  <- rename(melt_anom_qmean , year = variable,
                          anomolie = value)

melt_flow_anom <- rbind(melt_anom_qmin_av, melt_anom_qmax_av, melt_anom_qmean)
head(melt_flow_anom)

## remove X from year column

melt_flow_anom$year <- gsub("X", "", melt_flow_anom$year)
save(melt_flow_anom, file="output_data/flow_data_melt_anomolies_new_sites.RData")


## melt flow data raw and combine
melt_qmin_av_raw <- reshape2::melt(qmin_av, id=c("SiteID", "env_var")) 
melt_qmin_av_raw  <- rename(melt_qmin_av_raw , year = variable,
                             Flow = value)

melt_qmax_av_raw <- reshape2::melt(qmax_av, id=c("SiteID", "env_var")) 
melt_qmax_av_raw  <- rename(melt_qmax_av_raw , year = variable,
                            Flow = value)

melt_qmean_raw <- reshape2::melt(qmean, id=c("SiteID", "env_var")) 
melt_qmean_raw  <- rename(melt_qmean_raw , year = variable,
                          Flow = value)

melt_flow_raw <- rbind(melt_qmean_raw, melt_qmax_av_raw, melt_qmin_av_raw)
head(melt_flow_raw)

## remove X from year column

melt_flow_raw$year <- gsub("X", "", melt_flow_raw$year)
save(melt_flow_raw, file="output_data/flow_data_melt_raw_new_sites.RData")


# Check data --------------------------------------------------------------

head(melt_clim_raw)
unique(melt_clim_raw$env_var)

head(melt_clim_anom)
unique(melt_clim_anom$env_var)


head(melt_flow_raw)
unique(melt_flow_raw$env_var)

head(melt_flow_anom)
unique(melt_flow_anom$env_var)

# ## combine
# melt_clim <- rbind(melt_clim_raw, melt_clim_anom)

# Fish data sites ---------------------------------------------------------

## upload sites 
sites = read.csv("/Users/katieirving/Documents/sYNGEO/func_emp/data/Data_10262020/fishdata_selection_basins_same_time_window_10262020.csv",h=T)
dim(sites)

## subset to France
FranceSites <- sites %>%
  dplyr::filter(Country == "FRA")

# ## make spatial
# coordinates(sites)<- c("Longitude","Latitude")
# head(sites)

FranceSites <- unique(FranceSites$SiteID)

FranceSites ## 96

## subset env data

clim_raw_france <- subset(melt_clim_raw, SiteID %in% FranceSites)
clim_anom_france <- subset(melt_clim_anom, SiteID %in% FranceSites)

flow_raw_france <- subset(melt_flow_raw, SiteID %in% FranceSites)
flow_anom_france <- subset(melt_flow_anom, SiteID %in% FranceSites)
head(clim_raw_france)

# Correlation in raw v anomolies ------------------------------------------

## Climate
s=1
e=2
s

df <- NULL


for(s in 1:length(FranceSites)) {
  
  site_raw <- clim_raw_france %>%
    filter(SiteID == paste(FranceSites[s]))
  
  site_anom <- clim_anom_france %>%
    filter(SiteID == paste(FranceSites[s]))

  raw_env_var <- sort(unique(site_raw$env_var))
  anom_env_var <- sort(unique(site_anom$env_var))
  
  for(e in 1:length(raw_env_var)) {

    env_var_raw <- site_raw %>%
      filter(env_var == paste(raw_env_var[e]))
    
    env_var_anom <- site_anom %>%
      filter(env_var == paste(anom_env_var[e]))

    RawVals <- env_var_raw$Temp
    AnomVals <- env_var_anom$anomolie
    
    dfx <- data.frame(matrix(nrow=3, ncol=4))
    
    dfx[e, 1] <- FranceSites[s]
    dfx[e, 2] <- raw_env_var[e]
    dfx[e, 3] <- anom_env_var[e]
    dfx[e, 4] <- cor(RawVals, AnomVals)
    
    dfx <- na.omit(dfx)
    df <- rbind(df, dfx)
  }
  


}
df <- na.omit(df)
colnames(df) <- c("SiteID", "Raw_Env_Var", "Anom_Env_Var", "Cor_Val")
df

write.csv(df, "output_data/cor_clim_raw_v_anom.csv")

## Flow
s=1
e=2
s

df <- NULL


for(s in 1:length(FranceSites)) {
  
  site_raw <- flow_raw_france %>%
    filter(SiteID == paste(FranceSites[s]))
  
  site_anom <- flow_anom_france %>%
    filter(SiteID == paste(FranceSites[s]))
  
  raw_env_var <- sort(unique(site_raw$env_var))
  anom_env_var <- sort(unique(site_anom$env_var))

  for(e in 1:length(raw_env_var)) {
    
    env_var_raw <- site_raw %>%
      filter(env_var == paste(raw_env_var[e]))
    
    env_var_anom <- site_anom %>%
      filter(env_var == paste(anom_env_var[e]))
    
    RawVals <- env_var_raw$Flow
    AnomVals <- env_var_anom$anomolie
    
    dfx <- data.frame(matrix(nrow=3, ncol=4))
    
    dfx[e, 1] <- FranceSites[s]
    dfx[e, 2] <- raw_env_var[e]
    dfx[e, 3] <- anom_env_var[e]
    dfx[e, 4] <- cor(RawVals, AnomVals)
    
    dfx <- na.omit(dfx)
    df <- rbind(df, dfx)
  }
  
  
  
}

colnames(df) <- c("SiteID", "Raw_Env_Var", "Anom_Env_Var", "Cor_Val")
df

write.csv(df, "output_data/cor_flow_raw_v_anom.csv")

