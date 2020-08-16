## combining env vars and synchrony

library(reshape2)
library(tidyverse)
library(ggplot2)
library(patchwork)
install.packages("patchwork")

# upload data

# climate
# clim_tmax <- read.csv("input_data/Env/Sites_anomalies_tmax_av.csv")
# clim_tmin <- read.csv("input_data/Env/Sites_anomalies_tmin_av.csv")
# clim_tmean <- read.csv("input_data/Env/Sites_anomalies_tmean_av.csv")
# 
# save(clim_tmax, clim_tmean, clim_tmin, file="output_data/Site_anomolies_climate_av.RData")

load(file="output_data/Site_anomolies_climate_av.RData")
head(clim_tmax)
clim_tmax$env_var <- "clim_max"
clim_tmin$env_var <- "clim_min"
clim_tmean$env_var <- "clim_mean"

## melt climate data and combine

melt_clim_max <- reshape2::melt(clim_tmax, id=c("sYNGEO_ID", "env_var")) 
melt_clim_max  <- rename(melt_clim_max , year = variable,
                         anomolie = value)

melt_clim_min <- reshape2::melt(clim_tmin, id=c("sYNGEO_ID", "env_var")) 
melt_clim_min  <- rename(melt_clim_min , year = variable,
                         anomolie = value)

melt_clim_mean <- reshape2::melt(clim_tmean, id=c("sYNGEO_ID", "env_var")) 
melt_clim_mean  <- rename(melt_clim_mean , year = variable,
                          anomolie = value)

melt_clim <- rbind(melt_clim_max, melt_clim_min, melt_clim_mean)

## remove X from year column

melt_clim$year <- gsub("X", "", melt_clim$year)

## Flow

load(file="output_data/Sites_anomolies_qmin_av_flow.RData")
load(file="output_data/Sites_anomolies_qmax_av_flow.RData")
load(file="output_data/Sites_anomolies_qmean_av_flow.RData")
head(anom_qmax_av)
## add env variable name
anom_qmax_av$env_var <- "flow_max"
anom_qmin_av$env_var <- "flow_min"
anom_qmean$env_var <- "flow_mean"

melt_flow_max <- reshape2::melt(anom_qmax_av, id=c("sYNGEO_ID", "env_var")) 
melt_flow_max  <- rename(melt_flow_max , year = variable,
                         anomolie = value)

melt_flow_min <- reshape2::melt(anom_qmin_av, id=c("sYNGEO_ID", "env_var")) 
melt_flow_min  <- rename(melt_flow_min , year = variable,
                         anomolie = value)

melt_flow_mean <- reshape2::melt(anom_qmean, id=c("sYNGEO_ID", "env_var")) 
melt_flow_mean  <- rename(melt_flow_mean , year = variable,
                          anomolie = value)

melt_flow <- rbind(melt_flow_max, melt_flow_min, melt_flow_mean)

## remove X from year column

melt_flow$year <- gsub("X", "", melt_flow$year)

## synchrony scores

## basin 1
load(file="output_data/Basins/2080016510_bet_site_sync_one_out.RData")
basin$year_removed <- as.numeric(as.character(basin$year_removed))
basin$Correlation <- as.numeric(as.character(basin$Correlation))
basin <- na.omit(basin)
dim(basin)
## create sync pair code
basin <- basin %>% unite( "sync_pair", sYNGEO_ID1:sYNGEO_ID2, remove=F)
head(basin)
basinPair <- basin %>% filter(sync_pair == "242_241")
basinPair

ggplot(basinPair, aes(x =year_removed, y=Correlation)) +
  geom_line(aes( group = Axis, color = Axis)) +
  scale_y_continuous("Synchrony", limits= c(-1,1)) +
  facet_wrap(~Axis, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Between site synchrony: site 241:242 test",
       x = "Year Removed") #+ theme_bw(base_size = 15)


## check site matches
# bas_sites <- unique(basin$sYNGEO_ID1)
# length(bas_sites) ## 23
# 
# clim_sites <- unique(clim_tmax$sYNGEO_ID)
# length(clim_sites) ## 816
# 
# flow_sites <- unique(anom_qmin_av$sYNGEO_ID)
# length(flow_sites) ## 816
# 
# sum(clim_sites %in% bas_sites) ## 23

names(melt_clim)
names(basin)

## subset env to basin sites
sitespair <- c(unique(basinPair$sYNGEO_ID1), unique(basinPair$sYNGEO_ID2))
sitespair

basin_clim <- subset(melt_clim, sYNGEO_ID %in% sitespair)
head(basin_clim)
basin_clim

ggplot(basin_clim, aes(x =year, y=anomolie)) +
  geom_line(aes( group = env_var, linetype = env_var)) +
  # scale_y_continuous("Anomolie (c)", limits= c(-1,1)) +
  scale_linetype_manual(values=c( "dotted", "solid", "dotted")) +
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Climate anomolies: site 241:242 test",
       y = "Anomolie (c)",
       x = "Year") #+ theme_bw(base_size = 15)

basinPair1 <- filter(basinPair, Axis=="Axis1")
basinPair1 ## fish size

## merge by year

data <- merge(basin_clim, basinPair1,  by.x="year", by.y="year_removed")
str(data)

?sec_axis

ggplot(data, aes(x =year)) +
  geom_line(aes(y=Correlation, group = Axis, colour = Axis)) +
  geom_line(aes(y=anomolie, group = env_var, linetype = env_var )) +
  scale_linetype_manual(values=c( "dotted", "solid", "dotted")) +
  scale_y_continuous(name = "Synchrony", limits= c(-1,1), sec.axis =dup_axis(name="Anomolie (c)")) +
  # scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) +
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Between site synchrony: site 241:242 test",
       x = "Year") #+ theme_bw(base_size = 15)

### test another pair

unique(basin$sync_pair)

basinPair2 <- basin %>% filter(sync_pair == "668_241")
basinPair2

## subset env to basin sites
sitespair <- c(unique(basinPair2$sYNGEO_ID1), unique(basinPair2$sYNGEO_ID2))
sitespair

basin_clim <- subset(melt_clim, sYNGEO_ID %in% sitespair)
head(basin_clim)
basin_clim


data <- merge(basin_clim, basinPair2,  by.x="year", by.y="year_removed")
str(data)
data

?sec_axis

ggplot(data, aes(x =year)) +
  geom_line(aes(y=Correlation, group = Axis, colour = Axis)) +
  geom_line(aes(y=anomolie, group = env_var, linetype = env_var )) +
  scale_linetype_manual(values=c( "dotted", "solid", "dotted")) +
  scale_y_continuous(name = "Synchrony", limits= c(-1,1), sec.axis =dup_axis(name="Anomolie (c)")) +
  # scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) +
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Between site synchrony: site 668:242 test",
       x = "Year") #+ theme_bw(base_size = 15)

