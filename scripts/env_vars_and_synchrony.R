## combining env vars and synchrony

library(reshape2)
library(tidyverse)
library(ggplot2)
library(patchwork)
# install.packages("patchwork")


# upload data -------------------------------------------------------------

climate
clim_tmax <- read.csv("input_data/Env/Sites_anomalies_tmax_av_.csv")
clim_tmin <- read.csv("input_data/Env/Sites_anomalies_tmin_av.csv")
clim_tmean <- read.csv("input_data/Env/Sites_anomalies_tmean_av.csv")

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
save(melt_clim, file="output_data/clim_data_melt.RData")

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
save(melt_flow, file="output_data/flow_data_melt.RData")


# synchrony scores --------------------------------------------------------


## basin 1
load(file="output_data/Basins/2080016510_bet_site_sync_one_out.RData")
## France
basin$year_removed <- as.numeric(as.character(basin$year_removed))
basin$Correlation <- as.numeric(as.character(basin$Correlation))
basin <- na.omit(basin)
dim(basin) # 5520    6

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


# Climate and Flow figures  ----------------------------------------------------------------
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
  scale_linetype_manual(breaks = c("clim_max", "clim_mean", "clim_min"),
                        values=c( "dotted", "solid", "dotted"),
                        labels = c("Max Temperature", "Mean Temperature", "Min Temperature")) +
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Climate anomolies: site 241:242 test",
       y = "Anomolie (c)",
       x = "Year") #+ theme_bw(base_size = 15)

# basinPair1 <- filter(basinPair, Axis=="Axis1")
# basinPair1 ## fish size

## merge by year

data <- merge(basin_clim, basinPair,  by.x="year", by.y="year_removed")
str(data)
head(data)
unique(data$env_var)

## get overall synchrony value - basin 2080016510, synch pair 241:242
synchrony_axis <- read.csv("output_data/results_between_site_synchrony.csv")
head(synchrony_axis)

## create sync pair code
synchrony_axis <- synchrony_axis %>% unite( "sync_pair", sYNGEO_ID1:sYNGEO_ID2, remove=F)
unique(synchrony_axis$Axis)
france <- synchrony_axis %>% filter(basin_ID == "2080016510")
unique(france$sync_pair)
pair <- france %>% filter(sync_pair == "242_241")
pair
head(synchrony_axis)


ggplot(data, aes(x =year)) +
  geom_line(aes(y=Correlation, group = Axis, colour = Axis)) +
  geom_line(aes(y=anomolie, group = env_var, linetype = env_var )) +
  geom_hline(yintercept=pair$Correlation, linetype="dashed",colour="grey")+
  scale_color_brewer(breaks = c("Axis1", "Axis2"),
                     palette="Set1",
                     labels = c("Fish Size", "Fish Reproductive Type"),
                     name = "Functional Trait") +
  scale_linetype_manual(breaks = c("clim_max", "clim_mean", "clim_min"),
                        values=c( "dotted", "solid", "dotted"),
                        labels = c("Max Temperature", "Mean Temperature", "Min Temperature"), 
                        name = "Environmental Variable") +
  
  scale_y_continuous(name = "Synchrony", limits= c(-1,1), sec.axis =dup_axis(name="Anomolie (c)")) +
  # scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) +
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Between site synchrony: site 241:242 test",
       x = "Year") #+ theme_bw(base_size = 15)

### test another pair

unique(basin$sync_pair)

pair <- france %>% filter(sync_pair == "668_241")
pair

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


ggplot(data, aes(x =year)) +
  geom_line(aes(y=Correlation, group = Axis, colour = Axis)) +
  geom_line(aes(y=anomolie, group = env_var, linetype = env_var )) +
  geom_hline(yintercept=pair$Correlation, linetype="dashed",colour="grey")+
  scale_color_brewer(breaks = c("Axis1", "Axis2"),
                     palette="Set1",
                     labels = c("Fish Size", "Fish Reproductive Type"),
                     name = "Functional Trait") +
  scale_linetype_manual(breaks = c("clim_max", "clim_mean", "clim_min"),
                        values=c( "dotted", "solid", "dotted"),
                        labels = c("Max Temperature", "Mean Temperature", "Min Temperature"), 
                        name = "Environmental Variable") +
  scale_y_continuous(name = "Synchrony", limits= c(-1,1), sec.axis =dup_axis(name="Anomolie (c)")) +
  # scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) +
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Between site synchrony: site 668:241 test",
       x = "Year") #+ theme_bw(base_size = 15)


### flow 

## subset env to basin sites
sitespair <- c(unique(basinPair$sYNGEO_ID1), unique(basinPair$sYNGEO_ID2))
sitespair

basin_flow <- subset(melt_flow, sYNGEO_ID %in% sitespair)
head(basin_flow)
unique(basin_flow$env_var)

ggplot(basin_flow, aes(x =year, y=anomolie)) +
  geom_line(aes( group = env_var, linetype = env_var)) +
  # scale_y_continuous("Anomolie (c)", limits= c(-1,1)) +
  scale_linetype_manual(breaks = c("flow_max", "flow_mean", "flow_min"),
                        values=c( "dotted", "solid", "dashed"),
                        labels = c("Max Discharge", "Mean Discharge", "Min Discharge")) +
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Flow anomolies: site 241:242 test",
       y = "Anomolie (m/s)",
       x = "Year") #+ theme_bw(base_size = 15)

# basinPair1 <- filter(basinPair, Axis=="Axis1")
# basinPair1 ## fish size

## merge by year

data <- merge(basin_flow, basinPair,  by.x="year", by.y="year_removed")
str(data)
head(data)
unique(data$env_var)
pair <- france %>% filter(sync_pair == "242_241")
pair

ylim.prim <- c(-1, 1)   # in this example, synchrony
ylim.sec <- c(-100, 100)    # in this example, anomolie

b <- diff(ylim.prim)/diff(ylim.sec)
b

ggplot(data, aes(x =year)) +
  geom_line(aes(y=Correlation, group = Axis, colour = Axis)) +
  geom_line(aes(y=anomolie*b, group = env_var, linetype = env_var )) +
  geom_hline(yintercept=pair$Correlation, linetype="dashed",colour="grey")+
  scale_color_brewer(breaks = c("Axis1", "Axis2"),
                     palette="Set1",
                     labels = c("Fish Size", "Fish Reproductive Type"),
                     name = "Functional Trait") +
  scale_linetype_manual(breaks = c("flow_max", "flow_mean", "flow_min"),
                        values=c( "dotted", "solid", "dashed"),
                        labels = c("Max Discharge", "Mean Discharge", "Min Discharge"), 
                        name = "Environmental Variable") +
  scale_y_continuous(name = "Synchrony", limits= c(-1,1), sec.axis = sec_axis(~. /b, name = "Anomolie (m/s)")) +
  # scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) (m/s)
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Between site synchrony: site 241:242 test",
       x = "Year") #+ theme_bw(base_size = 15)


### test another pair

unique(basin$sync_pair)
pair <- france %>% filter(sync_pair == "668_241")
pair

basinPair2 <- basin %>% filter(sync_pair == "668_241")
basinPair2

## subset env to basin sites
sitespair <- c(unique(basinPair2$sYNGEO_ID1), unique(basinPair2$sYNGEO_ID2))
sitespair

basin_flow <- subset(melt_flow, sYNGEO_ID %in% sitespair)
head(basin_flow)
unique(basin_flow$env_var)

## merge by year

data <- merge(basin_flow, basinPair2,  by.x="year", by.y="year_removed")
str(data)
head(data)
unique(data$env_var)

ylim.prim <- c(-1, 1)   # in this example, synchrony
ylim.sec <- c(-100, 100)    # in this example, anomolie

b <- diff(ylim.prim)/diff(ylim.sec)
b

ggplot(data, aes(x =year)) +
  geom_line(aes(y=Correlation, group = Axis, colour = Axis)) +
  geom_line(aes(y=anomolie*b, group = env_var, linetype = env_var )) +
  geom_hline(yintercept=pair$Correlation, linetype="dashed",colour="grey")+
  scale_color_brewer(breaks = c("Axis1", "Axis2"),
                     palette="Set1",
                     labels = c("Fish Size", "Fish Reproductive Type"),
                     name = "Functional Trait") +
  scale_linetype_manual(breaks = c("flow_max", "flow_mean", "flow_min"),
                        values=c( "dotted", "solid", "dashed"),
                        labels = c("Max Discharge", "Mean Discharge", "Min Discharge"), 
                        name = "Environmental Variable") +
  scale_y_continuous(name = "Synchrony", limits= c(-1,1), sec.axis = sec_axis(~. /b, name = "Anomolie (m/s)")) +
  # scale_y_continuous("Events (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Discharge")) (m/s)
  facet_wrap(~sYNGEO_ID, nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Between site synchrony: site 668:241 test",
       x = "Year") #+ theme_bw(base_size = 15)



# Statistics --------------------------------------------------------------

# correlation on synchrony v anomolies

## upload env data

load(file="output_data/clim_data_melt.RData") ## melt_clim
load(file="output_data/flow_data_melt.RData") ## melt_flow

## upload and bind all basins 

load(file = "output_data/results_between_site_synchrony_29-46.RData")
synchrony_axis1 <- na.omit(synchrony_axis)
load(file = "output_data/results_between_site_synchrony_1-28.RData")
synchrony_axis2 <- na.omit(synchrony_axis)
rm(synchrony_axis)
basin <- rbind(synchrony_axis1, synchrony_axis2)

rm(synchrony_axis1)
rm(synchrony_axis2)
basin <- distinct(basin) ## many duplicates - check synchrony script 

basin <- as_tibble(basin)
unique(basin$year_removed)

basin <- basin %>% 
  mutate(year_removed = as.numeric(year_removed)) %>%
   mutate(Correlation = as.numeric(Correlation))
# check NAs
ind <- which(is.na(basin$year_removed))[1:10]
basin[ind,]

basin <- na.omit(basin)

melt_clim <- melt_clim %>%
  mutate(year = as.numeric(year))

melt_flow <- melt_flow %>%
  mutate(year = as.numeric(year))

## define sites in basin 

results=NULL

basins <- unique(basin$basin_ID)
# basins
b=1
p=1
a=1

## loop over basins
for(b in 1: length(basins)) {
  
  cat(paste("running basin", b))
  
  basinx <- filter(basin, basin_ID == basins[b])
  
  ## define sites in basin 
  site_prim <- unique(basinx$sYNGEO_ID2)
  
## loop over primary sites  
  
  for(p in 1: length(site_prim)) {
    
    ## filter synchrony sites by primary site (the constant through the pairs)
    basin_prim <- basinx %>% filter(sYNGEO_ID2 == site_prim[p]) %>%
      # dplyr::select(-sync_pair) %>%
      group_by(basin_ID, Axis, sYNGEO_ID1)
    
    sites_sec <- unique(basin_prim$sYNGEO_ID1)   
    sites_all <- c(sites_sec, site_prim[p])
    # basin_prim#
    
    for(a in 1:length(sites_all)) {
      
      # filter and format environmental data
      melt_climx <- filter(melt_clim, sYNGEO_ID %in% sites_all,!year == 2003 )
      # melt_climx$year <-as.numeric(as.character(melt_climx$year))
      
      melt_flowx <- filter(melt_flow, sYNGEO_ID %in% sites_all, !year == 2003 ) #%>%
      # melt_flowx$year <- as.numeric(as.character(melt_flowx$year))
      
      ## filter synchrony sites by secondary site ( the changing through the pairs)
      basin_sec <- basin_prim %>% filter(sYNGEO_ID1 == sites_all[a]) %>%
        rename(year=year_removed) #%>%
        # mutate(year = as.numeric(year), 
        #        Correlation = as.numeric(Correlation))
      
      ## merge env vars with synchrony and correlate by var type and axis (can change stat later!!)
      # str(basin_sec)
      # str( melt_flowx)
      melt_flow_prim <- melt_flowx %>% 
        filter(sYNGEO_ID == sites_all[a]) %>%
        full_join(basin_sec, by="year") %>%
        mutate(year_removed= year) %>%
        group_by(env_var, Axis, basin_ID, sYNGEO_ID2, sYNGEO_ID1) %>%
        rename(primary_site = sYNGEO_ID2, secondary_site = sYNGEO_ID1) %>%
        summarise(cor = cor(anomolie, Correlation)) #%>%
      
      melt_flow_prim
      melt_clim_prim <- melt_climx %>% 
        filter(sYNGEO_ID == sites_all[a]) %>%
        full_join(basin_sec, by="year") %>%
        group_by(env_var, Axis, basin_ID, sYNGEO_ID2, sYNGEO_ID1) %>%
        rename(primary_site = sYNGEO_ID2, secondary_site = sYNGEO_ID1) %>%
        summarise(cor = cor(anomolie, Correlation)) #%>%
      
      results <- rbind(results, melt_flow_prim, melt_clim_prim)
      
      
      
    }
  }
}


warnings()
  
results
unique(results$secondary_site) ## 748
unique(results$primary_site)  ## 748
unique(results$basin_ID)
save(results, file="output_data/cor_env_sync_results.RData")


# Figures -----------------------------------------------------------------



load(file="output_data/cor_env_sync_results.RData")
results
means <- c("flow_mean", "clim_mean")
results <- na.omit(results)

#then plot
ggplot(results, aes(x=factor(env_var),y=cor, fill=env_var))+
  geom_boxplot()+
  labs(title="Correlation") +
  facet_wrap(~Axis)

results_mean <- filter(results, env_var %in% means)
results_mean
results_sub <- filter(results_mean, basin_ID == "2080016510")

# 
# results_high <- filter(results_sub, cor <= -0.7 | cor >= 0.7)
# results_high

#then plot
 ggplot(results_sub, aes(x=factor(primary_site),y=cor, fill=Axis))+
  geom_boxplot()+
   scale_fill_brewer(breaks = c("Axis1", "Axis2"),
                      # values=c(  "red", "bluishgreen"),
                      palette="Set1",
                      labels = c("Fish Size", "Fish Reproductive Type"),
                      name = "Functional Trait") +
  labs(title="Correlation between environmental variables and (jackknife)
       synchrony: Basin 2080016510 (France)", 
       y="Correlation",
       x="Site") +
  facet_wrap(~env_var, nrow=2)


