## distance matrix

## workflow
library(sp)
library(raster)
library(ggplot2)
## get distances between sites
## Eucliean
## watercourse

## synchonry data

sync <- read.csv("output_data/02_results_between_site_synchrony_Jan2020.csv")
head(sync)
mean(na.omit(sync$Correlation))
sum(na.omit(sync$Correlation))

## site coords

fish_ab <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
head(fish_ab)

## define pairs
sync <- sync %>%
  rename(Pair = X) %>%
  mutate(Euclid_Dist_Meters = NA, Similarity = NA)

pairs <- unique(sync$Pair)

## get coords
SiteCoords <- fish_ab %>%
 dplyr::select(SiteID, Latitude, Longitude, HydroBasin) %>%
  distinct()

## loop over pairs
# p=2
for(p in 1:length(pairs)) {
  
  ## get pair from sync data
  
  pairx <- sync %>%
    filter(Pair == pairs[p])

  ## define sites
  S1 <- pairx$Site_ID1
  S2 <- pairx$Site_ID2

  ## get coords for each site
  CoordsS1 <- SiteCoords %>%
    filter(SiteID == S1) %>%
    dplyr::select(Longitude, Latitude, SiteID)
  
  CoordsS2 <- SiteCoords %>%
    filter(SiteID == S2) %>%
    dplyr::select(Longitude, Latitude, SiteID)

  sp::coordinates(CoordsS1) <- c("Longitude", "Latitude")
  sp::coordinates(CoordsS2) <- c("Longitude", "Latitude")
  
  #Make a distance matrix 
  dst <- pointDistance(CoordsS1,CoordsS2, lonlat=TRUE)

  ## add to dataframe
  sync[p,8] <- dst
 

}
  

  ## convert to similarities

## loops over basins

head(sync)

BasinsDFx <- NULL

Basins <- unique(sync$basin_ID)

for(s in 1:length(Basins)) {
  
  BasinsDF <- sync %>%
    filter(basin_ID == Basins[s])
  
  BasinsDF <- BasinsDF %>%
    mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
    mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))
  
  BasinsDFx <- rbind(BasinsDFx, BasinsDF)
  
}

head(BasinsDFx)

#then plot
ggplot(BasinsDFx, aes(x=Similarity,y=Correlation))+
  geom_line()+
  labs(title="Correlation") #+

  
  ## try one basin
  
  sync_basin <- BasinsDFx %>%
    filter(basin_ID == Basins[1])
  
  sync_basin
  
  #then plot
  ggplot(sync_basin, aes(x=Similarity,y=Correlation))+
    geom_line()+
    labs(title="Correlation") #+
  facet_wrap(~Axis)
  
  
  write.csv(BasinsDFx, "output_data/04_sync_data_similarity_euclidean_dist.csv")
