## distance matrix

## workflow
library(sp)
library(raster)
library(ggplot2)
## get distances between sites
## Eucliean
## watercourse

## synchonry data

sync <- read.csv("output_data/02a_results_between_site_synchrony_new_ord_new_sites.csv")
head(sync)
mean(na.omit(sync$Correlation))
sum(na.omit(sync$Correlation))

## site coords

fish_ab <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
head(fish_ab)


# within basin  -----------------------------------------------------------

## define pairs
sync <- sync %>%
  rename(Pair = X) %>%
  mutate(Euclid_Dist_Meters = NA, Similarity = NA, MeanLat = NA, MeanLon = NA)
head(sync)
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

  # get mean latitude/longitude
  MeanLat <- (CoordsS1$Latitude+CoordsS2$Latitude)/2
  MeanLon <- (CoordsS1$Longitude+CoordsS2$Longitude)/2
  
  ## add to dataframe

  ## add to dataframe
  sync[p,8] <- dst
  sync[p,10] <- MeanLat
  sync[p,11] <- MeanLon
 

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
  
  
  write.csv(BasinsDFx, "output_data/04_sync_data_similarity_euclidean_dist_new_ord.csv")
  
  
  ### no boundaries
  
  sync <- read.csv("output_data/02a_results_between_site_synchrony_new_sites_new_ord_no_boundaries.csv")
  head(sync)
  mean(na.omit(sync$Correlation))
  sum(na.omit(sync$Correlation))
  
  ## site coords
  
  fish_ab <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
  head(fish_ab)
  
  
  ## define pairs
  sync <- sync %>%
    rename(Pair = X) %>%
    mutate(Euclid_Dist_Meters = NA, Similarity = NA, MeanLat = NA, MeanLon = NA)
  head(sync)
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
    
    # get mean latitude/longitude
    MeanLat <- (CoordsS1$Latitude+CoordsS2$Latitude)/2
    MeanLon <- (CoordsS1$Longitude+CoordsS2$Longitude)/2
    
    ## add to dataframe
    
    ## add to dataframe
    sync[p,6] <- dst
    sync[p,8] <- MeanLat
    sync[p,9] <- MeanLon
    
    
  }
  
  head(sync)
  

  ## convert to similarities
  
  ## loops over basins
  
  head(sync)
  
    syncDF <- sync %>%
      mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
      mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))
    


  
  #then plot
  ggplot(syncDF, aes(x=Euclid_Dist_Meters,y=Correlation))+
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
  
  
  write.csv(syncDF, "output_data/04_sync_data_similarity_euclidean_dist_no_boundaries_new_ord.csv")
  
  
  
  

# between basin -----------------------------------------------------------

  
  ### centroid coords for each basin
  
  ## site coords
  
  fish_ab <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
  head(fish_ab)
  
  ## get coords
  SiteCoords <- fish_ab %>%
    dplyr::select(SiteID, Latitude, Longitude, HydroBasin) %>%
    distinct()
  head(SiteCoords)
  ## make spatial
  install.packages("rgeos")
  library(rgeos)
  install.packages("geosphere")
  library(geosphere)
  
  # sp::coordinates(SiteCoords) <- c("Longitude", "Latitude")
  
  basins <- unique(fish_ab$HydroBasin)
  basins[1]
  
  centroids <- NULL
  
  b=1
  for(b in 1:length(basins)) {
    
    pol <- fish_ab %>%
      filter(HydroBasin == basins[b]) %>%
      dplyr::select(SiteID, Latitude, Longitude, HydroBasin, Country) %>%
      distinct()
    
    pol_coords <- pol %>%
      dplyr::select( Longitude, Latitude) #%>%
    
    cents <- centroid(pol_coords)
    
    centroids<-rbind(centroids, cbind(basins[b], pol$HydroBasin[1],
                                      pol$Country[1], cents))
    
  }
  

  
  
  centroids <- as.data.frame(centroids)
  centroids
  names(centroids) <- c("HydroBasin", "HydroBasin", "Country", "Longitude", "Latitude")
  
  head(centroids)
  
  write.csv(centroids, "output_data/04_basin_centroid_coord.csv")
  
  ## get distances and similarities
  
  BasinSync <- read.csv("output_data/02_between_basin_sync_per_country_new_ord.csv")
  BasinSync
 
  BasinSync <- BasinSync %>%
    rename(BasinPairs = X)
  
  pairs <- unique(BasinSync$BasinPairs)
  
  centroids <- centroids[,-1]
  centroids$Longitude <- as.numeric(centroids$Longitude)
  centroids$Latitude <- as.numeric(centroids$Latitude)
  head(centroids)
  str(centroids)
  dim(BasinSync)
  ## loop over centroids
  p=2
  for(p in 1:length(pairs)) {
    
    ## get pair from sync data
    
    pairx <- BasinSync %>%
      filter(BasinPairs == pairs[p])
    
    ## define sites
    S1 <- pairx$BasinID1
    S2 <- pairx$BasinID2
    
    ## get coords for each site
    CoordsS1 <- centroids %>%
      filter(HydroBasin == S1) %>%
      dplyr::select(Longitude, Latitude, HydroBasin)
    
    CoordsS2 <- centroids %>%
      filter(HydroBasin == S2) %>%
      dplyr::select(Longitude, Latitude, HydroBasin)
    
    sp::coordinates(CoordsS1) <- c("Longitude", "Latitude")
    sp::coordinates(CoordsS2) <- c("Longitude", "Latitude")
    
    # get mean latitude/longitude
    MeanLat <- (CoordsS1$Latitude+CoordsS2$Latitude)/2
    MeanLon <- (CoordsS1$Longitude+CoordsS2$Longitude)/2
    
   
    #Make a distance matrix 
    dst <- pointDistance(CoordsS1,CoordsS2, lonlat=TRUE)
    
    ## add to dataframe
    BasinSync[p,7] <- dst
    BasinSync[p,8] <- MeanLat
    BasinSync[p,9] <- MeanLon
    
  }
  
  head(BasinSync)
  colnames(BasinSync)[7] <- "Euclid_Dist_Meters"
  colnames(BasinSync)[8] <- "MeanLat"
  colnames(BasinSync)[9] <- "MeanLon"
  
  ## convert to similarities
  
  ## loops over basins
  
  head(BasinSync)
  
  CountriesDFx <- NULL
  
  Countries <- unique(BasinSync$Country)
  
  for(s in 1:length(Countries)) {
    
    BasinsDF <- BasinSync %>%
      filter(Country == Countries[s])
    
    BasinsDF <- BasinsDF %>%
      mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
      mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))
    
    CountriesDFx <- rbind(CountriesDFx, BasinsDF)
    
  }
  
  head(CountriesDFx)
  
  write.csv(CountriesDFx, "output_data/04_basin_sync_data_similarity_euclidean_dist_new_ord.csv")
  
  # #then plot
  # ggplot(CountriesDFx, aes(x=Similarity,y=Correlation))+
  #   geom_line()+
  #   labs(title="Correlation") #+
  
  
  ### distances/similarities for all basins (not binned per country)
  
  ## get distances and similarities
  
  BasinSync <- read.csv("output_data/02a_between_basin_sync_all_together_new_ord.csv")
  BasinSync
  
  BasinSync <- BasinSync %>%
    rename(BasinPairs = X)
  
  pairs <- unique(BasinSync$BasinPairs)
  
  centroids <- read.csv("output_data/04_basin_centroid_coord.csv")
  
  centroids <- centroids[,-c(1,3)]
  centroids$Longitude <- as.numeric(centroids$Longitude)
  centroids$Latitude <- as.numeric(centroids$Latitude)
  head(centroids)
  str(centroids)
  dim(BasinSync)
  ## loop over centroids
  p=2
  for(p in 1:length(pairs)) {
    
    ## get pair from sync data
    
    pairx <- BasinSync %>%
      filter(BasinPairs == pairs[p])
    
    ## define sites
    S1 <- pairx$BasinID1
    S2 <- pairx$BasinID2
    
    ## get coords for each site
    CoordsS1 <- centroids %>%
      filter(HydroBasin == S1) %>%
      dplyr::select(Longitude, Latitude, HydroBasin)
    
    CoordsS2 <- centroids %>%
      filter(HydroBasin == S2) %>%
      dplyr::select(Longitude, Latitude, HydroBasin)
    
    sp::coordinates(CoordsS1) <- c("Longitude", "Latitude")
    sp::coordinates(CoordsS2) <- c("Longitude", "Latitude")
    
    
    # get mean latitude/longitude
    MeanLat <- (CoordsS1$Latitude+CoordsS2$Latitude)/2
    MeanLon <- (CoordsS1$Longitude+CoordsS2$Longitude)/2
    
    
    #Make a distance matrix 
    dst <- pointDistance(CoordsS1,CoordsS2, lonlat=TRUE)
    
    ## add to dataframe
    BasinSync[p,6] <- dst
    BasinSync[p,7] <- MeanLat
    BasinSync[p,8] <- MeanLon
    
    
  }
  
  head(BasinSync)

  colnames(BasinSync)[6] <- "Euclid_Dist_Meters"
  colnames(BasinSync)[7] <- "MeanLat"
  colnames(BasinSync)[8] <- "MeanLon"
  
  ## convert to similarities
  
  ## loops over basins
  
  head(BasinSync)
  
  BasinsDF <- BasinSync %>%
    mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
    mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))
  

  
  head(BasinsDF)
  
  write.csv(BasinsDF, "output_data/04_basin_sync_data_similarity_euclidean_dist_all_together_new_ord.csv")
  



