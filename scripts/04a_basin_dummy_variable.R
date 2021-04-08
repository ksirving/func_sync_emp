###  adding the dummy basin  variable

library(tidyverse)
library(tidylog)

### data

## between site synchrony no boundaries
Sync <- read.csv("output_data/04_sync_data_similarity_euclidean_dist_no_boundaries_new_ord.csv")

Sync <- Sync %>%
  select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

Syncx <- Sync %>%
  select(Pair, Site_ID1, Site_ID2) %>%
  distinct()

head(Syncx)
dim(Sync) ## 370691

## country, basin, site info

SimSync <- read.csv( "output_data/04_sync_data_similarity_euclidean_dist_new_ord.csv")
head(SimSync)

SimSync <- SimSync %>%
  select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

basins <- SimSync %>%
  select(Pair, basin_ID, Country, Site_ID1, Site_ID2) %>%
  distinct()

head(basins)
dim(basins)

## join dfs by pair, if pair in basins is in pair in Sync then the sites are in the same basin

sum(Syncx$Pair %in% basins$Pair)  ## 8442

## which don't match?

Syncx$Pair %in% basins$Pair

?full_join
SyncJoin <- full_join(Syncx, basins, by = "Pair")
dim(SyncJoin) ## lose 17 pairs for some reason???

head(SyncJoin)

SyncJoin <- SyncJoin %>%
  mutate(InBasin = ifelse(is.na(basin_ID), 0, 1))


## remove extra columns so left with just pair and dummy variable

SyncJoin <- SyncJoin %>%
  select(Pair, InBasin, basin_ID, Country)

head(SyncJoin)

write.csv(SyncJoin, "output_data/04a_pairs_with_dummy_variable.csv")

### join back to main DF

Sync <- read.csv("output_data/04_sync_data_similarity_euclidean_dist_no_boundaries_new_ord.csv")

Sync <- Sync %>%
  select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

dim(SyncJoin) # 292247
dim(Sync) # 370691 why different? 


SyncBasinVar <- full_join(Sync, SyncJoin, by = "Pair", keep=T) ## 17 rows not matched
sum(is.na(SyncBasinVar)) ## 206460
dim(SyncBasinVar)
nas <- which(is.na(SyncBasinVar))

SyncBasinVar[370692:370708,] ## these are the 17 not matched

SyncBasinVar[741400:741409,] ## these are outside of dimensions, wtf??


SyncBasinVar <- SyncBasinVar[-nas,] ## this dim is now correct

write.csv(SyncBasinVar, "output_data/04a_sync_data_similarity_euclidean_dist_no_boundaries_new_ord_dummy_var.csv")

