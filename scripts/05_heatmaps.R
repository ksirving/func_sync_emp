### heatmaps

library(sp)
library(raster)
library(ggplot2)
library(tidyverse)
library(tidyr)

## work flow

## between site synchrony heat map between sites - done, not very useful
## between site synchrony heatmap similarities (temp & distance)
## between site synchrony mean similarities/correlartions
## mean between basin synchrony with countries, temp/distance similarities

## synchrony data

sync <- read.csv("output_data/02_results_between_site_synchrony_Jan2020.csv")
head(sync)

## synchrony with distance similarities

SimSync <- read.csv( "output_data/04_sync_data_similarity_euclidean_dist.csv")
head(SimSync)

## heatmap

syncMat <- read.csv("/Users/katieirving/Documents/git/func_sync_emp/output_data/cor_matrix/02_site_sync_2080016510_Axis1.csv")
str(syncMat)
head(syncMat)

syncMat <- syncMat %>%
  select(-S5166, -S5448) %>%
  filter(!X %in% c("S5166", "S5448" ))

rownames(syncMat) <- syncMat$X
syncMat$X <- NULL
# syncMat <- na.omit(syncMat)

data <- as.matrix(syncMat)
head(data)
?heatmap
# Default Heatmap
heatmap(data)

### temp sync 

TempSync <- read.csv("output_data/02_temp_sync_between_sites.csv")
head(TempSync)
TempSync <- rename(TempSync, Pair = X, TempCor = Correlation)

AllSync <- full_join(TempSync[, c(1,3,5)], SimSync, by = "Pair")
head(AllSync)

### axis 1 and max temp test
data <- AllSync %>%
   filter(Env_Var == "clim_max_raw", Axis == "Axis1")


head(data)
### need data set as a matrix to ruen into heatmap

