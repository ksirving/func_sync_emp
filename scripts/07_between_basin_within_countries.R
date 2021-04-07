### between basin synchrony

## just for the differences in figures


## packages

library(tidyverse)
library(tidylog)

out.dir <- "/Users/katieirving/Documents/git/func_sync_emp/Figures/"

## data

CountriesDFx <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist_new_ord.csv") ## between basins, bound by country
TempCor <- read.csv("output_data/02_between_basin_sync_temp_all_together.csv") ## between basins no boundaries
FlowCor <- read.csv("output_data/02_between_basin_sync_flow_all_together.csv") ## between basins no boundaries
head(CountriesDFx)
head(TempCor)

## boxplots showing synchrony between basins per cpuntry

mean_sync <- CountriesDFx %>%
  group_by(Axis, BasinID2, Country) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity))

b1 <- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
b1

file.name1 <- paste0(out.dir, "Mean_basin_sync_per_country_new_ord.jpg")
ggsave(c1, filename=file.name1, dpi=300, height=5, width=6)

b2 <- ggplot(CountriesDFx, aes(x=Similarity, y=Correlation, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
b2

file.name1 <- paste0(out.dir, "Raw_basin_sync_per_country_new_ord.jpg")
ggsave(c2, filename=file.name1, dpi=300, height=5, width=6)

filter(mean_sync, Mean_Sim <=0.25)
filter(mean_sync, Mean_Sim >=0.75)

