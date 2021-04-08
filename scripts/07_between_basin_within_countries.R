### between basin synchrony

## just for the differences in figures


## packages

library(tidyverse)
library(tidylog)

out.dir <- "/Users/katieirving/Documents/git/func_sync_emp/Figures/"

## data

BasinSync <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist_new_ord.csv") ## between basins, bound by country
TempCor <- read.csv("output_data/02_between_basin_sync_temp_all_together.csv") ## between basins no boundaries
FlowCor <- read.csv("output_data/02_between_basin_sync_flow_all_together.csv") ## between basins no boundaries
head(BasinSync)
head(TempCor)

## boxplots showing synchrony between basins per cpuntry


b1 <- ggplot(BasinSync, aes(x=Country, y=Correlation, fill=Axis)) +
  geom_boxplot() +
  facet_wrap(~Axis, scale="free")
b1
file.name1 <- paste0(out.dir, "Between_basin_sync_per_country_new_ord.jpg")
ggsave(b1, filename=file.name1, dpi=300, height=5, width=6)


