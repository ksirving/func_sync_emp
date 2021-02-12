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
### temperature
tempMat <- read.csv("/Users/katieirving/Documents/git/func_sync_emp/output_data/cor_matrix/02_temp_site_sync_2080016510_clim_max_raw.csv")
str(tempMat)
head(tempMat)

tempMat <- tempMat %>%
  select(-S5166, -S5448) %>%
  filter(!X %in% c("S5166", "S5448" ))

rownames(tempMat) <- tempMat$X
tempMat$X <- NULL
# syncMat <- na.omit(syncMat)

data <- as.matrix(tempMat)
head(data)
?heatmap
# Default Heatmap
heatmap(data)

### temp sync 

TempSync <- read.csv("output_data/02_temp_sync_between_sites.csv")

TempSync <- rename(TempSync, Pair = X, TempCor = Correlation)

## make sync values wider and format for join

basin_tally <- SimSync %>% group_by(Country) %>% count(basin_ID)
basin_tally

SimSyncx <- SimSync %>%
  select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

TempSync <- TempSync %>%
  filter(Env_Var == "clim_max_raw")
head(TempSync)

AllSync <- full_join(TempSync[, c(1,3,5)], SimSyncx, by = "Pair")
AllSync <- na.omit(AllSync)

AllSync <- AllSync %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation")

### axis 1 and max temp test
AllData <- AllSync %>%
   filter( Axis == "Axis1")

### need data set as a matrix to ruen into heatmap

## filter to basin

basin1sync <- AllData %>%
  filter(basin_ID == "2080016510") ### France

head(basin1sync)
basin1sync <- na.omit(basin1sync)

# x <- LETTERS[1:20]
# y <- paste0("var", seq(1,20))
# data <- expand.grid(X=x, Y=y)
# data$Z <- runif(400, 0, 5)
# 
# # Heatmap 
# ggplot(data, aes(X, Y, fill= Z)) + 
#   geom_tile()
out.dir <- "/Users/katieirving/Documents/git/func_sync_emp/Figures"

h1 <- ggplot(basin1sync, aes(Site_ID1, Site_ID2, fill= Correlation)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90))

file.name1 <- paste0(out.dir, "example_basin_heatmap_sync.jpg")
ggsave(h1, filename=file.name1, dpi=300, height=5, width=6)

h2 <- ggplot(basin1sync, aes(Site_ID1, Site_ID2, fill= Similarity)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90))

file.name1 <- paste0(out.dir, "example_basin_heatmap_distance.jpg")
ggsave(h2, filename=file.name1, dpi=300, height=5, width=6)

h3 <- ggplot(basin1sync, aes(Site_ID1, Site_ID2, fill= TempCor)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90))

file.name1 <- paste0(out.dir, "example_basin_heatmap_temp.jpg")
ggsave(h3, filename=file.name1, dpi=300, height=5, width=6)

### all on one 
# install.packages("akima")
require(akima)


# ## test on data - 1 basin
# data <- data.frame(x=basin1$Similarity,
#                    y=basin1$TempCor,
#                    distance=basin1$Correlation)
# head(data)
# resolution
# resolution <- 0.0001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
# a <- interp(x=data$x, y=data$y, z=data$distance, 
#             xo=seq(min(data$x),max(data$x),by=resolution), 
#             yo=seq(min(data$y),max(data$y),by=resolution), duplicate="mean")
# 
# # image(a) #you can of course modify the color palette and the color categories. See ?image for more explanation
# filled.contour(a, color.palette=heat.colors)

## all basins 
data <- data.frame(x=AllData$Similarity,
                   y=AllData$TempCor,
                   distance=AllData$Correlation)
head(data)
data <- na.omit(data)
resolution
resolution <- 0.01 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
a <- interp(x=data$x, y=data$y, z=data$distance, 
            xo=seq(min(data$x),max(data$x),by=resolution), 
            yo=seq(min(data$y),max(data$y),by=resolution), duplicate="mean")

# image(a) #you can of course modify the color palette and the color categories. See ?image for more explanation
filled.contour(a, color.palette=heat.colors)

