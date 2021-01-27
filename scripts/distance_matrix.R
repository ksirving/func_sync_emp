## distance matrix

## workflow
library(sp)
library(raster)
## get distances between sites
## Eucliean
## watercourse

## synchonry data

sync <- read.csv("output_data/02_results_between_site_synchrony_Jan2020.csv")


## site coords

fish_ab <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
head(fish_ab)

SiteCoords <- fish_ab %>%
 select(SiteID, Latitude, Longitude, HydroBasin) %>%
  distinct()

## make spatial
head(SiteCoords)
str(SiteCoords)

sp::coordinates(SiteCoords) <- c("Latitude", "Longitude")
## get distance between points per basin
SiteCoords[1,1]
pointDistance(p1, p2, lonlat)
## https://stackoverflow.com/questions/32363998/function-to-calculate-geospatial-distance-between-two-points-lat-long-using-r
# NOT RUN {
a <- cbind(c(1,5,55,31),c(3,7,20,22))
b <- cbind(c(4,2,8,65),c(50,-90,20,32))   
a
b
pointDistance(c(0, 0), c(1, 1), lonlat=FALSE)
pointDistance(c(0, 0), c(1, 1), lonlat=TRUE)
pointDistance(c(0, 0), a, lonlat=TRUE)
pointDistance(a, b, lonlat=TRUE)
dst
#Make a distance matrix 
dst <- pointDistance(a, lonlat=TRUE)
# coerce to dist object
dst <- as.dist(dst)
# }