### format species data and ordination
# install.packages("FD")
library(FD)
library(vegan)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)
library(ade4)
## upload fish abundance data
setwd("/Users/katieirving/Documents/git/func_sync_emp")


fish_ab <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")

head(fish_ab)

# unique(FishData$UnitAbundance)

## upload trait data

trt <- read.csv("input_data/Bio/Matrix_traits_selection_10262020.csv")
head(trt)
# log the fecundity trait
trt$AVG_FECUND<-log(trt$AVG_FECUND)

# check the matchin of spp
setdiff(trt$Species, fish_ab$Species)
setdiff(fish_ab$Species, trt$Species) # all matched!!!

# make sure basin is a factor
fish_ab$HydroBasin<-as.factor(fish_ab$HydroBasin)

unique(fish_ab$Month)

# add variable that gives unique id based on site, season and year
fish_ab$site_seas_year<-paste(fish_ab$SiteID, fish_ab$Month,fish_ab$Year, sep="_")
fish_ab2 <- fish_ab
#### just have a look
fish_ab2 %>% 
  filter(Species=="Esox lucius")
## some basins not havng season

# convert to wide format
#fish_mat<-dcast(fish_ab2, sYNGEO_ID  ~ Species, value.var="Abundances", fun.aggregate = sum)
## use reshape::dcast to convert to wide format using the "site_seas_y" variable as unique identified
fish_mat2<-dcast(fish_ab2, site_seas_year  ~ Species, value.var="Abundance", fun.aggregate = sum)

# add  columns of year, site and seasons to the fish_mat2 matrix using "colsplit"
# Some sites have NAs on season; season values with NAs are pasted but turned into character#
fish_mat3<-cbind(colsplit(fish_mat2$site_seas_year,"_", c("site","month", "year")),fish_mat2)

# add column site_year (if duplicates are here means two seasons are covered; then we can aggregate these within a site ##
fish_mat3$site_year<-paste(fish_mat3$site, fish_mat3$year, sep="_")

# find sites with two seasons
which((duplicated(fish_mat3$site_year)==TRUE)) ### no sites sampled twice in a year

#assign row names to the final fish abund matrix
row.names(fish_mat3)<-fish_mat3$site_year
head(fish_mat3)
names(fish_mat3)

#############################################
## towards the df of sites by traits ######

## functcomp requires spp and site names to be row names and column names etc, and the df should not contain other info ##
trt<- trt[order(trt$Species),] # sort species names in the trait df (they should match the fish matrix)
row.names(trt)<-trt$Species
trt$Species<-NULL
head(trt)
unique(trt$AVG_RGUILD)

# create a "clean" df (called fish for traits "fish_fortr") with fish abundance for the functcomp command (weighting traits by spp relative abund)
fish_fortrt<-fish_mat3[,c(5:276)]  
row.names(fish_fortrt)<-fish_mat3$site_year
head(fish_fortrt)

?functcomp
## the siteXtrait matrix
trt_matrix<-functcomp(trt, as.matrix(fish_fortrt), CWM.type = "all")
head(trt_matrix)
sum(is.na(trt_matrix)) ## 40
apply(is.na(trt_matrix), 2, which)
write.csv(trt_matrix, "output_data/01_trait_matrix_weighted_by_abundance.csv")
trt_matrix <- read.csv("output_data/01_trait_matrix_weighted_by_abundance.csv")
## change NAs to 0 for now, change later !!!!!!!!

trt_matrix[is.na(trt_matrix)] <- 0
head(trt_matrix)
## run PCA (called "julian_pca') with weighted traits; The weight assigned to the categorical feeding traits are lower

julian_pca<-dudi.pca(trt_matrix, col.w = c(1,1,1,1,1,1,0.2,0.2,0.2,0.2,0.2), scannf = FALSE, nf = 2)
# scatter(julian_pca)
# julian_pca

### Plotting #####
# plot with factoextra #
# install.packages("factoextra")
library(factoextra)
head(fish_ab2)
# create a df with all sites, basin and year info for plotting aid
site_year_basin<-fish_mat3[,c(1,3)]
head(site_year_basin)
# add the basin id 
site_year_basin$HydroBasin<- fish_ab2$HydroBasin[match( site_year_basin$site, fish_ab2$SiteID)]
# add the origin
site_year_basin$Country<-fish_ab2$Country[match(site_year_basin$site, fish_ab2$SiteID)]
# add the year
# site_year_basin<-cbind(site_year_basin, colsplit(site_year_basin$site_year, "_", c("SiteID", "Year")))
names(site_year_basin)
julian_pca$li # the row coordinates
julian_pca$co # the vriable loading on the pca axes

# prepare and export the overall sites coordinates

trait_pca_scores<-cbind(julian_pca$li, site_year_basin)

write.csv(trait_pca_scores, "output_data/trait_pca_scores_new_sites.csv", row.names = T)


pca_origin

# the plots
pca_origin <- fviz_pca(julian_pca, habillage=site_year_basin$Country, label="var", geom="point", 
         addEllipses = T, ellipse.type='convex', ellipse.alpha=0.01, labelsize=4, col.var="black")

pca_year<-fviz_pca(julian_pca, habillage=as.numeric(site_year_basin$year),label="var", geom="point", 
                   addEllipses = T, ellipse.type='convex', ellipse.alpha=0.01, labelsize=4, col.var="black")
pca_year
pca_year+scale_color_grey()

pca_year+scale_color_viridis_d()




