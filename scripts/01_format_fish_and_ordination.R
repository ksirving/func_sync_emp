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
str(fish_ab)
## remove basins - Sweden 2080030650 and 2080031160
## keep only origin Ohio and LTRM in mississippi
## change to relative abundance
## missing trait values - remove fish with less than 2 traits (check that it's less than 5%)
## use gower distance to interpolate?, 

## remove basins - Sweden 2080030650 and 2080031160
## keep only origin Ohio and LTRM in mississippi
basins_remove <- c(2080031160, 2080030650)
origin_remove <- c("Tennessee", "ltr")

fish_ab <- fish_ab %>%
  filter(!HydroBasin %in% basins_remove, !ORIGIN %in% origin_remove) 

# test <- fish_ab %>%
#   filter(SiteID %in% c("S4370", "S482")) ##only 1 species at these sites in 2012

## change to relative abundance - site/year
fish_ab_rel <- fish_ab %>%
  group_by(SiteID, Year) %>%
  mutate(TotalAbundanceSiteYear = sum(Abundance)) %>% ## total species abundance at each site/year
  ungroup %>%
  group_by(SiteID) %>%
  mutate(TotalAbundanceSite = sum(Abundance)) %>% ## total species abundance at each site
  ungroup() %>%
  group_by(Species, SiteID) %>%
  mutate(TotalAbundanceSiteSpecies = sum(Abundance)) ## abundance of each species at each site
#%>%


fish_ab_rel <- fish_ab_rel %>%
  ungroup() %>%
  mutate(RelAbundanceSiteYear = (Abundance/TotalAbundanceSiteYear)*100) %>%
  mutate(RelAbundanceSite = (TotalAbundanceSiteSpecies/TotalAbundanceSite)*100)


# unique(FishData$UnitAbundance)
## missing trait values - remove fish with less than 2 traits (check that it's less than 5%)

## upload trait data

trt <- read.csv("input_data/Bio/Matrix_traits_selection_10262020.csv")
head(trt)
dim(trt) ## 272, 7 traits

## count NAs in each row
trt <- trt %>%
  mutate(number_nas = rowSums(is.na(trt)))

rare_species <- trt %>%
  filter(number_nas > 4)

RSp <- rare_species$Species
RSp
# [1] "Barbatula quignardi"     "Etheostoma tennesseense" "Gobio occitaniae"       
# [4] "Leuciscus burdigalensis" "Notropis wickliffi"      "Pachychilon pictum"     
# [7] "Percina williamsi"       "Phoxinus bigerri"        "Squalius laietanus" 
# 
# ## check species are only 5% of abundance at site/year
# 
# head(fish_ab_rel)
# length(unique(fish_ab_rel$Species))
# 
# fish_ab_rel_rare <- fish_ab_rel %>%
#   filter(Species %in% RSp)
# ## save
# write.csv(fish_ab_rel_rare, "output_data/01_species_rel_adundance_2_or_less_traits.csv")
# 
# rare_species <- rare_species %>% 
#   filter(Species %in% fish_ab_rel_rare$Species)
# 
# write.csv(rare_species, "output_data/01_species_traits_2_or_less_traits.csv")
#   
# rare_species
# unique(fish_ab_rel_rare$Species) ## "Squalius laietanus" "Phoxinus bigerri"   "Gobio occitaniae"   "Notropis wickliffi"
# 
# # [1] "Squalius laietanus"      "Phoxinus bigerri"        "Barbatula quignardi"    
# # [4] "Leuciscus burdigalensis" "Pachychilon pictum"      "Gobio occitaniae"       
# # [7] "Notropis wickliffi" 
# 
# ## tally number of basins and number of sites fish with fewer traits occur
# fish_ab_rel_rare$HydroBasin <- as.character(fish_ab_rel_rare$HydroBasin)
# ?tally
# 
# SiteTally <- fish_ab_rel_rare %>%
#   group_by(Species, HydroBasin) %>%
#   summarise(No_Sites = length(unique(SiteID)))# %>%
#   # summarise(No_basins = length(unique(HydroBasin))) #%>% ## number of basins species occurs
# SiteTally
# write.csv(SiteTally, "output_data/01_no_sites_basins_2_or_less_traits.csv")

## remove rare species from main df

fish_ab_rel <- fish_ab_rel %>%
  filter(!Species %in% RSp)

## filter trt to same species as fish df

fish_sp <- unique(fish_ab_rel$Species)
fish_sp
trt <- trt %>%
  filter(Species %in% fish_sp)

# log the fecundity trait
trt$AVG_FECUND<-log(trt$AVG_FECUND)

# check the matchin of spp
setdiff(trt$Species, fish_ab_rel$Species)
setdiff(fish_ab_rel$Species, trt$Species) # all matched!!!
# length(unique(trt$Species)) ## 272
# length(unique(fish_ab_rel$Species)) ## 236 - fewer species due to removal of basins and origins
# make sure basin is a factor
fish_ab_rel$HydroBasin<-as.factor(fish_ab_rel$HydroBasin)


# add variable that gives unique id based on site, season and year
head(fish_ab_rel)
length(unique(fish_ab_rel$Species)) ## 229
dim(trt) ## 229
fish_ab2 <- fish_ab_rel
fish_ab2$site_seas_year<-paste(fish_ab2$SiteID, fish_ab2$Month,fish_ab2$Year, sep="_")
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
names(fish_mat3)
# create a "clean" df (called fish for traits "fish_fortr") with fish abundance for the functcomp command (weighting traits by spp relative abund)
fish_fortrt<-fish_mat3[,c(5:233)]  
row.names(fish_fortrt)<-fish_mat3$site_year
head(fish_fortrt)



## the siteXtrait matrix
trt_matrix<-functcomp(trt, as.matrix(fish_fortrt), CWM.type = "all")
head(trt_matrix)
sum(is.na(trt_matrix)) ## 30 with species removed
apply(is.na(trt_matrix), 2, which)
write.csv(trt_matrix, "output_data/01_trait_matrix_weighted_by_abundance.csv")
# trt_matrix <- read.csv("output_data/01_trait_matrix_weighted_by_abundance.csv")
## change NAs to 0 for now, change later !!!!!!!!
## use gower here
trt_matrix[is.na(trt_matrix)] <- 0

trt_matrix$number_nas <- NULL
## run PCA (called "julian_pca') with weighted traits; The weight assigned to the categorical feeding traits are lower
?dudi.pca
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

write.csv(trait_pca_scores, "output_data/01_trait_pca_scores_new_sites.csv", row.names = T)


pca_origin

# the plots
pca_origin <- fviz_pca(julian_pca, habillage=site_year_basin$Country, label="var", geom="point", 
         addEllipses = T, ellipse.type='convex', ellipse.alpha=0.01, labelsize=4, col.var="black")

pca_year<-fviz_pca(julian_pca, habillage=as.numeric(site_year_basin$year),label="var", geom="point", 
                   addEllipses = T, ellipse.type='convex', ellipse.alpha=0.01, labelsize=4, col.var="black")
pca_year
pca_year+scale_color_grey()

pca_year+scale_color_viridis_d()




