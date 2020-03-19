#### script to run the ordination of species traits. It taked the traits infor and the fish abundance data
### then match names, etc and uses FD::functcomp to weight traits by the relative abundance of spp in each site

setwd("/Users/katieirving/Documents/sYNGEO/func_emp")

# read the trait values
trt<-read.csv("Imputed_trait_values.csv")
head(trt)
# log the fecundity trait
trt$AVG_FECUND<-log(trt$AVG_FECUND)


install.packages("FD")
library(FD)
library(vegan)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)
library(ade4)

# read fish abund data
fish_ab<-read.csv("fishdata_selection_basins_same_time_window_cleanTaxo.csv")
fish_ab$New_names<-gsub(" ","_", fish_ab$New_names)

# check the matchin of spp
setdiff(trt$X, fish_ab$New_names)
setdiff(fish_ab$New_names, trt$X) # 8 spp w/o trt info

#
#trt2<-trt[match(fish_ab$New_names, trt$X),] # creates duplicates


# the trait df with matched spp
trt_match<-subset(trt, trt$X %in% fish_ab$New_names)

#matching fish abund with fish trt
fish_ab2<-subset(fish_ab, fish_ab$New_names %in% trt_match$X)

# make sure basin is a factor
fish_ab2$MAIN_BAS<-as.factor(fish_ab2$MAIN_BAS)

# add variable that gives unique id based on site, season and year
fish_ab2$site_seas_y<-paste(fish_ab2$sYNGEO_ID, fish_ab2$Season,fish_ab2$Year, sep="_")

#### just have a look
fish_ab2 %>% 
  filter(New_names=="Esox_lucius")
   ## some basins not havng season


# convert to wide format
#fish_mat<-dcast(fish_ab2, sYNGEO_ID  ~ Species, value.var="Abundances", fun.aggregate = sum)
## use reshape::dcast to convert to wide format using the "site_seas_y" variable as unique identified
fish_mat2<-dcast(fish_ab2, site_seas_y  ~ New_names, value.var="Abundances", fun.aggregate = sum)

## there are some sites sampled in both summer and autumn

# add  columns of year, site and seasons to the fish_mat2 matrix using "colsplit"
# Some sites have NAs on season; season values with NAs are pasted but turned into character#
fish_mat3<-cbind(colsplit(fish_mat2$site_seas_y,"_", c("site","season", "year")),fish_mat2)

# add column site_year (if duplicates are here means two seasons are covered; then we can aggregate these within a site ##
fish_mat3$site_y<-paste(fish_mat3$site, fish_mat3$year, sep="_")

# find sites with two seasons
which((duplicated(fish_mat3$site_y)==TRUE))
xx<-which((duplicated(fish_mat3$site_y)==TRUE))

#dupl<-fish_mat3[xx,]

#dupl.mat<-fish_mat3 %>% filter(site %in% dupl$site)




# aggregate samples with duplicates (two seasons at the same site are averaged)
fish_mat4<- aggregate(fish_mat3, mean, by=list(site_year=fish_mat3$site_y))
# get rid of a variable created by the "agregate" command
fish_mat4$site_y<-NULL
# warnings()
#assign row names to the final fish abund matrix
row.names(fish_mat4)<-fish_mat4$site_year


#############################################
## towards the df of sites by traits ######

## functcomp requires spp and site names to be row names and column names etc, and the df should not contain other info ##
trt_match<- trt_match[order(trt_match$X),] # sort species names in the trait df (they should match the fish matrix)
row.names(trt_match)<-trt_match$X
trt_match$X<-NULL

# create a "clean" df (called fish for traits "fish_fortr") with fish abundance for the functcomp command (weighting traits by spp relative abund)
fish_fortrt<-fish_mat4[,c(6:205)]  
row.names(fish_fortrt)<-fish_mat4$site_year
#fish_fortrt$site_seas_y<-NULL

## the siteXtrait matrix
trt_matrix<-functcomp(trt_match, as.matrix(fish_fortrt), CWM.type = "all")

## run PCA (called "julian_pca') with weighted traits; The weight assigned to the categorical feeding traits are lower
?dudi.pca
julian_pca<-dudi.pca(trt_matrix, col.w = c(1,1,1,1,1,1,1,0.2,0.2,0.2,0.2,0.2))

scatter(julian_pca)
julian_pca


julian_pca$li # the row coordinates
julian_pca$co # the vriable loading on the pca axes


# prepare and export the overall sites coordinates

trait_pca_scores<-cbind(julian_pca$li, site_year_basin)

setwd("~/Documents/SynGeo/Functional")
write.csv(trait_pca_scores, "trait_pca_scores.csv", row.names = T)

### Plotting #####
# plot with factoextra #
install.packages("factoextra")
library(factoextra)

# create a df with all sites, basin and year info for plotting aid
site_year_basin<-fish_mat4[,c(1,2)]
# add the basin id 
site_year_basin$MAIN_BAS<- fish_ab2$MAIN_BAS[match( site_year_basin$site, fish_ab2$sYNGEO_ID)]
# add the origin
site_year_basin$origin<-fish_ab2$Origin[match( site_year_basin$site, fish_ab2$sYNGEO_ID)]
# add the year
site_year_basin<-cbind(site_year_basin, colsplit(site_year_basin$site_year, "_", c("sINGEO_ID", "year")))
names(site_year_basin)


# the plots
fviz_pca(julian_pca, habillage=site_year_basin$origin, label="var", geom="point", 
         addEllipses = T, ellipse.type='convex', ellipse.alpha=0.01, labelsize=4, col.var="black")

pca_year<-fviz_pca(julian_pca, habillage=as.numeric(site_year_basin$year),label="var", geom="point", 
         addEllipses = T, ellipse.type='convex', ellipse.alpha=0.01, labelsize=4, col.var="black")

pca_year+scale_color_grey()

pca_year+scale_color_viridis_d()



