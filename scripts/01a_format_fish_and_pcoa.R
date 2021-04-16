### format species data and ordination
# install.packages("FD")
library(FD)
library(vegan)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)
library(ade4)
library(cluster)
library(ggplot2)
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
  filter(number_nas > 5)

RSp <- rare_species$Species
RSp

## remove rare species (species with 2 or less traits) from main df

fish_ab_rel <- fish_ab_rel %>%
  filter(!Species %in% RSp)

## filter trt to same species as fish df

fish_sp <- unique(fish_ab_rel$Species)
fish_sp
trt <- trt %>%
  filter(Species %in% fish_sp)

### check distribution of traits and transform
hist(log(trt$AVG_MXL)+1) ## skewed
hist(log(trt$AVG_LMAT)+1) ## skewed
hist(log(trt$AVG_AGEMAT)+1) ## skewed
hist(log(trt$AVG_LONGY)+1) ## skewed
hist(log(trt$AVG_FECUND)+1) ## skewed
hist(trt$AVG_EGGSIZE) ## less skewed - do we want to transform this one?
## some transformations not quite normal

head(trt)
# log traits

trt$AVG_MXL<-log(trt$AVG_MXL)
trt$AVG_LMAT<-log(trt$AVG_LMAT)
trt$AVG_AGEMAT<-log(trt$AVG_AGEMAT)
trt$AVG_LONGY<-log(trt$AVG_LONGY)
trt$AVG_FECUND<-log(trt$AVG_FECUND)
trt$AVG_EGGSIZE<-log(trt$AVG_EGGSIZE)

## rank reproductive guild

unique(trt$AVG_RGUILD)

# NG = non guarders
# OS = Open substrate
# NS = Nest Spawners
# G = Guarders
# BH = brood hiders
# SC = Substrate choosers

# "NG_OS"  non guarder, open substrate
# "NG_BH"  non guarders, brood hiders
# "G_SC"   guarder, substare chooser
# "G_NS"   guarder,  nest spawners
# "Bearer"

## put in correct order
trt <- trt %>%
  mutate(AVG_RGUILD_ORD = NA) %>%
  mutate(AVG_RGUILD_ORD = replace(AVG_RGUILD_ORD, AVG_RGUILD == "Bearer", 5)) %>%
  mutate(AVG_RGUILD_ORD = replace(AVG_RGUILD_ORD, AVG_RGUILD == "G_NS", 4)) %>%
  mutate(AVG_RGUILD_ORD = replace(AVG_RGUILD_ORD, AVG_RGUILD == "G_SC", 3)) %>%
  mutate(AVG_RGUILD_ORD = replace(AVG_RGUILD_ORD, AVG_RGUILD == "NG_BH", 2)) %>%
  mutate(AVG_RGUILD_ORD = replace(AVG_RGUILD_ORD, AVG_RGUILD == "NG_OS", 1)) %>%
  select(-AVG_RGUILD)

head(trt)

trt$AVG_RGUILD_ORD =  as.ordered(trt$AVG_RGUILD_ORD)

# check the matchin of spp
setdiff(trt$Species, fish_ab_rel$Species)
setdiff(fish_ab_rel$Species, trt$Species) # all matched!!!
# length(unique(trt$Species)) ## 272
# length(unique(fish_ab_rel$Species)) ## 236 - fewer species due to removal of basins and origins
# make sure basin is a factor
fish_ab_rel$HydroBasin<-as.factor(fish_ab_rel$HydroBasin)


# add variable that gives unique id based on site, season and year
# head(fish_ab_rel)
# length(unique(fish_ab_rel$Species)) ## 232
# dim(trt) ## 232
fish_ab2 <- fish_ab_rel
fish_ab2$site_seas_year<-paste(fish_ab2$SiteID, fish_ab2$Month,fish_ab2$Year, sep="_")
#### just have a look
# fish_ab2 %>% 
#   filter(Species=="Esox lucius")
## some basins not havng season

# convert to wide format
#fish_mat<-dcast(fish_ab2, sYNGEO_ID  ~ Species, value.var="Abundances", fun.aggregate = sum)
## use reshape::dcast to convert to wide format using the "site_seas_y" variable as unique identified
fish_mat2<-dcast(fish_ab2, site_seas_year  ~ Species, value.var="RelAbundanceSiteYear", fun.aggregate = sum)
names(fish_ab2)
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
trt$number_nas<-NULL

head(trt) ## traits per species
head(fish_mat3) ### abundance of species per site year

names(fish_mat3)

# create a "clean" df (called fish for traits "fish_fortr") with fish abundance for the functcomp command (weighting traits by spp relative abund)
fish_fortrt<-fish_mat3[,c(5:236)]  
row.names(fish_fortrt)<-fish_mat3$site_year
# head(fish_fortrt)
# dim(trt)
# dim(fish_fortrt)
# ?functcomp

## computes the functional composition of functional traits, by community weighted mean
# trt_matrix<-functcomp(trt, as.matrix(fish_fortrt), CWM.type = "all")
trt_matrix<-functcomp(trt, as.matrix(fish_fortrt), CWM.type = "dom")  
head(trt_matrix)

# trt_matrix <- trt_matrix[1:1000,]

trt_matrix$AVG_RGUILD_ORD = as.ordered(trt_matrix$AVG_RGUILD_ORD)
DDf = gowdis(trt_matrix, ord = "podani") 

## check NAs
# sum(is.na(trt_matrix)) ## 40 with species removed
# sum(is.na(DDf)) ## 0
# apply(is.na(trt_matrix), 2, which)

write.csv(trt_matrix, "output_data/01a_trait_matrix_weighted_by_abundance_transformed.csv")

# trt_matrix <- read.csv("output_data/01a_trait_matrix_weighted_by_abundance_transformed.csv")
# View(trt_matrix)
# str(trt_matrix)

# trt_matrix <- trt_matrix[,-1]
# PCoA with ape package

# PCOA <- pcoa(DDf)
# 
# # plot the eigenvalues and interpret
# barplot(PCOA$values$Relative_eig[1:10])

# PCOA <- pcoa(dist, correction = "cailliez")
# 
# # Plot your results
# biplot.pcoa(PCOA)
# ?biplot.pcoa
# ## doesn't have traits, add them separately
# pca_origin <- biplot.pcoa(PCOA, trt_matrix_sub)

### PCoA with cmdscale


pcoa<-cmdscale(DDf,eig=T, add=T, k=200)
class(pcoa)
# save(pcoa, file="models/01a_pcoa_cmdscale_neg_dims.RData")
# save(pcoa, file="models/01a_pcoa_cmdscale_17_dims.RData")
# save(pcoa, file="models/01a_pcoa_cmdscale_2000_dims.RData")
save(pcoa, file="models/01a_pcoa_cmdscale_200_dims.RData")
load(file="models/01a_pcoa_cmdscale_2_dims.RData")

head(pcoa$points)
dim(pcoa$points) ## 7246    200
plot(pcoa$points)
pcoa$GOF
pcoa$x
pcoa


eig_pc <- pcoa$eig * 100 / sum(pcoa$eig)
b <- barplot(eig_pc,
             las=1,
             xlab="Dimensions", 
             ylab="Proportion of explained variance (%)", y.axis=NULL,
             col="darkgrey")

plot(cumsum(pcoa$eig) / sum(pcoa$eig),
     type="h", lwd=5, las=1,
     xlab="Number of dimensions",
     ylab=expression(R^2))

plot(pcoa$eig,
       type="h", lwd=5, las=1,
       xlab="Number of dimensions",
       ylab="Eigenvalues")

## calculate the percentage of variation that each pcoa axis accounts for...
mds.var.per <- round(pcoa$eig/sum(pcoa$eig)*100, 1)
mds.var.per

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

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- pcoa$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2])

trait_pca_scores<-cbind(mds.data, site_year_basin)
View(trait_pca_scores)
write.csv(trait_pca_scores, "output_data/01a_trait_pcoa_scores_new_sites_new_ord.csv", row.names = T)


## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(pcoa$eig/sum(pcoa$eig)*100, 1)
mds.var.per


head(mds.data)

ggplot(data=trait_pca_scores, aes(x=X, y=Y, color= Country)) +
  geom_point() +
  theme_bw() +
  xlab(paste("PCoA - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("PCoA - ", mds.var.per[2], "%", sep="")) +
  ggtitle("PCoA plot using Gower distance")


pcoa_vals <- as.data.frame(pcoa$points)
pcoa$ac
class(pcoa)
head(pcoa_vals)
str(pcoa_vals)

pcoa_vals <- as.data.frame(pcoa$points)
pcoa$ac
class(pcoa)
head(pcoa_vals)
str(pcoa_vals)

# x <- pcoa$points[,1]
# y <- pcoa$points[,2]
# plot(x, y, xlab="Axis 1", ylab="Axis 2",
#      main="Metric MDS") ## , type="n"
# text(x, y, labels = row.names(trt_matrix), cex=.7)
# 
# ## graph parameters to make plot pretty - not working!!!
# pca_origin <- biplot.pcoa(pcoa, trt_matrix, habillage=site_year_basin$Country, label="var", geom="point", 
#                        addEllipses = T, ellipse.type='convex', ellipse.alpha=0.01, labelsize=4, col.var="black")
# 
# pca_origin
