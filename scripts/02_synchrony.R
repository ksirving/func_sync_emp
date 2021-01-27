## synchrony

## packages
library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
# install.packages("synchrony")
# install.packages("codyn")
# install.packages("rfishbase")
# install.packages("munfold")
# install.packages("gdata")
library(synchrony)
library(codyn)
# library(rfishbase)
library(munfold)
library(data.table)
library(gdata)

getwd()

originaldata <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
head(originaldata)

unique(originaldata$Year)
y = subset(originaldata, Year == 2004)
unique(y$HydroBasin) ## 43
unique(originaldata$HydroBasin) ## 45
## upload pca scores for synchrony

pca_scores <- read.csv("output_data/trait_pca_scores_new_sites.csv")

## which axis?
## dimension 1 explains 54.4%
## traits that describe fish size are along this axis, e.g. eggsize, max length, age at maturity etc
## for interpretation, we can describe trait synchrony in terms of fish size in relation to climate anomonlies.


## which synchrony?
## site is unit of analysis as pca scores are effectively an index score of that site
## synchrony between sites
## synchrony between basins

## Synchrony between sites
## between sites within basins

## axis 1 (fish size)

## group by basin, change name of syngeo id

pca_basins <- pca_scores %>%
  group_by(HydroBasin) %>%
  rename(site_year = X)

# unique(pca_basins$year)
# y = subset(pca_basins, year == 2004)
# unique(y$HydroBasin) ## 43
# unique(pca_basins$HydroBasin)
### loop over basins
basinsID<-unique(pca_basins$HydroBasin) # 45 basins
Nbasins<-length(unique(pca_basins$HydroBasin))

nlevels(factor(pca_basins$HydroBasin)) 
synchrony_axis = NULL
basin = 13

for (basin in 1:length(basinsID)) {
  basindata<-pca_basins[pca_basins$HydroBasin==basinsID[basin],]

  #//////////////////////////////////////////////////////
  #data preparation
  #/////////////////////////////////////////////////////

  basindata_melt <- basindata  %>%
    reshape2::melt(id= c("site_year", "HydroBasin", "Country", "site", "year")) %>%
    rename(axis=variable, score = value)
 
  ## test the below to see if needed
  
  # length(unique(basindata_melt$site_ID))
  # calculate (temporal) prevalence per site (filter scores present in at least 80% site/years)
  # timserieslength<-length(unique(basindata$year))
  
  # site_consistency<- basindata_melt %>%
  #   group_by(sYNGEO_ID, axis) %>%
  #   summarize(prop.occurrence=length(unique(year))/timserieslength) %>%
  #   subset(prop.occurrence>=.8) 
  # 
  # basindata_melt$site_ID %in% site_consistency$sYNGEO_ID ## check all match
  # 

  #///////////////////////////////////////////////////////
  #compute btween site synchrony
  #///////////////////////////////////////////////////////    
  ### loop over axis
  Naxis<-length(unique(basindata_melt$axis))

  ax=1
  
  for (ax in 1: Naxis) {
    
    axis_data<-basindata_melt[basindata_melt$axis==unique(basindata_melt$axis)[ax],]
    years <- unique(sort(axis_data$year)) ## define years for columns

    # make df wide
    axis_data  <- axis_data %>% 
      select(-c(site_year) ) %>%
      spread(site, score) #%>%

    # flip data
      axis_data <- t(axis_data)[-c(1:4),]
    
    # define rows and column names and convert to numbers
    sitesIDs<-rownames(axis_data)
    colnames(axis_data) <- years
    axis_data<-apply(axis_data, 2, as.numeric)
    rownames(axis_data)<-sitesIDs
   
    # change NAs to zero - can change later if needed
    axis_data[which(is.na(axis_data))] <- 0

    ### synchrony
    correlation<-cor(t(axis_data), use = "pairwise.complete.obs")
    vector_data_correl<- unmatrix(correlation,byrow=F)
    lower_triangle<-lower.tri(correlation)
    vector_data_triangle<-unmatrix(lower_triangle, byrow=F)
    correl_result<-vector_data_correl[vector_data_triangle]
    site_ID1<-sapply(strsplit(names(correl_result),":"),'[',1)
    site_ID2<-sapply(strsplit(names(correl_result),":"),'[',2)
    correl_result
    synchrony_axis<-rbind(synchrony_axis, cbind(basinsID[basin], 
                                              as.character(unique(basindata_melt$axis)[ax]),
                                              correl_result,site_ID1,site_ID2))
  }
}

synchrony_axis
warnings()
synchrony_axis<-data.frame(synchrony_axis)
colnames(synchrony_axis)<-c("basin_ID", "Axis", "Correlation","Site_ID1","Site_ID2")
nlevels(factor(synchrony_axis$basin_ID)) # 45
nlevels(factor(synchrony_axis$Axis)) # 2


###save results
write.csv(synchrony_axis, "output_data/02_results_between_site_synchrony_Jan2020.csv")
# synchrony_axis <- read.csv("output_data/02_results_between_site_synchrony.csv")
synchrony_axis
## plot synchrony


# Leaving one out synchrony -----------------------------------------------


## synchrony leaving one year out

### loop over basins
basinsID<-unique(pca_basins$HydroBasin) # 46 basins
Nbasins<-length(unique(pca_basins$HydroBasin))
basinsID[basin]
nlevels(factor(pca_basins$HydroBasin)) 
# synchrony_axis = NULL

library(foreach)
library(doParallel)
detectCores()
# install.packages("doParallel")


##############################
##### parallels not working!!!!!

for (basin in 1:length(basinsID)) {
  synchrony_axis = NULL
  basindata<-pca_basins[pca_basins$HydroBasin==basinsID[basin],]
  
  #//////////////////////////////////////////////////////
  #data preparation
  #/////////////////////////////////////////////////////
  
  
  basindata_melt <- basindata %>% select(-X) %>%
    reshape2::melt(id= c("site_year", "site_ID", "HydroBasin", "origin", "sYNGEO_ID", "year")) %>%
    rename(axis=variable, score = value)
  
  
  #///////////////////////////////////////////////////////
  #compute btween site synchrony
  #///////////////////////////////////////////////////////    
  
  ### loop over species
  Naxis<-length(unique(basindata_melt$axis))
  # ax <- 1
  
  ## loop over axis
  for (ax in 1: Naxis) {
    axis_data<-basindata_melt[basindata_melt$axis==unique(basindata_melt$axis)[ax],]
    years <- unique(sort(axis_data$year)) ## define years for columns
    
    # make df wide
    axis_data  <- axis_data %>% 
      select(-c(site_year, sYNGEO_ID) ) %>%
      spread(site_ID, score) #%>%
    # flip data
    axis_data <- t(axis_data)[-c(1:4),]
    
    # define rows and column names and convert to numbers
    sitesIDs<-rownames(axis_data)
    colnames(axis_data) <- years
    axis_data<-apply(axis_data, 2, as.numeric)
    rownames(axis_data)<-sitesIDs
    
    # change NAs to zero - can change later if needed
    axis_data[which(is.na(axis_data))] <- 0
  
    ### synchrony - leaving one out
    ## loop over years
    for (y in 1: length(years)) {
    
    year_removed <- paste(years[y])
    axis_data_reduced <- axis_data[,-y] 
    correlation<-cor(t(axis_data_reduced), use = "pairwise.complete.obs")
    vector_data_correl<- unmatrix(correlation,byrow=F)
    lower_triangle<-lower.tri(correlation)
    vector_data_triangle<-unmatrix(lower_triangle, byrow=F)
    correl_result<-vector_data_correl[vector_data_triangle]
    site_ID1<-sapply(strsplit(names(correl_result),":"),'[',1)
    site_ID2<-sapply(strsplit(names(correl_result),":"),'[',2)
    correl_result
    synchrony_axis<-rbind(synchrony_axis, cbind(basinsID[basin], 
                                                as.character(unique(basindata_melt$axis)[ax]),
                                                correl_result,site_ID1,site_ID2, year_removed))
    synchrony_axis
    }
    
  }
  synchrony_axis<-data.frame(synchrony_axis)
  colnames(synchrony_axis)<-c("basin_ID", "Axis", "Correlation","sYNGEO_ID1","sYNGEO_ID2", "year_removed")
  save(synchrony_axis, file=paste0("output_data/", paste(basinsID[basin]), "_bet_site_sync_one_out.RData", sep=""))
  rm(synchrony_axis)
}

  stopCluster(cl)
  warnings()
# warnings()
# synchrony_axis
dim(synchrony_axis) #19984257        6 - 1:28 basins
dim(synchrony_axis) #18561158         29:46
synchrony_axis<-data.frame(synchrony_axis)
colnames(synchrony_axis)<-c("basin_ID", "Axis", "Correlation","sYNGEO_ID1","sYNGEO_ID2", "year_removed")
# nlevels(factor(synchrony_axis$basin_ID)) # 28
# nlevels(factor(synchrony_axis$Axis)) # 2
# nlevels(factor(synchrony_axis$year_removed)) # 11
# tail(synchrony_axis)
# head(synchrony_axis)

###save results
# save(synchrony_axis, file = "output_data/results_between_site_synchrony_1-28.RData") ## 1-28 basins
# load(file = "output_data/results_between_site_synchrony_1-28.RData")


save(synchrony_axis, file = "output_data/results_between_site_synchrony_29-46.RData") ## 29-46 basins

# save(synchrony_axis, file = paste0("output_data/results_between_site_synchrony", basinsID[basin],  ".RData"))

load(file = "output_data/results_between_site_synchrony_1-28.RData")

## split data files by basin

basinIDs <- unique(synchrony_axis$basin_ID)

for (b in 1: length(basinIDs)) {
  
  basin <- subset(synchrony_axis, basin_ID == basinIDs[b])
  basin <- na.omit(basin)
  
  save(basin, file=paste0("output_data/basins", paste(basinIDs[b], "_bet_site_sync_one_out.RData", sep="")))
  
}

rm(synchrony_axis)

load(file = "output_data/results_between_site_synchrony_29-46.RData")
## split data files by basin

basinIDs <- unique(synchrony_axis$basin_ID)

for (b in 1: length(basinIDs)) {
  
  basin <- subset(synchrony_axis, basin_ID == basinIDs[b])
  basin <- na.omit(basin)
  
  save(basin, file=paste0("output_data/basins", paste(basinIDs[b], "_bet_site_sync_one_out.RData", sep="")))
  
  
}
