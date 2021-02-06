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

## upload pca scores for synchrony

pca_scores <- read.csv("output_data/01_trait_pca_scores_new_sites.csv")

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

  
  for (ax in 1: Naxis) {
    
    axis_data<-basindata_melt[basindata_melt$axis==unique(basindata_melt$axis)[ax],]
    years <- unique(sort(axis_data$year)) ## define years for columns

    # make df wide
    axis_data  <- axis_data %>% 
      dplyr::select(-c(site_year) ) %>%
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
    head(correlation)
    vector_data_correl<- unmatrix(correlation,byrow=F)
    lower_triangle<-lower.tri(correlation)
    vector_data_triangle<-unmatrix(lower_triangle, byrow=F)
    correl_result<-vector_data_correl[vector_data_triangle]
    site_ID1<-sapply(strsplit(names(correl_result),":"),'[',1)
    site_ID2<-sapply(strsplit(names(correl_result),":"),'[',2)
    correl_result
    synchrony_axis<-rbind(synchrony_axis, cbind(basinsID[basin], 
                                              as.character(unique(basindata_melt$axis)[ax]),
                                              basindata_melt$Country[1],
                                              correl_result,site_ID1,site_ID2))
  }
}
basinsID[basin]
basindata_melt$Country
synchrony_axis
warnings()
synchrony_axis<-data.frame(synchrony_axis)
colnames(synchrony_axis)<-c("basin_ID", "Axis","Country", "Correlation","Site_ID1","Site_ID2")
nlevels(factor(synchrony_axis$basin_ID)) # 43
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
basinsID
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
  
  
  basindata_melt <- basindata  %>%
    reshape2::melt(id= c("site_year", "HydroBasin", "Country", "site", "year")) %>%
    rename(axis=variable, score = value)
  
  
  #///////////////////////////////////////////////////////
  #compute btween site synchrony
  #///////////////////////////////////////////////////////    
  
  ### loop over species
  Naxis<-length(unique(basindata_melt$axis))

  
  ## loop over axis
  for (ax in 1: Naxis) {
    axis_data<-basindata_melt[basindata_melt$axis==unique(basindata_melt$axis)[ax],]
    years <- unique(sort(axis_data$year)) ## define years for columns
    # head(axis_data)
    # make df wide
    axis_data  <- axis_data %>% 
      dplyr::select(-c(site_year) ) %>%
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
                                                basindata_melt$Country[1],
                                                correl_result,site_ID1,site_ID2, year_removed))
    synchrony_axis
    }
    
  }
  synchrony_axis<-data.frame(synchrony_axis)
  colnames(synchrony_axis)<-c("basin_ID", "Axis","Country", "Correlation","Site_ID1","Site_ID2", "year_removed")
  save(synchrony_axis, file=paste0("output_data/01_", paste(basinsID[basin]), "_bet_site_sync_one_out.RData", sep=""))
  rm(synchrony_axis)
}



load(file= "/Users/katieirving/Documents/git/func_sync_emp/output_data/01_7080047060_bet_site_sync_one_out.RData")
  head(synchrony_axis)

### between basin synchrony
  
  originaldata <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
  head(originaldata)
  
  ## upload pca scores for synchrony
  
  pca_scores <- read.csv("output_data/01_trait_pca_scores_new_sites.csv")

  ## centroid of basin
  
  head(pca_scores)
  
  pca_means <- pca_scores %>%
    group_by(HydroBasin, Axis, year, Country) %>%
    summarise(BasinCentroidYear = mean(Score))
  
  head(pca_means)
  
  ### between basin synchrony
  
  CountryID<-unique(pca_means$Country) # 7 basins
  CountryID <- CountryID[-c(3,5)] ## remove UK and Finland as they only have 1 basin
  # Nbasins<-length(unique(pca_basins$HydroBasin))
  
  nlevels(factor(pca_means$Country)) 
  synchrony_axis = NULL
  country 
  
  for (country in 1:length(CountryID)) {
    countrydata<-pca_means[pca_means$Country==CountryID[country],]
    
    #//////////////////////////////////////////////////////
    #data preparation
    #/////////////////////////////////////////////////////
    
    # countrydata_melt <- countrydata  %>%
    #   reshape2::melt(id= c( "Country", "HydroBasin", "year")) %>%
    #   rename(axis=variable, score = value)
    # head(countrydata_melt)
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
    Naxis<-length(unique(countrydata$Axis))
    
    ax
    for (ax in 1: Naxis) {
      
      axis_data<-countrydata[countrydata$Axis==unique(countrydata$Axis)[ax],]
      years <- unique(sort(axis_data$year)) ## define years for columns
      
      # make df wide
      axis_data  <- axis_data %>% 
        # dplyr::select(-c(site_year) ) %>%
        spread(HydroBasin, BasinCentroidYear) #%>%
      # head(axis_data)
      # flip data
      axis_data <- t(axis_data)[-c(1:3),]
      
      # define rows and column names and convert to numbers
      sitesIDs<-rownames(axis_data)
      colnames(axis_data) <- years
      axis_data<-apply(axis_data, 2, as.numeric)
      rownames(axis_data)<-sitesIDs
      
      # change NAs to zero - can change later if needed
      axis_data[which(is.na(axis_data))] <- 0
      
      ### synchrony
      correlation<-cor(t(axis_data), use = "pairwise.complete.obs")
      head(correlation)
      vector_data_correl<- unmatrix(correlation,byrow=F)
      lower_triangle<-lower.tri(correlation)
      vector_data_triangle<-unmatrix(lower_triangle, byrow=F)
      correl_result<-vector_data_correl[vector_data_triangle]
      Basin_ID1<-sapply(strsplit(names(correl_result),":"),'[',1)
      Basin_ID2<-sapply(strsplit(names(correl_result),":"),'[',2)
      correl_result
      synchrony_axis<-rbind(synchrony_axis, cbind(CountryID[country], 
                                                  as.character(unique(countrydata$Axis)[ax]),
                                                  correl_result,Basin_ID1,Basin_ID2))
    }
  }
  
  synchrony_axis<-data.frame(synchrony_axis)
  head(synchrony_axis)
  colnames(synchrony_axis)<-c("Country", "Axis", "Correlation","BasinID1","BasinID2")

  write.csv(synchrony_axis, "output_data/02_between_basin_sync_per_country.csv")  
  
  # between basin synchrony, whole data
  ## upload pca scores for synchrony
  
  pca_scores <- read.csv("output_data/01_trait_pca_scores_new_sites.csv")
  pca_scores <- pca_scores %>%
    pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Score")
  
  ## centroid of basin
  
  head(pca_scores)
  
  pca_means <- pca_scores %>%
    group_by(HydroBasin, Axis, year, Country) %>%
    summarise(BasinCentroidYear = mean(Score))
  
  head(pca_means)
  
  synchrony_axis = NULL
  
  
    Naxis<-length(unique(pca_means$Axis))
    Naxis
    # ax=1
    for (ax in 1: Naxis) {
      
      axis_data<-pca_means[pca_means$Axis==unique(pca_means$Axis)[ax],]
      years <- unique(sort(axis_data$year)) ## define years for columns
      
      # make df wide
      axis_data  <- axis_data %>% 
        dplyr::select(-c(Country) ) %>%
        spread(HydroBasin, BasinCentroidYear) #%>%
      head(axis_data)
      # flip data
      axis_data <- t(axis_data)[-c(1:3),]
      
      # define rows and column names and convert to numbers
      sitesIDs<-rownames(axis_data)
      colnames(axis_data) <- years
      axis_data<-apply(axis_data, 2, as.numeric)
      rownames(axis_data)<-sitesIDs
      
      # change NAs to zero - can change later if needed
      axis_data[which(is.na(axis_data))] <- 0
      
      ### synchrony
      correlation<-cor(t(axis_data), use = "pairwise.complete.obs")
      head(correlation)
      vector_data_correl<- unmatrix(correlation,byrow=F)
      lower_triangle<-lower.tri(correlation)
      vector_data_triangle<-unmatrix(lower_triangle, byrow=F)
      correl_result<-vector_data_correl[vector_data_triangle]
      Basin_ID1<-sapply(strsplit(names(correl_result),":"),'[',1)
      Basin_ID2<-sapply(strsplit(names(correl_result),":"),'[',2)
      correl_result
      synchrony_axis<-rbind(synchrony_axis, cbind(as.character(unique(pca_means$Axis)[ax]),
                                                  correl_result,Basin_ID1,Basin_ID2))
    }
  
    synchrony_axis<-data.frame(synchrony_axis)
    head(synchrony_axis)
    dim(synchrony_axis)
    colnames(synchrony_axis)<-c("Axis", "Correlation","BasinID1","BasinID2")
    
    write.csv(synchrony_axis, "output_data/02_between_basin_sync_all_together.csv")  
    