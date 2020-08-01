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
library(rfishbase)
library(munfold)
library(data.table)
library(gdata)

originaldata <- read.csv("/Users/katieirving/Documents/sYNGEO/func_emp/data/fishdata_selection_basins_same_time_window_cleanTaxo.csv")
head(originaldata)
## upload pca scores for synchrony

pca_scores <- read.csv("output_data/trait_pca_scores_july2020.csv")
head(pca_scores)

## which axis?
## dimension 1 explains 50.5%
## traits that describe fish size are along this axis, e.g. eggsize, max length, age at maturity etc
## for interpretation, we can describe trait synchrony in terms of fish size in relation to climate anomonlies.
## dimension 2 explains 22.8%
## traits that describe reproduction e.g. reproductive guild, fecundity
## use both axes

## which synchrony?
## site is unit of analysis as pca scores are effectively an index score of that site
## synchrony between sites
## synchrony between basins

## Synchrony between sites
## between sites within basins

## axis 1 (fish size)

## group by basin, change name of syngeo id

pca_basins <- pca_scores %>%
  group_by(MAIN_BAS) %>%
  rename(sYNGEO_ID = sINGEO_ID, 
         site_ID = site) 
pca_basins



### loop over basins
basinsID<-unique(pca_basins$MAIN_BAS) # 46 basins
Nbasins<-length(unique(pca_basins$MAIN_BAS))
basinsID

synchrony_a1 = synchrony_a2 = NULL

basin = 1

for (basin in 1:length(basinsID)) {
  basindata<-pca_basins[pca_basins$MAIN_BAS==basinsID[basin],]
  head(basindata)
  #//////////////////////////////////////////////////////
  #data preparation
  #/////////////////////////////////////////////////////
  
  
  ## select only axis 1 (fish size and put in columns)
  datasites <- basindata %>% 
    select(-c(Axis2,X, site_year, sYNGEO_ID) )%>%
    spread(site_ID, Axis1) #%>%
    
  
  sync_data <- ungroup(datasites)
  sync_data
  
  ## need to remove NAs before correlation
  cor(select(sync_data, 4:27))
  
  
  

  
  
  
  ####select species present in at least two sites
  spp_by_site<-names(which(apply(ifelse(table(basindata$New_names,basindata$sYNGEO_ID) >0,1,0),1,sum)>=2))
  
  # now subset the matrix by 'widespread enough' species
  alldata<-basindata[which(basindata$New_names %in% spp_by_site),]
  
  ####select species present in 80% of the years/site combinaison
  # create abundance matrix per year
  alldata <- alldata %>%
    group_by(sYNGEO_ID,New_names,Year) %>%
    summarize(Abundance=sum(Abundances))
  
  # calculate (temporal) prevalence per site (filter species present in at least 80% sampling events)
  timserieslength<-length(unique(alldata$Year))
  
  spp_consistency<- alldata %>%
    group_by(sYNGEO_ID, New_names) %>%
    summarize(prop.occurrence=length(unique(Year))/timserieslength) %>%
    subset(prop.occurrence>=.8) 
  
  #create matrix
  spp_consistency$Speciescombination<-paste0(spp_consistency$sYNGEO_ID,spp_consistency$New_names) # create spp_I x Species label
  alldata$sYNGEO_IDxSpeciescombination<-paste0(alldata$sYNGEO_ID,alldata$New_names) # Same on the other side
  alldata<- alldata[alldata$sYNGEO_IDxSpeciescombination %in% spp_consistency$Speciescombination,]
  alldata<-alldata[,-which(names(alldata) == "sYNGEO_IDxSpeciescombination")]
  alldata$New_names<-factor(alldata$New_names)
  
  # Now check again for spp that after the previous filtering have been left in a single site
  remspp<-names(which(apply(ifelse(table(alldata$New_names,alldata$sYNGEO_ID) >0,1,0),1,sum)<2))
  if(length(remspp) > 0){
    alldata<-alldata[-which(alldata$New_names %in% remspp),]
  }
  
  #///////////////////////////////////////////////////////
  #compute intraspecific synchrony & variability
  #///////////////////////////////////////////////////////    
  ### loop over species
  Nspecies<-length(unique(alldata$New_names))
  for (spp in 1:Nspecies) {
    spp_data<-alldata[alldata$New_names==unique(alldata$New_names)[spp],]
    spp_data<-t(spread(spp_data, sYNGEO_ID, Abundance))[-c(1:2),]
    sitesIDs<-rownames(spp_data)
    spp_data<-apply(spp_data, 2, as.numeric)
    rownames(spp_data)<-sitesIDs
    
    ###intraspecific variability
    cv<-apply(spp_data,1,sd,na.rm=T)/apply(spp_data,1,mean,na.rm=T)
    sYNGEO_ID = names(cv)
    variability_spp<-rbind(variability_spp, cbind(basinsID[basin], 
                                                  as.character(unique(alldata$New_names)[spp]),
                                                  cv,sYNGEO_ID))
    ###intraspecific synchrony
    correlation<-cor(t(spp_data), use = "pairwise.complete.obs")
    vector_data_correl<- unmatrix(correlation,byrow=F)
    lower_triangle<-lower.tri(correlation)
    vector_data_triangle<-unmatrix(lower_triangle, byrow=F)
    correl_result<-vector_data_correl[vector_data_triangle]
    sYNGEO_ID1<-sapply(strsplit(names(correl_result),":"),'[',1)
    sYNGEO_ID2<-sapply(strsplit(names(correl_result),":"),'[',2)
    synchrony_spp<-rbind(synchrony_spp, cbind(basinsID[basin], 
                                              as.character(unique(alldata$New_names)[spp]),
                                              correl_result,sYNGEO_ID1,sYNGEO_ID2))
  }
}
synchrony_spp<-data.frame(synchrony_spp)
colnames(synchrony_spp)<-c("basin_ID", "Species", "Correlation","sYNGEO_ID1","sYNGEO_ID2")
nlevels(synchrony_spp$basin_ID) 
nlevels(synchrony_spp$Species) 

variability_spp<-data.frame(variability_spp)
colnames(variability_spp)<-c("basin_ID", "Species", "CV","sYNGEO_ID")
nlevels(variability_spp$basin_ID) 
nlevels(variability_spp$Species) 

###save results
write.csv(variability_spp, "results_intraspecific_variability.csv")
write.csv(synchrony_spp, "results_intraspecific_synchrony.csv")


