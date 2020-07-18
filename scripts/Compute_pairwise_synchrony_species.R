#-------------------------------------------------------------------------------------------------------------------------
### intraspecific synchrony & variability
#-------------------------------------------------------------------------------------------------------------------------
# 1) Select basins: basins w/ more than 10 'high quality' sites, with quality being defined as 80+% completeness) [done prior to the synchrony analyses]
# 2) Select one sampling per year (preference for Autumn)
# 3) Select spp occuring in 2+ sites within basin
# 4) Select spp w/ non-zero abund for >80% of the records within site
# 5) Sites w/ 1+ year of data, fill in NA's with 0 for species
# 6) compute synchrony and variability

#////////////////////////////////////////////////////////////
setwd(".")
#////////////////////////////////////////////////////////////

library(dplyr)
library(synchrony)
library(codyn)
library(rfishbase)
library(tidyr)
library(munfold)
library(data.table)
library(gdata)
#///////////////////////////////////////////////////////////

### Load the data (single data set across countries & basins)
originaldata<-read.csv("fishdata_selection_basins_cleanTaxo.csv")

nlevels(factor(originaldata$MAIN_BAS)) 
nlevels(factor(originaldata$New_names)) 

### loop over basins
basinsID<-unique(originaldata$MAIN_BAS)
Nbasins<-length(unique(originaldata$MAIN_BAS))
  
  synchrony_spp = variability_spp = NULL
  for (basin in 1:length(basinsID)) {
  basindata<-originaldata[originaldata$MAIN_BAS==basinsID[basin],]

  #//////////////////////////////////////////////////////
  #data preparation
  #/////////////////////////////////////////////////////
  ###select only one sampling event per year
  basindata$OP_ID_Site<-paste0(basindata$sYNGEO_ID,"_",basindata$OP_ID)
  datasites<-unique(basindata[,c(1:6,14)])
  datasites$SiteYear<-paste0(datasites$sYNGEO_ID,"_",datasites$Year)
  setorder(datasites,SiteYear,Season)  #reorder table (Autumn first)
  oprem<-datasites$OP_ID_Site[which(duplicated(datasites$SiteYear))] #identify duplicated sampling events per year
  if(length(oprem) > 0){
  basindata<-basindata[-which(basindata$OP_ID_Site %in% oprem),]  #pick only one sampling event per year (Autumn first)
  }
  
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
