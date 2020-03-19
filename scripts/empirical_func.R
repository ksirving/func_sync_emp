# functional synchrony

#  PCoA
install.packages("FD")
library(FD)
library(vegan)
library(reshape)
install.packages("reshape2")
library(reshape2)
#  upload data
#  sites
sites <- read.csv("/Users/katie/Documents/sYNGEO/func_emp/fishdata_selection_basins_same_time_window_cleanTaxo.csv", header=T)
head(sites)

#  traits

traits <- read.csv("/Users/katie/Documents/sYNGEO/func_emp/Imputed_trait_values.csv")
head(traits)
dim(traits)
#  235   9
?functcomp


#  reshape dataframe

head(sites)

dim(sites)
#  columns needed 

length(unique(sites$New_names))
#  208

sites$New_names <- gsub(" ", "_", sites$New_names)
#  need melt or not??

?cast
match_data2 <- subset(sites, New_names %in% spp_list)
head(match_data2)
dim(match_data2)
melt_df <- melt(match_data2, id=c("New_names", "sYNGEO_ID"))
head(melt_df)
sites_df <- cast(melt_df, sYNGEO_ID ~ New_names, fun.aggregate=sum )
# syn_df <- cast(melt_df, sYNGEO_ID ~ Species)
head(sites_df)
dim(sites_df)

length(unique(sites_df$sYNGEO_ID))
head(traits)
traits$X
colnames(sites_df)

spp_names_traits <-   traits$X  %in% colnames(sites_df)
sum(spp_names_traits)
spp_names_traits


match_data <- subset(traits, traits$X  %in% colnames(sites_df))
head(match_data)
dim(match_data)
spp_list <- match_data$X
#  

match_data2 <- subset(sites, New_names %in% spp_list)
head(match_data2)
dim(match_data2)

head(sites_df)
#  fish data base with abundances

head(match_data)
#  trait database with matched species

#  summer or autumn
sum(is.na(sites$Month))

head(sites)

#  summer vs autumn
#  how many sites have both?

library(plyr)
library(dplyr)


sea_count <- sites %>% group_by(sYNGEO_ID, Year) %>% summarise(count = length(unique(Season)))
sea_count$count
length(unique(sea_count$count))

seas_twos <- subset(sea_count, count=="2")
head(seas_twos)
dim(seas_twos)

site_twos <- seas_twos$sYNGEO_ID
site_twos

sub_sites <- subset(sites, sYNGEO_ID %in% site_twos)
head(sub_sites)

dim(sub_sites)

unique(sub_sites$Origin)
length(unique(sub_sites$sYNGEO_ID))

?dist
?vegdist
