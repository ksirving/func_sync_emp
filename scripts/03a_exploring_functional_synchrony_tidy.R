## exploratory analysis

library(tidyverse)
library(tidylog)

out.dir <- "/Users/katieirving/Documents/git/func_sync_emp/Figures/"

## questions
## How does it vary through space?
## What are the main drivers? - come back to this!

## how does it vary through space?
## are traits synchronous within basins?
## are traits synchronous between basins?
## what is the trajectory of synchrony over time?

## workflow
## look at trait synchrony (and/or PCA scores) in each basin - plot trajectory
## plot synchrony as a function of distance (euclidean for now)
## get species richness per site
## plot synchrony as a function of species richness
## how to plot trajectory globally?
## calculate between basin synchrony (centroid of basin)


## upload synchrony data
## all synchrony
sync <- read.csv("output_data/02_results_between_site_synchrony_Jan2020.csv")
## leave one out synchrony - one basin

## sync values with distances

SimSync <- read.csv( "output_data/04_sync_data_similarity_euclidean_dist.csv")
head(SimSync)

str(SimSync)


# Overall Synchrony within basin (between sites) -------------------------------------------------------


SimSync$basin_ID <- as.factor(SimSync$basin_ID)

# SimSync1 <- SimSync %>%
#   filter(Axis == "Axis1")

## figure shows fish size and reproduction overall synchrony in each basin
b1 = ggplot(SimSync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  # theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~Country, scale="free") 
b1
file.name1 <- paste0(out.dir, "Between_site_sync_per_country.jpg")
ggsave(b1, filename=file.name1, dpi=300, height=5, width=6)

## how many basins in each country

basin_tally <- SimSync %>% group_by(Country) %>% count(basin_ID)
basin_tally

## single country examples

Fra_sync <- SimSync %>%
  filter(Country == "FRA")

b1 <- ggplot(Fra_sync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# scale_color_brewer(breaks = c("Axis1", "Axis2"),
#                    palette="Set1",
#                    labels = c("Fish Size", "Fish Reproductive Type"),
#                    name = "Functional Trait") 
b1

file.name1 <- paste0(out.dir, "France_between_site_sync.jpg")
ggsave(b1, filename=file.name1, dpi=300, height=5, width=6)


SWE_sync <- SimSync %>%
  filter(Country == "SWE")

b2 <- ggplot(SWE_sync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
b2

file.name1 <- paste0(out.dir, "Sweden_between_site_sync.jpg")
ggsave(b2, filename=file.name1, dpi=300, height=5, width=6)
## leave one out sync - does not work!!!


# Synchrony with distance -------------------------------------------------

## mean synchrony per site with distance simialrity

head(SimSync)

mean_sync <- SimSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity))

s1<- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))### this is nice, how to get sync values between 0 and 1?
s1
file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_similarity.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)

head(mean_sync)

## mean sync per site with temp cor

TempSync <- read.csv("output_data/02_temp_sync_between_sites.csv")

TempSync <- rename(TempSync, Pair = X, TempCor = Correlation)

## make sync values wider and format for join

SimSyncx <- SimSync %>%
  select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

TempSync <- TempSync %>%
  filter(Env_Var == "clim_max_raw")
head(TempSync)

AllSync <- full_join(TempSync[, c(1,3,5)], SimSyncx, by = "Pair")
AllSync <- na.omit(AllSync)

AllSync <- AllSync %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation")

# unique(AllSync$Axis)

mean_sync <- AllSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity), Mean_TempCor = mean(TempCor))
unique(mean_sync$Axis)

s1<- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))### this is nice, how to get sync values between 0 and 1?
s1
file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_similarity.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)


s3<- ggplot(mean_sync, aes(x=Mean_TempCor, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Temp Synchrony", limits=c(-1, 1))### this is nice, how to get sync values between 0 and 1?
s3

file.name1 <- paste0(out.dir, "Mean_between_site_sync_temp_sync_all_sites.jpg")
ggsave(s3, filename=file.name1, dpi=300, height=5, width=6)

##################################
## mean synchrony with raw distance
##################################

mean_sync <- SimSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters), Mean_Lat =mean(MeanLat))
head(mean_sync)
range(mean_sync$Mean_Dist)

s1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Axis)) + ### this is nice
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)", limits=c(0, 800000)) +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
  
s1
file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_raw.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)


s2<- ggplot(mean_sync, aes(x=Mean_Lat, y=Mean_Cor, color = Axis)) + ### this is nice
  geom_point() +
  scale_x_continuous(name="Mean Latitude") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))

s2
file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_latitude.jpg")
ggsave(s2, filename=file.name1, dpi=300, height=5, width=6)

head(mean_sync)

## mean sync per site with temp cor

TempSync <- read.csv("output_data/02_temp_sync_between_sites.csv")

TempSync <- rename(TempSync, Pair = X, TempCor = Correlation)

## make sync values wider and format for join

SimSyncx <- SimSync %>%
  select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

TempSync <- TempSync %>%
  filter(Env_Var == "clim_max_raw")
head(TempSync)

AllSync <- full_join(TempSync[, c(1,3,5)], SimSyncx, by = "Pair")
AllSync <- na.omit(AllSync)

AllSync <- AllSync %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation")

# unique(AllSync$Axis)

mean_sync <- AllSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters), Mean_TempCor = mean(TempCor))
unique(mean_sync$Axis)

s1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Axis)) + 
  geom_point() +### this is nice, how to get sync values between 0 and 1?
  scale_x_continuous(name="Mean Eucliean Distance (m)", limits=c(0, 800000)) +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
s1
file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_raw.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)

s2<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_TempCor)) +
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)", limits=c(0, 800000)) +
  scale_y_continuous(name="Mean Temperature Synchrony")
  
s2

file.name1 <- paste0(out.dir, "Mean_between_site_temp_sync_all_sites_distance_raw.jpg")
ggsave(s2, filename=file.name1, dpi=300, height=5, width=6)

## mean sync per site with flow cor

FlowSync <- read.csv("output_data/02_flow_sync_between_sites.csv")

FlowSync <- rename(FlowSync, Pair = X, FlowCor = Correlation)
unique(FlowSync$Env_Var)
## make sync values wider and format for join

SimSyncx <- SimSync %>%
  select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

FlowSync <- FlowSync %>%
  filter(Env_Var == "qmax_raw")
head(FlowSync)
?gsub
FlowSync$Pair <- gsub(".1", "",FlowSync$Pair, fixed = T )

AllSync <- full_join(FlowSync[, c(1,3,5)], SimSyncx, by = "Pair") ### some didn't match, fix this later!!!!
AllSync <- na.omit(AllSync)

AllSync <- AllSync %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation")

# unique(AllSync$Axis)

mean_sync <- AllSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters), Mean_FlowCor = mean(FlowCor))
unique(mean_sync$Axis)

s1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Axis)) + 
  geom_point() +### this is nice, how to get sync values between 0 and 1?
  scale_x_continuous(name="Mean Eucliean Distance (m)", limits=c(0, 800000)) +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
s1
file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_raw.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)

s2<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_FlowCor)) +
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)", limits=c(0, 800000)) +
  scale_y_continuous(name="Mean Flow Synchrony")

s2

file.name1 <- paste0(out.dir, "Mean_between_site_flow_sync_all_sites_distance_raw.jpg")
ggsave(s2, filename=file.name1, dpi=300, height=5, width=6)


# Between Basin Synchrony -------------------------------------------------

## plot between basin synchrony

# BasinSync <- read.csv("output_data/02_between_basin_sync_per_country.csv")
AllSync <- read.csv("output_data/02_between_basin_sync_all_together.csv")

# head(BasinSync)
# 
# b3 <- ggplot(BasinSync, aes(x=Country, y=Correlation, fill=Axis)) + 
#   geom_boxplot() +
#   facet_wrap(~Axis, scale="free") 
# b3
# file.name1 <- paste0(out.dir, "Between_basin_sync_per_country.jpg")
# ggsave(b3, filename=file.name1, dpi=300, height=5, width=6)

head(AllSync)

### basin synchrony and similarity

CountriesDFx <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist.csv")
head(CountriesDFx)

mean_sync <- CountriesDFx %>%
  group_by(Axis, BasinID2, Country) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity))

c1 <- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
c1

file.name1 <- paste0(out.dir, "Mean_basin_sync_per_country.jpg")
ggsave(c1, filename=file.name1, dpi=300, height=5, width=6)

c2 <- ggplot(CountriesDFx, aes(x=Similarity, y=Correlation, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
c2

file.name1 <- paste0(out.dir, "Raw_basin_sync_per_country.jpg")
ggsave(c2, filename=file.name1, dpi=300, height=5, width=6)

filter(mean_sync, Mean_Sim <=0.25)
filter(mean_sync, Mean_Sim >=0.75)

### basin synchrony and similarity (not binned by country)

allDFx <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist_all_together.csv")
head(allDFx)

mean_sync <- allDFx %>%
  group_by(Axis, BasinID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity))

g1 <- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))### this is nice, how to get sync values between 0 and 1?
g1

file.name1 <- paste0(out.dir, "Mean_basin_sync_global.jpg")
ggsave(g1, filename=file.name1, dpi=300, height=5, width=6)

ggplot(allDFx, aes(x=Similarity, y=Correlation, color = Axis)) + 
  geom_point()

filter(mean_sync, Mean_Sim <=0.25)
filter(mean_sync, Mean_Sim >=0.75)


##############################
## raw distance 
############################
head(CountriesDFx)
mean_sync <- CountriesDFx %>%
  group_by(Axis, BasinID2, Country) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters))

c1 <- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
  
c1

file.name1 <- paste0(out.dir, "Mean_basin_sync_per_country_mean_distance_raw.jpg")
ggsave(c1, filename=file.name1, dpi=300, height=5, width=6)

c2 <- ggplot(CountriesDFx, aes(x=Euclid_Dist_Meters, y=Correlation, color = Axis)) + 
  geom_point() +
  scale_x_continuous(name="Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
c2

file.name1 <- paste0(out.dir, "Raw_basin_sync_per_country_distance_raw.jpg")
ggsave(c2, filename=file.name1, dpi=300, height=5, width=6)

# filter(mean_sync, Mean_D <=0.25)
# filter(mean_sync, Mean_Sim >=0.75)

### basin synchrony and similarity (not binned by country)

allDFx <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist_all_together.csv")
head(allDFx)

mean_sync <- allDFx %>%
  group_by(Axis, BasinID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters))

g1 <- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Axis)) + 
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))
g1

file.name1 <- paste0(out.dir, "Mean_basin_sync_global)raw_distance.jpg")
ggsave(g1, filename=file.name1, dpi=300, height=5, width=6)

ggplot(allDFx, aes(x=Euclid_Dist_Meters, y=Correlation, color = Axis)) + 
  geom_point()

filter(mean_sync, Mean_Sim <=0.25)
filter(mean_sync, Mean_Sim >=0.75)

### temp basin sync
TempSync <- read.csv("output_data/02_between_basin_sync_temp_all_together.csv")

FlowSync <- read.csv("output_data/02_between_basin_sync_flow_all_together.csv")
head(TempSync)
head(FlowSync)

allDFx <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist_all_together.csv")
head(allDFx)

## format dfs
TempSync <- TempSync %>%
  dplyr::select(-X) %>%
  filter(EnvVar == "clim_max_raw") %>%
  rename(TempCor = Correlation) %>%
  unite("Pair", BasinID1:BasinID2, remove = F, sep= ".") %>%
  distinct()

FlowSync <- FlowSync %>%
  dplyr::select(-X) %>%
  filter(EnvVar == "qmax_raw") %>%
  rename(FlowCor = Correlation) %>%
  unite("Pair", BasinID1:BasinID2, remove = F, sep= ".") %>%
  distinct()


allDFx <- allDFx %>%
  dplyr::select( -X, -BasinPairs) %>%
  rename(BasinCor = Correlation) %>%
  pivot_wider(names_from = "Axis", values_from = BasinCor) %>%
    unite("Pair", BasinID1:BasinID2, remove = F, sep= ".")


head(allDFx)
head(TempSync)
head(FlowSync)

## join DFs
AllSync <- full_join(TempSync[, c(2,3)], allDFx, by = "Pair")
AllSync <- full_join(FlowSync[, c(2,3)], AllSync, by = "Pair")
AllSync <- na.omit(AllSync)
head(AllSync)

AllSync <- AllSync %>%
  pivot_longer(c(Axis1:Axis2, FlowCor, TempCor), names_to = "Variable", values_to = "Correlation") #%>%
  # dplyr::select(Euclid_Dist_Meters, MeanLat, Variable, Correlation)

## figures raw sync values (not summarised)
names(AllSync)

b1 <- ggplot(AllSync, aes(x=Euclid_Dist_Meters, y=Correlation, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Euclid_Dist_Meters") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
b1

b2 <- ggplot(AllSync, aes(x=MeanLat, y=Correlation, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="MeanLat") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
b2


head(AllSync)

AllSyncWide <- AllSync %>%
  pivot_wider(names_from = "Variable", values_from = "Correlation")

head(AllSyncWide)
names(AllSyncWide)
mean_sync <- AllSync %>%
  group_by(Variable, BasinID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity), 
            Mean_Dist = mean(Euclid_Dist_Meters), Mean_Lat = mean(MeanLat))
head(mean_sync)

b1 <- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Mean Euclid Dist Meters") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
b1

file.name1 <- paste0(out.dir, "Mean_basin_sync_func_temp_flow_global_raw_distance.jpg")
ggsave(b1, filename=file.name1, dpi=300, height=5, width=6)

b2 <- ggplot(mean_sync, aes(x=Mean_Lat, y=Mean_Cor, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Mean Lat") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
b2

file.name1 <- paste0(out.dir, "Mean_basin_sync_func_temp_flow_global_mean_lat.jpg")
ggsave(b2, filename=file.name1, dpi=300, height=5, width=6)

# All data one figure -----------------------------------------------------

## create bi plot with site synchrony & mean latitude
## and biplot with basin synchrony and mean latitude

## data

## between site
SimSync <- read.csv( "output_data/04_sync_data_similarity_euclidean_dist.csv")

## format synchrony
SimSyncx <- SimSync %>%
  dplyr::select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

## upload and format temp
TempSync <- read.csv("output_data/02_temp_sync_between_sites.csv")
TempSync <- rename(TempSync, Pair = X, TempCor = Correlation)
head(TempSync)
dim(TempSync)

TempSync <- TempSync %>%
  filter(Env_Var == "clim_max_raw")
head(TempSync)

## mean sync per site with flow cor

FlowSync <- read.csv("output_data/02_flow_sync_between_sites.csv")

FlowSync <- rename(FlowSync, Pair = X, FlowCor = Correlation)
unique(FlowSync$Env_Var)

FlowSync <- FlowSync %>%
  filter(Env_Var == "qmax_raw")
head(FlowSync)
# ?gsub
FlowSync$Pair <- gsub(".1", "",FlowSync$Pair, fixed = T )
 
## join dfs
AllSync <- full_join(TempSync[, c(1,3,5)], SimSyncx, by = "Pair")
AllSync <- full_join(FlowSync[, c(1,3,5)], AllSync, by = "Pair") ### again here, 51 site pairs didn't match, fix later!!!
AllSync <- na.omit(AllSync)
dim(AllSync)
names(AllSync)
head(AllSyncx)
AllSyncx <- AllSync %>%
  pivot_longer(c(TempCor, FlowCor, Axis1:Axis2), names_to = "Variable", values_to = "Correlation") %>%
  dplyr::select(Euclid_Dist_Meters, MeanLat, Variable, Correlation)


## biplot
# library("devtools")
# install_github("kassambara/factoextra")
library("factoextra")
install.packages("factoextra")

res.pca <- prcomp(AllSyncx[, -3],  scale = TRUE)
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, geom="point")
  
p <- fviz_pca_ind(res.pca, label="none", habillage=AllSync$Variable,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)

## mean sync

head(AllSync)
AllSyncx <- AllSync %>%
  pivot_longer(c(TempCor, FlowCor, Axis1:Axis2), names_to = "Variable", values_to = "Correlation")

mean_sync <- AllSyncx %>%
  group_by(Variable, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters),
            Mean_Lat = mean(MeanLat))

head(mean_sync)
# sum(is.na(mean_sync))
res.pca <- prcomp(mean_sync[, -c(1:2)],  scale = TRUE)
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, geom="point")

p <- fviz_pca_ind(res.pca, label="none", habillage=mean_sync$Variable,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)


b1 <- ggplot(mean_sync, aes(x=Mean_Lat, y=Mean_Cor, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Latitude") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
b1

file.name1 <- paste0(out.dir, "Mean_site_sync_temp_flow_latitude.jpg")
ggsave(b1, filename=file.name1, dpi=300, height=5, width=6)

b2 <- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
b2

file.name1 <- paste0(out.dir, "Mean_site_sync_temp_flow_raw_distance.jpg")
ggsave(b2, filename=file.name1, dpi=300, height=5, width=6)

# mean_SimSync <- SimSync %>%
#   group_by(Axis, Site_ID2) %>%
#   summarise(Mean_Cor = mean(Correlation), Mean_Lat = mean(MeanLat),
#             Mean_Lon = mean(MeanLon))
# 
# 
# b1 <- ggplot(mean_SimSync, aes(x=Mean_Lat, y=Mean_Cor, color = Axis)) + 
#   geom_point() +
#   scale_x_continuous(name="Latitude") +
#   scale_y_continuous(name="Synchrony", limits=c(-1, 1))
# b1
# 
# b2 <- ggplot(mean_SimSync, aes(x=Mean_Lon, y=Mean_Cor, color = Axis)) + 
#   geom_point() +
#   scale_x_continuous(name="Longitude") +
#   scale_y_continuous(name="Synchrony", limits=c(-1, 1))
# b2



## between basin (global)
allDFx <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist_all_together.csv")
head(allDFx)




