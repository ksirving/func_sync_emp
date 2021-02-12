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


## upload PCA scores
pca_scores <- read.csv("output_data/01_trait_pca_scores_new_sites.csv")


## upload synchrony data
## all synchrony
sync <- read.csv("output_data/02_results_between_site_synchrony_Jan2020.csv")
## leave one out synchrony - one basin

load(file= "/Users/katieirving/Documents/git/func_sync_emp/output_data/01_7080047060_bet_site_sync_one_out.RData")
LOOsync <- synchrony_axis

head(pca_scores)

pca_scores <- pca_scores %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Score")

## box plot each axis each year

# create a data frame
variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)
head(data)
head(pca_scores)
str(pca_scores)

pca_scores$year <- as.factor(pca_scores$year)

ggplot(pca_scores, aes(x=year, y=Score, fill=Axis)) + 
  geom_boxplot()

p2 <- ggplot(pca_scores, aes(x=year, y=Score, fill=Axis)) + 
  geom_boxplot() +
  facet_wrap(~Country, scale="free")
p2

## try by country and group by basin

France_PCA <- pca_scores %>%
  filter(Country == "FRA")
?scale_y_continuous
p2 <- ggplot(France_PCA, aes(x=year, y=Score, fill=Axis)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(-3,3))+
  facet_wrap(~HydroBasin, scale="free")
p2

## same on sync values

SimSync <- read.csv( "output_data/04_sync_data_similarity_euclidean_dist.csv")
head(SimSync)

str(SimSync)
str(pca_scores)

SimSync$basin_ID <- as.factor(SimSync$basin_ID)
 
# SimSync1 <- SimSync %>%
#   filter(Axis == "Axis1")

## figure shows fish size and reproduction overall synchrony in each basin
b1 = ggplot(SimSync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  # theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~Country, scale="free") 

file.name1 <- paste0(out.dir, "Between_site_sync_per_country.jpg")
ggsave(b1, filename=file.name1, dpi=300, height=5, width=6)

## how many basins in each country

basin_tally <- SimSync %>% group_by(Country) %>% count(basin_ID)
basin_tally
## try one country

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
# LOOsync
# 
# ggplot(LOOsync, aes(x=year_removed, y=Correlation, fill=Axis)) + 
#   geom_boxplot()
# 
# p2 <- ggplot(pca_scores, aes(x=year, y=Correlation, fill=Axis)) + 
#   geom_boxplot() +
#   facet_wrap(~Country, scale="free")
# p2

## try by country and group by basin
# 
# France_PCA <- pca_scores %>%
#   filter(Country == "FRA")
# ?scale_y_continuous
# p2 <- ggplot(France_PCA, aes(x=year, y=Score, fill=Axis)) + 
#   geom_boxplot() +
#   scale_y_continuous(limits = c(-3,3))+
#   facet_wrap(~HydroBasin, scale="free")
# p2

## try one basin with siumilarioty

Fra_sync <- SimSync %>%
  filter(basin_ID == "2080029400")

ggplot(Fra_sync, aes(x=Similarity, y=Correlation)) + 
  geom_point()

## plot all points

ggplot(SimSync1, aes(x=Similarity, y=Correlation)) + 
  geom_point()

## mean synchrony per site with simialrity

head(SimSync)

mean_sync <- SimSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity))

s1<- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() ### this is nice, how to get sync values between 0 and 1?

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
  geom_point() ### this is nice, how to get sync values between 0 and 1?
s1
file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_similarity.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)

s2<- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_TempCor)) + 
  geom_point() ### this is nice, how to get sync values between 0 and 1?
s2

file.name1 <- paste0(out.dir, "Mean_between_site_temp_sync_all_sites_distance_similarity.jpg")
ggsave(s2, filename=file.name1, dpi=300, height=5, width=6)

s3<- ggplot(mean_sync, aes(x=Mean_TempCor, y=Mean_Cor, color = Axis)) + 
  geom_point() ### this is nice, how to get sync values between 0 and 1?
s3

file.name1 <- paste0(out.dir, "Mean_between_site_sync_temp_sync_all_sites.jpg")
ggsave(s3, filename=file.name1, dpi=300, height=5, width=6)

basins <- sync %>%
  dplyr::select(basin_ID, Country, Site_ID2) %>%
  distinct()

mean_sync_join <- left_join(mean_sync, basins, by="Site_ID2")

mean_sync_join

## heatmap

head(sync)

## filter to one basin

FIN_sync <- SimSync %>%
  filter(basin_ID == "2080029400", Axis == "Axis1")

length(unique(FIN_sync$Site_ID1)) ## 9

FIN_sync <- FIN_sync[order(FIN_sync$Correlation),]
head(FIN_sync)

FIN_sync

row.names(FIN_sync) <- FIN_sync$Pair

FIN_syncx <- FIN_sync %>%
  dplyr :: select(Correlation,  Euclid_Dist_Meters, Similarity )
head(FIN_syncx)
# The mtcars dataset:
data <- as.matrix(FIN_syncx)
head(data)
# Default Heatmap
heatmap(data)
head(SimSync)

## plot between site synchrony

BasinSync <- read.csv("output_data/02_between_basin_sync_per_country.csv")
AllSync <- read.csv("output_data/02_between_basin_sync_all_together.csv")

head(BasinSync)

b3 <- ggplot(BasinSync, aes(x=Country, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  facet_wrap(~Axis, scale="free") 
b3
file.name1 <- paste0(out.dir, "Between_basin_sync_per_country.jpg")
ggsave(b3, filename=file.name1, dpi=300, height=5, width=6)

head(AllSync)

### basin synchrony and similarity

CountriesDFx <- read.csv("output_data/04_basin_sync_data_similarity_euclidean_dist.csv")
head(CountriesDFx)

mean_sync <- CountriesDFx %>%
  group_by(Axis, BasinID2, Country) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity))

c1 <- ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() 
c1

file.name1 <- paste0(out.dir, "Mean_basin_sync_per_country.jpg")
ggsave(c1, filename=file.name1, dpi=300, height=5, width=6)

c2 <- ggplot(CountriesDFx, aes(x=Similarity, y=Correlation, color = Axis)) + 
  geom_point()
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
  geom_point() ### this is nice, how to get sync values between 0 and 1?
g1

file.name1 <- paste0(out.dir, "Mean_basin_sync_global.jpg")
ggsave(g1, filename=file.name1, dpi=300, height=5, width=6)

ggplot(allDFx, aes(x=Similarity, y=Correlation, color = Axis)) + 
  geom_point()

filter(mean_sync, Mean_Sim <=0.25)
filter(mean_sync, Mean_Sim >=0.75)

## distance matrix of site synchrony




# load sda library
install.packages("sda")
library("sda")

## prepare data set
# data(iris) # good old iris data
# head(iris)
# X = as.matrix(iris[,1:4])
# Y = iris[,5]
# X
# Y
# ## estimate centroids and empirical pooled variances
# centroids(X, Y, lambda.var=0)
# 
# ## also compute group-specific variances
# centroids(X, Y, var.groups=TRUE, lambda.var=0)
# 
# ## use shrinkage estimator for the variances
# centroids(X, Y, var.groups=TRUE)
# 
# ## return centered data
# xc = centroids(X, Y, centered.data=TRUE)$centered.data
# xc
# apply(xc, 2, mean)
# 
# ## useful, e.g., to compute the inverse pooled correlation matrix
# powcor.shrink(xc, alpha=-1)

