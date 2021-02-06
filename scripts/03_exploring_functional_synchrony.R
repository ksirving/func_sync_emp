## exploratory analysis

library(tidyverse)
library(tidylog)

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
ggplot(SimSync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  facet_wrap(~Country, scale="free") 

## how many basins in each country

basin_tally <- SimSync %>% group_by(Country) %>% count(basin_ID)

## try one country

Fra_sync <- SimSync %>%
  filter(Country == "FRA")

ggplot(Fra_sync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot()

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

## mean synchrony per site

head(SimSync)

mean_sync <- SimSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Sim = mean(Similarity))

ggplot(mean_sync, aes(x=Mean_Sim, y=Mean_Cor, color = Axis)) + 
  geom_point() ### this is nice, how to get sync values between 0 and 1?



head(mean_sync)

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

ggplot(BasinSync, aes(x=Country, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  facet_wrap(~Axis, scale="free") 

head(AllSync)


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

