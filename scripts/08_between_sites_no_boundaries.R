### between site anaylsis no boundaries and within basin variable

## packages

library(tidyverse)
library(tidylog)

out.dir <- "/Users/katieirving/Documents/git/func_sync_emp/Figures/"

## data 

Sync <- read.csv("output_data/04a_sync_data_similarity_euclidean_dist_no_boundaries_new_ord_dummy_var.csv")

head(Sync)

Sync <- Sync %>%
  select(-X,-Pair.y) %>%
  rename(Pair = Pair.x) %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation")

NAs <- which(is.na(Sync))
Sync[NAs, ]

### mean sync grouping by inBasin dummy variable

mean_syncInBasin <- Sync %>%
  filter(InBasin ==1) %>%
  group_by(Axis, Site_ID2, basin_ID, Country, InBasin) %>%
  summarise(Mean_Cor = mean(na.omit(Correlation)), Mean_Dist = mean(na.omit(Euclid_Dist_Meters)), 
            Mean_Lat =mean(na.omit(MeanLat))) 


mean_syncOutBasin <- Sync %>%
  filter(InBasin ==0) %>%
  select(-basin_ID, -Country,) %>%
  group_by(Axis, Site_ID2, InBasin) %>%
  summarise(Mean_Cor = mean(na.omit(Correlation)), Mean_Dist = mean(na.omit(Euclid_Dist_Meters)), 
            Mean_Lat =mean(na.omit(MeanLat)))

## NAs in outbasin  - find these later!!

head(mean_syncInBasin)
head(mean_syncOutBasin)
sum(is.na(mean_syncOutBasin))
sum(is.na(mean_syncInBasin))

NAsOut <- which(is.na(mean_syncOutBasin))

mean_syncOutBasin[NAsOut, ]

NAsIn <- which(is.na(mean_syncInBasin))

mean_syncInBasin[NAsIn, ]

## combine dfs back together

mean_syncInBasin <- mean_syncInBasin %>%
  ungroup() %>%
  select(-basin_ID, -Country)


SyncBasin <- bind_rows(mean_syncInBasin, mean_syncOutBasin)
SyncBasin <- na.omit(SyncBasin)
head(SyncBasin)
unique(SyncBasin$InBasin)

## logistic regression looking predicting the probability that pairs are in the same basin - 
## x = correlation, y = probability
?glm
glmmod1 <- glm(InBasin ~ Mean_Cor, data = subset(SyncBasin, Axis == "Axis1"), family = binomial(link = "logit"))
glmmod2 <- glm(InBasin ~ Mean_Cor, data = subset(SyncBasin, Axis == "Axis2"), family = binomial(link = "logit"))

xcor <- seq(-1, 1, 0.01)
range(SyncBasin$Mean_Cor)

ycor1 <- predict(glmmod1, list(Mean_Cor = xcor),type="response")
ycor2 <- predict(glmmod2, list(Mean_Cor = xcor),type="response")

plot(SyncBasin$Mean_Cor, SyncBasin$InBasin, pch = 16, xlab = "Synchrony", ylab="Probability", main = "Probability that synchrony pairs are within same basin")
lines(xcor, ycor1)
lines(xcor, ycor2)
summary(glmmod) 
summary(glmmod1)
summary(glmmod2)

## plot mean synchrony of sites within basin, and sites outside of basin, facet = InBasin

# New facet label names for inbasin
supp.labs <- c("Within Same Basin", "Outside Basin")
names(supp.labs) <- c("1", "0")

s1<- ggplot(SyncBasin, aes(x=Mean_Dist, y=Mean_Cor, color = Axis)) + ### this is nice
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))

s1

file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_raw_within_out_basin_new_ord.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)



s2<- ggplot(SyncBasin, aes(x=log(Mean_Dist), y=Mean_Cor, color = Axis)) + ### this is nice
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Log Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))

s2

file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_LOG_distance_raw_within_out_basin_new_ord.jpg")
ggsave(s2, filename=file.name1, dpi=300, height=5, width=6)
