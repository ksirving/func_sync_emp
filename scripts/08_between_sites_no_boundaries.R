## Analysis 2

### between site anaylsis no boundaries and within basin variable

## packages

library(tidyverse)
library(tidylog)

out.dir <- "/Users/katieirving/Documents/git/func_sync_emp/Figures/"

## data 

SimSync <- read.csv("output_data/04a_sync_data_similarity_euclidean_dist_no_boundaries_new_ord_dummy_var.csv")

head(Sync)

SimSync <- SimSync %>%
  select(-X,-Pair.y) %>%
  rename(Pair = Pair.x) %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation")

NAs <- which(is.na(SimSync))
Sync[NAs, ]

### mean sync grouping by inBasin dummy variable

mean_syncInBasin <- SimSync %>%
  filter(InBasin ==1) %>%
  group_by(Axis, Site_ID2, basin_ID, Country, InBasin) %>%
  summarise(Mean_Cor = mean(na.omit(Correlation)), Mean_Dist = mean(na.omit(Euclid_Dist_Meters)), 
            Mean_Lat =mean(na.omit(MeanLat))) 


mean_syncOutBasin <- SimSync %>%
  filter(InBasin ==0) %>%
  select(-basin_ID, -Country,) %>%
  group_by(Axis, Site_ID2, InBasin) %>%
  summarise(Mean_Cor = mean(na.omit(Correlation)), Mean_Dist = mean(na.omit(Euclid_Dist_Meters)), 
            Mean_Lat =mean(na.omit(MeanLat)))

## NAs in outbasin  - find these later!!

head(mean_syncInBasin)
head(mean_syncOutBasin)
sum(is.na(mean_syncOutBasin)) ## 66
sum(is.na(mean_syncInBasin)) ## 66

NAsOut <- which(is.na(mean_syncOutBasin))

mean_syncOutBasin[NAsOut, ]

NAsIn <- which(is.na(mean_syncInBasin))

test <- mean_syncInBasin[NAsIn, ] ## all rows missing

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



# Environmental variables -------------------------------------------------

## upload data
TempSync <- read.csv("output_data/02a_temp_sync_between_sites_new_ord_no_boundaries.csv")
FlowSync <- read.csv("output_data/02a_flow_sync_between_sites_new_ord_no_boundaries.csv")
SimSync <- read.csv("output_data/04a_sync_data_similarity_euclidean_dist_no_boundaries_new_ord_dummy_var.csv")


## mean temp synchrony and mean trait synchrony
## figure ?
head(TempSync)
TempSync <- rename(TempSync, Pair = X, TempCor = Correlation)

## make sync values wider and format for join
head(SimSync)

SimSyncx <- SimSync %>%
  select(-X,-Pair.y) %>%
  rename(Pair = Pair.x) %>%
  distinct()

TempSync <- TempSync %>%
  filter(Env_Var == "clim_max_raw")
head(TempSync)

AllSync <- full_join(TempSync[, c(1,3,5)], SimSyncx, by = "Pair")
# AllSync <- na.omit(AllSync)

head(AllSync)

AllSync <- AllSync %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation") %>%
  select(-Site_ID2.x) %>%
  rename(Site_ID2 = Site_ID2.y)
  

mean_sync <- AllSync %>%
  group_by(Axis, Site_ID2, InBasin) %>%
  summarise(Mean_Cor = mean(na.omit(Correlation)), Mean_Dist = mean(na.omit(Euclid_Dist_Meters)), Mean_TempCor = mean(na.omit(TempCor)))

head(mean_sync)

# New facet label names for inbasin
supp.labs <- c("Within Same Basin", "Outside Basin")
names(supp.labs) <- c("1", "0")



t1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_TempCor)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Temperature Synchrony")

t1

file.name1 <- paste0(out.dir, "Mean_between_site_temp_sync_all_sites_distance_raw_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(t1, filename=file.name1, dpi=300, height=5, width=6)

t1a<- ggplot(mean_sync, aes(x=log(Mean_Dist), y=Mean_TempCor)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Log Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Temperature Synchrony")

t1a

file.name1 <- paste0(out.dir, "Mean_between_site_temp_sync_all_sites_log_distance_raw_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(t1a, filename=file.name1, dpi=300, height=5, width=6)

#### flow

head(FlowSync)
FlowSync <- rename(FlowSync, Pair = X, FlowCor = Correlation)

unique(FlowSync$Env_Var)
FlowSync <- FlowSync %>%
  filter(Env_Var == "qmax_raw")
head(FlowSync)

FlowSync$Pair <- gsub(".1", "",FlowSync$Pair, fixed = T )

AllSync <- full_join(FlowSync[, c(1,3,5)], SimSyncx, by = "Pair") ## some do not match????
# AllSync <- na.omit(AllSync)

head(AllSync)

AllSync <- AllSync %>%
  pivot_longer(Axis1:Axis2, names_to = "Axis", values_to = "Correlation") %>%
  select(-Site_ID2.x) %>%
  rename(Site_ID2 = Site_ID2.y)

mean_sync <- AllSync %>%
  group_by(Axis, Site_ID2, InBasin) %>%
  summarise(Mean_Cor = mean(na.omit(Correlation)), Mean_Dist = mean(na.omit(Euclid_Dist_Meters)), Mean_FlowCor = mean(na.omit(FlowCor)))

head(mean_sync)


f1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_FlowCor)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Flow Synchrony")

f1

file.name1 <- paste0(out.dir, "Mean_between_site_flow_sync_all_sites_distance_raw_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(f1, filename=file.name1, dpi=300, height=5, width=6)

f1a<- ggplot(mean_sync, aes(x=log(Mean_Dist), y=Mean_FlowCor)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Log Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Flow Synchrony")

f1a

file.name1 <- paste0(out.dir, "Mean_between_site_flow_sync_all_sites_log_distance_raw_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(f1a, filename=file.name1, dpi=300, height=5, width=6)


### all on one figure

AllSync <- full_join(TempSync[, c(1,3,5)], SimSyncx, by = "Pair")
AllSync <- full_join(FlowSync[, c(1,3,5)], AllSync, by = "Pair") ### again here, 51 site pairs didn't match, fix later!!!
# AllSync <- na.omit(AllSync)
dim(AllSync)
names(AllSync)
head(AllSyncx)
head(mean_sync)

AllSyncx <- AllSync %>%
  pivot_longer(c(TempCor, FlowCor, Axis1:Axis2), names_to = "Variable", values_to = "Correlation") %>%
  dplyr::select(Site_ID1, Site_ID2, Euclid_Dist_Meters, MeanLat, Variable, Correlation, InBasin, basin_ID, Country)

mean_sync <- AllSyncx %>%
  group_by(Variable, Site_ID2, InBasin) %>%
  summarise(Mean_Cor = mean(na.omit(Correlation)), Mean_Dist = mean(na.omit(Euclid_Dist_Meters)))


a2 <- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Variable)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
a2

file.name1 <- paste0(out.dir, "Mean_site_sync_temp_flow_raw_distance_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(a2, filename=file.name1, dpi=300, height=5, width=6)

a2a <- ggplot(mean_sync, aes(x=log(Mean_Dist), y=Mean_Cor, color = Variable)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Log Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
a2a

file.name1 <- paste0(out.dir, "Mean_site_sync_temp_flow_raw_log_distance_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(a2a, filename=file.name1, dpi=300, height=5, width=6)

### plot without mean

head(AllSyncx)

h1 <- ggplot(AllSyncx, aes(x=Euclid_Dist_Meters, y=Correlation, color = Variable)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))

h1

file.name1 <- paste0(out.dir, "Raw_site_sync_temp_flow_raw_distance_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(h1, filename=file.name1, dpi=300, height=5, width=6)

h1a <- ggplot(AllSyncx, aes(x=log(Euclid_Dist_Meters), y=Correlation, color = Variable)) +
  geom_point() +
  facet_wrap(~InBasin, labeller = labeller(InBasin = supp.labs)) +
  scale_x_continuous(name="Log Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))

h1a

file.name1 <- paste0(out.dir, "Raw_site_sync_temp_flow_raw_log_distance_new_ord_no_boundaries_within_out_basin.jpg")
ggsave(h1a, filename=file.name1, dpi=300, height=5, width=6)
