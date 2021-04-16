### between sites within basins analysis

## packages

library(tidyverse)
library(tidylog)

out.dir <- "/Users/katieirving/Documents/git/func_sync_emp/Figures/"


## data 

SimSync <- read.csv("output_data/04_sync_data_similarity_euclidean_dist_new_ord.csv") ## between site trait synchrony
TempSync <- read.csv("output_data/02_temp_sync_between_sites.csv")
FlowSync <- read.csv("output_data/02_flow_sync_between_sites.csv")


## trait synchrony - country box plots
## figures 2 & 3

## how many basins in each country

# basin_tally <- SimSync %>% group_by(Country, basinID) %>% count(site_ID1) ## not correct!!!!
# basin_tally

SimSync$basin_ID <- as.factor(SimSync$basin_ID)

## figure shows fish size and reproduction overall synchrony in each basin - check the axis
b1 = ggplot(SimSync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  # theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~Country, scale="free") 
b1

file.name1 <- paste0(out.dir, "Between_site_sync_per_country_new_ord.jpg")
ggsave(b1, filename=file.name1, dpi=300, height=5, width=6)

## single country examples

Fra_sync <- SimSync %>%
  filter(Country == "FRA")

b2 <- ggplot(Fra_sync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
b2

file.name1 <- paste0(out.dir, "France_between_site_sync_new_ord.jpg")
ggsave(b2, filename=file.name1, dpi=300, height=5, width=6)


SWE_sync <- SimSync %>%
  filter(Country == "SWE")

b3 <- ggplot(SWE_sync, aes(x=basin_ID, y=Correlation, fill=Axis)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
b3

file.name1 <- paste0(out.dir, "Sweden_between_site_sync_new_ord.jpg")
ggsave(b3, filename=file.name1, dpi=300, height=5, width=6)

#### mean distance and synchrony
## Figure 4

mean_sync <- SimSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters), Mean_Lat =mean(MeanLat))


s1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Axis)) + ### this is nice
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))

s1

file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_distance_raw_new_ord.jpg")
ggsave(s1, filename=file.name1, dpi=300, height=5, width=6)

s1a<- ggplot(mean_sync, aes(x=log(Mean_Dist), y=Mean_Cor, color = Axis)) + ### this is nice
  geom_point() +
  scale_x_continuous(name="Log Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))

s1a

file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_log_distance_raw_new_ord.jpg")
ggsave(s1a, filename=file.name1, dpi=300, height=5, width=6)


## mean latitude and synchrony
s2<- ggplot(mean_sync, aes(x=Mean_Lat, y=Mean_Cor, color = Axis)) + ### this is nice
  geom_point() +
  scale_x_continuous(name="Mean Latitude") +
  scale_y_continuous(name="Mean Synchrony", limits=c(-1, 1))

s2

file.name1 <- paste0(out.dir, "Mean_between_site_sync_all_sites_latitude_new_ord.jpg")
ggsave(s2, filename=file.name1, dpi=300, height=5, width=6)

head(mean_sync)

## mean temp synchrony and mean trait synchrony
## figure 5

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


mean_sync <- AllSync %>%
  group_by(Axis, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters), Mean_TempCor = mean(TempCor))
unique(mean_sync$Axis)


t1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_TempCor)) +
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)", limits=c(0, 800000)) +
  scale_y_continuous(name="Mean Temperature Synchrony")

t1

file.name1 <- paste0(out.dir, "Mean_between_site_temp_sync_all_sites_distance_raw_new_ord.jpg")
ggsave(t1, filename=file.name1, dpi=300, height=5, width=6)

t1a<- ggplot(mean_sync, aes(x=log(Mean_Dist), y=Mean_TempCor)) +
  geom_point() +
  scale_x_continuous(name="Log Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Temperature Synchrony")

t1a

file.name1 <- paste0(out.dir, "Mean_between_site_temp_sync_all_sites_log_distance_raw_new_ord.jpg")
ggsave(t1a, filename=file.name1, dpi=300, height=5, width=6)


## mean flow synchrony and mean trait synchrony
# figure 5

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


f1<- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_FlowCor)) +
  geom_point() +
  scale_x_continuous(name="Mean Eucliean Distance (m)", limits=c(0, 800000)) +
  scale_y_continuous(name="Mean Flow Synchrony")

f1

file.name1 <- paste0(out.dir, "Mean_between_site_flow_sync_all_sites_distance_raw_new_ord.jpg")
ggsave(f1, filename=file.name1, dpi=300, height=5, width=6)

f1a<- ggplot(mean_sync, aes(x=log(Mean_Dist), y=Mean_FlowCor)) +
  geom_point() +
  scale_x_continuous(name="Log Mean Eucliean Distance (m)") +
  scale_y_continuous(name="Mean Flow Synchrony")

f1a

file.name1 <- paste0(out.dir, "Mean_between_site_flow_sync_all_sites_log_distance_raw_new_ord.jpg")
ggsave(f1a, filename=file.name1, dpi=300, height=5, width=6)


## all on one plot
## figure 5

## format synchrony
SimSyncx <- SimSync %>%
  dplyr::select(-Pair, -X) %>%
  pivot_wider(names_from = "Axis", values_from = Correlation) %>%
  unite("Pair", Site_ID1:Site_ID2, remove = F, sep= ".") %>%
  distinct()

## format temp

# ?gsub
FlowSync$Pair <- gsub(".1", "",FlowSync$Pair, fixed = T )

## join dfs
AllSync <- full_join(TempSync[, c(1,3,5)], SimSyncx, by = "Pair")
AllSync <- full_join(FlowSync[, c(1,3,5)], AllSync, by = "Pair") ### again here, 51 site pairs didn't match, fix later!!!
AllSync <- na.omit(AllSync)
dim(AllSync)
names(AllSync)
head(AllSyncx)
head(mean_sync)

AllSyncx <- AllSync %>%
  pivot_longer(c(TempCor, FlowCor, Axis1:Axis2), names_to = "Variable", values_to = "Correlation") %>%
  dplyr::select(Site_ID1, Site_ID2, Euclid_Dist_Meters, MeanLat, Variable, Correlation, basin_ID, Country)

mean_sync <- AllSyncx %>%
  group_by(Variable, Site_ID2) %>%
  summarise(Mean_Cor = mean(Correlation), Mean_Dist = mean(Euclid_Dist_Meters),
            Mean_Lat = mean(MeanLat))


a1 <- ggplot(mean_sync, aes(x=Mean_Lat, y=Mean_Cor, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Latitude") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
a1

file.name1 <- paste0(out.dir, "Mean_site_sync_temp_flow_latitude_new_ord.jpg")
ggsave(a1, filename=file.name1, dpi=300, height=5, width=6)

a2 <- ggplot(mean_sync, aes(x=Mean_Dist, y=Mean_Cor, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
a2

file.name1 <- paste0(out.dir, "Mean_site_sync_temp_flow_raw_distance_new_ord.jpg")
ggsave(a2, filename=file.name1, dpi=300, height=5, width=6)

a2a <- ggplot(mean_sync, aes(x=log(Mean_Dist), y=Mean_Cor, color = Variable)) +
  geom_point() +
  scale_x_continuous(name="Log Eucliean Distance (m)") +
  scale_y_continuous(name="Synchrony", limits=c(-1, 1))
a2a

file.name1 <- paste0(out.dir, "Mean_site_sync_temp_flow_raw_log_distance_new_ord.jpg")
ggsave(a2a, filename=file.name1, dpi=300, height=5, width=6)



# Matrix regression on synchrony -------------------------------------------------







