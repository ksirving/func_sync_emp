## Code graveyard

stopCluster(cl)
warnings()
# warnings()
# synchrony_axis
# dim(synchrony_axis) #19984257        6 - 1:28 basins
# dim(synchrony_axis) #18561158         29:46
# synchrony_axis<-data.frame(synchrony_axis)
# colnames(synchrony_axis)<-c("basin_ID", "Axis", "Correlation","Site_ID1","Site_ID2", "year_removed")
# nlevels(factor(synchrony_axis$basin_ID)) # 28
# nlevels(factor(synchrony_axis$Axis)) # 2
# nlevels(factor(synchrony_axis$year_removed)) # 11
# tail(synchrony_axis)
# head(synchrony_axis)

###save results
# save(synchrony_axis, file = "output_data/results_between_site_synchrony_1-28.RData") ## 1-28 basins
# load(file = "output_data/results_between_site_synchrony_1-28.RData")


save(synchrony_axis, file = "output_data/results_between_site_synchrony_29-46.RData") ## 29-46 basins

# save(synchrony_axis, file = paste0("output_data/results_between_site_synchrony", basinsID[basin],  ".RData"))

load(file = "output_data/results_between_site_synchrony_1-28.RData")

## split data files by basin

basinIDs <- unique(synchrony_axis$basin_ID)

for (b in 1: length(basinIDs)) {
  
  basin <- subset(synchrony_axis, basin_ID == basinIDs[b])
  basin <- na.omit(basin)
  
  save(basin, file=paste0("output_data/basins", paste(basinIDs[b], "_bet_site_sync_one_out.RData", sep="")))
  
}

rm(synchrony_axis)

load(file = "output_data/results_between_site_synchrony_29-46.RData")
## split data files by basin

basinIDs <- unique(synchrony_axis$basin_ID)

for (b in 1: length(basinIDs)) {
  
  basin <- subset(synchrony_axis, basin_ID == basinIDs[b])
  basin <- na.omit(basin)
  
  save(basin, file=paste0("output_data/basins", paste(basinIDs[b], "_bet_site_sync_one_out.RData", sep="")))
  
  