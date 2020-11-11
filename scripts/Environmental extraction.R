library(ncdf4); library(sp); library(raster)
#-------------------------------------------------------------------------------------
#climate data
#-------------------------------------------------------------------------------------
#---------------------------
#check content climate data
#---------------------------
dat.fname <- "/Users/katieirving/Documents/sYNGEO/func_emp/data/TerraClimate_tmin_2003.nc"
#
##get info layers
ncin <- nc_open(dat.fname)
ncin
nc_close(ncin)
#--------------------------

#--------------------------
#upload data
#--------------------------
# sites = read.table("C:/Users/Lise Comte/Dropbox/Time_series_sYNGEO/Fish Data/FINAL_DATA/Empirical/sites_selection_basins_same_time_window.txt",h=T)
# sites <- read.csv("/Users/katieirving/Documents/git/func_sync_emp/input_data/Bio/sites_selection_basins_same_time_window_10262020.csv")
sites <- read.csv("input_data/Bio/SiteID_Coords_locations.csv")
coordinates(sites)<- c("Longitude","Latitude")
head(sites)
#-------------------------
#extract data (loop over the years)
#-------------------------

b2 <- brick(dat.fname,varname="tmin",level=1)   #just to initialize the loop

tmin_av = tmax_av = NULL
for(i in 2003:2013){
b1 <- brick(paste0("/Users/katieirving/Documents/sYNGEO/func_emp/data/TerraClimate_tmin_",i,".nc"),varname="tmin",level=1)   
b2 <- brick(paste0("/Users/katieirving/Documents/sYNGEO/func_emp/data/TerraClimate_tmax_",i,".nc"),varname="tmax",level=1)   


#if want to check the names
#aa = gsub("X","",names(b2))
#Year = sapply(strsplit(as.character(aa),".",fixed=T),'[',1)
#Month = sapply(strsplit(as.character(aa),".",fixed=T),'[',2)

#---------------------------------------------------------------
#calculating annual average based on monthly data
#--------------------------------------------------------------

tmin = cbind(extract(b1[[1]],sites),extract(b1[[2]],sites),extract(b1[[3]],sites),extract(b1[[4]],sites),extract(b1[[5]],sites),extract(b1[[6]],sites),extract(b1[[7]],sites),extract(b1[[8]],sites),extract(b1[[9]],sites),extract(b1[[10]],sites),extract(b1[[11]],sites),extract(b1[[12]],sites))
tmin_av = cbind(tmin_av,apply(tmin,1,mean))

tmax = cbind(extract(b2[[1]],sites),extract(b2[[2]],sites),extract(b2[[3]],sites),extract(b2[[4]],sites),extract(b2[[5]],sites),extract(b2[[6]],sites),extract(b2[[7]],sites),extract(b2[[8]],sites),extract(b2[[9]],sites),extract(b2[[10]],sites),extract(b2[[11]],sites),extract(b2[[12]],sites))
tmax_av = cbind(tmax_av,apply(tmax,1,mean))

print(i)
}
# warnings()
colnames(tmax_av) = 2003:2013
colnames(tmin_av) = 2003:2013

tmin_av = data.frame(SiteID = sites$SiteID,tmin_av)
tmax_av = data.frame(SiteID = sites$SiteID,tmax_av)

tmean = (tmin_av[,2:12] + tmax_av[,2:12])/2
tmean = data.frame(SiteID = sites$SiteID,tmean)

write.csv(tmin_av,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_tmin_av_new_sites.csv",row.names=F)
write.csv(tmax_av,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_tmax_av_new_sites.csv",row.names=F)
write.csv(tmean,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_tmean_av_new_sites.csv",row.names=F)

#------------------------------------
#compute anomalies
#------------------------------------
anom_tmean = (tmean[,2:12] - apply(tmean[,2:12],1,mean))
anom_tmean = data.frame(SiteID = sites$SiteID,anom_tmean)

anom_tmin_av = (tmin_av[,2:12] - apply(tmin_av[,2:12],1,mean))
anom_tmin_av = data.frame(SiteID = sites$SiteID,anom_tmin_av)

anom_tmax_av = (tmax_av[,2:12] - apply(tmax_av[,2:12],1,mean))
anom_tmax_av = data.frame(SiteID = sites$SiteID,anom_tmax_av)

write.csv(anom_tmin_av,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_anomalies_tmin_av_new_sites.csv",row.names=F)
write.csv(anom_tmax_av,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_anomalies_tmax_av_new_sites.csv",row.names=F)
write.csv(anom_tmean,"/Users/katieirving/Documents/git/func_sync_emp/output_data/Sites_anomalies_tmean_av_new_sites.csv",row.names=F)

#-------------------------------------------------------------------------------------
#flow data
#-------------------------------------------------------------------------------------
#---------------------------
#check content flow data
#---------------------------
dat.fname <- "/Users/katieirving/Documents/sYNGEO/func_emp/data/FLO1K.ts.1960.2015.qav.nc"
#
str(dat.fname)
##get info layers
ncin <- nc_open(dat.fname)
ncin
nc_close(ncin)

setwd("/Users/katieirving/Documents/git/func_sync_emp")
#--------------------------

#--------------------------
#upload site data
#--------------------------
# FishSites <- read.csv("/Users/katieirving/Documents/git/func_sync_emp/input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
# 
# FishSites <- FishSites[, c(9,11,12,14,15,16)]
# 
# FishSites <- FishSites[!duplicated(FishSites), ]
# write.csv(FishSites, "input_data/Bio/SiteID_Coords_locations.csv")

# sites <- sites[!duplicated(sites), ]
dim(FishSites) ## sites
coordinates(FishSites)<- c("Longitude","Latitude")
sites <- FishSites
#-------------------------
#extract data 
#-------------------------

#create raster brick
# bmean <- brick("/Users/katieirving/Documents/sYNGEO/func_emp/data/FLO1K.ts.1960.2015.qav.nc",varname="qav",level=1)   #1960-2015

bmin <- brick("/Users/katieirving/Documents/sYNGEO/func_emp/data/FLO1K.ts.1960.2015.qmi.nc",varname="qmi",level=1)   #1960-2015

bmax <- brick("/Users/katieirving/Documents/sYNGEO/func_emp/data/FLO1K.ts.1960.2015.qma.nc",varname="qma",level=1)   #1960-2015

Year = as.numeric(sapply(strsplit(gsub("X","",names(bmin)),".",fixed=T),'[',1))

qmin_av = qmax_av = NULL
i=1
for(i in 1:nrow(sites)){
pts = sites[i,]
ii = which(Year %in% (2003:2014)) 
qmin_av=rbind(qmin_av,c(extract(bmin,pts)[ii]))
qmax_av=rbind(qmax_av,c(extract(bmax,pts)[ii]))

}


save(qmin_av, qmax_av, file="output_data/flow_min_max_values_extracted_new_sites.RData")
load(file="output_data/flow_min_max_values_extracted_new_sites.RData")
range(qmin_av)
# str(qmin_av)
qmin_av <- as.data.frame(qmin_av)
qmax_av <- as.data.frame(qmax_av)
colnames(qmin_av) = 2003:2014
colnames(qmax_av) = 2003:2014
names(qmax_av)
dim(qmax_av)
head(sites)
dim(sites)

## get siteid

# siteSP <- shapefile("/Users/katieirving/Documents/git/func_sync_emp/input_data/Bio/Sites_funcsynchrony_10262020.shp")
# sites = read.csv("input_data/Bio/sites_selection_basins_same_time_window_10262020.csv",h=T)

qmin_av = data.frame(SiteID = sites$SiteID,qmin_av)
qmax_av = data.frame(SiteID = sites$SiteID,qmax_av)
qmax_av <- qmax_av[, -13]
qmin_av <- qmin_av[, -13]
qmean = (qmin_av[,2:12] + qmax_av[,2:12])/2
qmean = data.frame(SiteID = sites$SiteID,qmean)
## 4 NAs - sites with no flow value
## remove duplicates

save(qmin_av, qmax_av, qmean, file="output_data/flow_min_max_mean_extracted_new_sites_raw.RData")

library(tidyverse)
#------------------------------------
#compute anomalies
#------------------------------------
anom_qmean = (qmean[,2:12] - apply(qmean[,2:12],1,mean))
anom_qmean = data.frame(SiteID = sites$SiteID,anom_qmean)

anom_qmin_av = (qmin_av[,2:12] - apply(qmin_av[,2:12],1,mean))
anom_qmin_av = data.frame(SiteID = sites$SiteID,anom_qmin_av)
anom_qmin_av$X2003
head(anom_qmin_av)

anom_qmax_av = (qmax_av[,2:12] - apply(qmax_av[,2:12],1,mean))
anom_qmax_av = data.frame(SiteID = sites$SiteID,anom_qmax_av)


save(anom_qmin_av,file="output_data/Sites_anomolies_qmin_av_flow_new_sites.RData")
save(anom_qmax_av,file="output_data/Sites_anomolies_qmax_av_flow_new_sites.RData")
save(anom_qmean,file="output_data/Sites_anomolies_qmean_av_flow_new_sites.RData")


names(qmean)
dim(qmin_av) ## 795
head(qmin_av)
head(anom_qmin_av)

save(qmin_av,file="output_data/Sites_qmin_av_flow_new_sites_raw.RData")
save(qmax_av,file="output_data/Sites_qmax_av_flow_new_sites_raw.RData")
save(qmean,file="output_data/Sites_qmean_flow_new_sites_raw.RData")


