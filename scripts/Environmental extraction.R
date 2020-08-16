library(ncdf4); library(sp); library(raster)
#-------------------------------------------------------------------------------------
#climate data
#-------------------------------------------------------------------------------------
#---------------------------
#check content climate data
#---------------------------
dat.fname <- "D:/Empirical Functional paper/TerraClimate_tmin_2003.nc"
#
##get info layers
ncin <- nc_open(dat.fname)
ncin
nc_close(ncin)
#--------------------------

#--------------------------
#upload data
#--------------------------
sites = read.table("C:/Users/Lise Comte/Dropbox/Time_series_sYNGEO/Fish Data/FINAL_DATA/Empirical/sites_selection_basins_same_time_window.txt",h=T)
coordinates(sites)<- c("Longitude","Latitude")

#-------------------------
#extract data (loop over the years)
#-------------------------

b2 <- brick(dat.fname,varname="tmin",level=1)   #just to initialize the loop

tmin_av = tmax_av = NULL
for(i in 2003:2013){
b1 <- brick(paste0("D:/Empirical Functional paper/TerraClimate_tmin_",i,".nc"),varname="tmin",level=1)   
b2 <- brick(paste0("D:/Empirical Functional paper/TerraClimate_tmax_",i,".nc"),varname="tmax",level=1)   


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

colnames(tmax_av) = 2003:2013
colnames(tmin_av) = 2003:2013

tmin_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,tmin_av)
tmax_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,tmax_av)

tmean = (tmin_av[,2:12] + tmax_av[,2:12])/2
tmean = data.frame(sYNGEO_ID = sites$sYNGEO_ID,tmean)

write.csv(tmin_av,"D:/Empirical Functional paper/Sites_tmin_av.csv",row.names=F)
write.csv(tmax_av,"D:/Empirical Functional paper/Sites_tmax_av.csv",row.names=F)
write.csv(tmean,"D:/Empirical Functional paper/Sites_tmean_av.csv",row.names=F)

#------------------------------------
#compute anomalies
#------------------------------------
anom_tmean = (tmean[,2:12] - apply(tmean[,2:12],1,mean))
anom_tmean = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_tmean)

anom_tmin_av = (tmin_av[,2:12] - apply(tmin_av[,2:12],1,mean))
anom_tmin_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_tmin_av)

anom_tmax_av = (tmax_av[,2:12] - apply(tmax_av[,2:12],1,mean))
anom_tmax_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_tmax_av)

write.csv(anom_tmin_av,"D:/Empirical Functional paper/Sites_anomalies_tmin_av.csv",row.names=F)
write.csv(anom_tmax_av,"D:/Empirical Functional paper/Sites_anomalies_tmax_av.csv",row.names=F)
write.csv(anom_tmean,"D:/Empirical Functional paper/Sites_anomalies_tmean_av.csv",row.names=F)

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
sites = read.table("input_data/Bio/fishdata_selection_basins_same_time_window.txt",h=T)
head(sites)
sites <- sites[, -c(3:8)]
sites <- sites[!duplicated(sites), ]
dim(sites) ## sites
coordinates(sites)<- c("Longitude","Latitude")
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



save(qmin_av, qmax_av, file="output_data/flow_min_max_values_extracted.RData")
load(file="output_data/flow_min_max_values_extracted.RData")
# str(qmin_av)
qmin_av <- as.data.frame(qmin_av)
qmax_av <- as.data.frame(qmax_av)
colnames(qmin_av) = 2003:2014
colnames(qmax_av) = 2003:2014
head(qmax_av)

qmin_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,qmin_av)
qmax_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,qmax_av)
qmean = (qmin_av[,2:12] + qmax_av[,2:12])/2
qmean = data.frame(SiteID = sites$SiteID,qmean)
dim(qmin_av)
## remove duplicates

library(tidyverse)
#------------------------------------
#compute anomalies
#------------------------------------
anom_qmean = (qmean[,2:12] - apply(qmean[,2:12],1,mean))
anom_qmean = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_qmean)

# head(anom_qmean)
anom_qmin_av = (qmin_av[,2:12] - apply(qmin_av[,2:12],1,mean))
anom_qmin_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_qmin_av)


anom_qmax_av = (qmax_av[,2:12] - apply(qmax_av[,2:12],1,mean))
anom_qmax_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_qmax_av)


save(anom_qmin_av,file="output_data/Sites_anomolies_qmin_av_flow.RData")
save(anom_qmax_av,file="output_data/Sites_anomolies_qmax_av_flow.RData")
save(anom_qmean,file="output_data/Sites_anomolies_qmean_av_flow.RData")

qmin_av <- qmin_av %>%  select(-X2014)
qmax_av <- qmax_av %>%  select(-X2014)

names(qmean)
dim(qmin_av) ## 816
head(qmin_av)
head(anom_qmin_av)

save(qmin_av,file="output_data/Sites_qmin_av_flow.RData")
save(qmax_av,file="output_data/Sites_qmax_av_flow.RData")
save(qmean,file="output_data/Sites_qmean_flow.RData")


