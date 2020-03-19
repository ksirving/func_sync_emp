### methods details

## find out particulars about bio and env data.

## biological data

# fish abundance 

setwd("/Users/katieirving/Documents/sYNGEO/func_emp")

fish_ab<-read.csv("fishdata_selection_basins_same_time_window_cleanTaxo.csv")
head(fish_ab)
fish_ab$New_names<-gsub(" ","_", fish_ab$New_names)

range(fish_ab$Year)
unique(fish_ab$Origin)
# [1] France    Spain     Sweden    UK        Australia
# [6] Maryland  OHIO      Southwest

length(unique(fish_ab$sYNGEO_ID))
# 815

length(unique(fish_ab$MAIN_BAS))
# 46

length(unique(fish_ab$New_names))
# 208

library(plyr)
plyr::count(fish_ab,"Origin") # 8 countries
plyr::count(fish_ab, c("Origin", "MAIN_BAS", "sYNGEO_ID"))

# Australia - 2 basins
# France
#     Origin  freq  basin sites
# 1 Australia  3788 2     
# 2    France 12464 5
# 3  Maryland   841 1
# 4      OHIO  5119 1
# 5 Southwest  1132 1
# 6     Spain   572 1
# 7    Sweden 11280 26
# 8        UK  8128 9

install.packages("ncdf4")
library(ncdf4); library(sp); library(raster)
#-------------------------------------------------------------------------------------
#flow data
#-------------------------------------------------------------------------------------
#---------------------------
#check content flow data
#---------------------------
dat.fname <- "/Users/katieirving/Documents/sYNGEO/func_emp/data/FLO1K.ts.1960.2015.qmi.nc"
#
##get info layers
ncin <- nc_open(dat.fname)
ncin
nc_close(ncin)
#--------------------------

#--------------------------
#upload data
#--------------------------
sites = read.table("/Users/katieirving/Documents/sYNGEO/func_emp/data/fishdata_selection_basins_same_time_window.txt",h=T)
coordinates(sites)<- c("Longitude","Latitude")

#-------------------------
#extract data 
#-------------------------

#create raster brick
b1 <- brick("FLO1K.ts.1960.2015.qmin",varname="qmin",level=1)   #1960-2015
b2 <- brick("FLO1K.ts.1960.2015.qmin",varname="qmax",level=1)   #1960-2015

Year = as.numeric(sapply(strsplit(gsub("X","",names(b1)),".",fixed=T),'[',1))

qmin_av = qmax_av = NULL
for(i in 1:nrow(sites)){
  pts = sites[i,]
  ii = which(Year %in% (2003:2014)) 
  qmin_av=rbind(qmin_av,c(extract(b1,pts)[ii]))
  qmax_av=rbind(qmax_av,c(extract(b2,pts)[ii]))
  
}

colnames(qmin_av) = 2003:2013
colnames(qmax_av) = 2003:2013

qmin_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,qmin_av)
qmax_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,qmax_av)

qmean = (qmin_av[,2:12] + qmax_av[,2:12])/2
qmean = data.frame(sYNGEO_ID = sites$sYNGEO_ID,qmean)

write.csv(qmin_av,"D:/Empirical Functional paper/Sites_qmin_av.csv",row.names=F)
write.csv(qmax_av,"D:/Empirical Functional paper/Sites_qmax_av.csv",row.names=F)
write.csv(qmean,"D:/Empirical Functional paper/Sites_qmean_av.csv",row.names=F)

#------------------------------------
#compute anomalies
#------------------------------------
anom_qmean = (qmean[,2:12] - apply(qmean[,2:12],1,mean))
anom_qmean = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_qmean)

anom_qmin_av = (qmin_av[,2:12] - apply(qmin_av[,2:12],1,mean))
anom_qmin_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_qmin_av)

anom_qmax_av = (qmax_av[,2:12] - apply(qmax_av[,2:12],1,mean))
anom_qmax_av = data.frame(sYNGEO_ID = sites$sYNGEO_ID,anom_qmax_av)

write.csv(anom_qmin_av,"D:/Empirical Functional paper/Sites_anomalies_qmin_av.csv",row.names=F)
write.csv(anom_qmax_av,"D:/Empirical Functional paper/Sites_anomalies_qmax_av.csv",row.names=F)
write.csv(anom_qmean,"D:/Empirical Functional paper/Sites_anomalies_qmean_av.csv",row.names=F)





