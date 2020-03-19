#------------------------------
#discharge flow1k
#------------------------------
#create raster brick
dat.fname <- "C:/Users/Lise Comte/Dropbox/analyses_temporal_datasets/ENV_LAYERS/FLOW1K/FLO1K.ts.1960.2015.qav.nc"
b <- brick(dat.fname,varname="qav",level=1)   #1960-2015

Year = as.numeric(sapply(strsplit(gsub("X","",names(b)),".",fixed=T),'[',1)) + 4

for(kk in levels(sites$Ecoregion)){
sites_fin = sites[sites$Ecoregion == kk,]

qav = NULL
for(i in 1:nrow(sites_fin)){
pts = sites_fin[i,]
ii = which(Year %in% (pts$Tfirst - 4) : (pts$Tlast)) 
qav=rbind(qav,cbind(as.character(sites_fin$Glob_ID[i]),(extract(b,pts)[ii]  - 273.15),Year[ii]))
}

colnames(qav) = c("Glob_ID","Qav","Year")
write.csv(tp,paste0("Discharge_for_trends_",kk,".csv"),row.names=F)
print(kk)
}
