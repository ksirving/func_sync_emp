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

