#this script reads in the rx maps from each year in a landis simulation
#and maps cell by cell the total rx count
library(raster)
library(rgdal)

#get files that have the correct name
events<-list.files(path="C:\\Users\\kejones8\\grin_repo\\GRIN\\landis_runs\\Highfire_1\\LANDIS_Sapps_Active_v1_5\\social-climate-fire\\", pattern="event-ID",full.names=TRUE)
allrasters <- stack(events) #this only worked after loading rgdal

allrasters[allrasters>1]<-1

count<-sum(allrasters)

plot(count)
