library(sp)
library(rgdal)

#examining USFS hazardous fuels treatments

aoi_haz_ful_treat<-readOGR("C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\usfs_hazfultreat_aoi.shp")

activities<-unique(aoi_haz_ful_treat$ACTIVITY)

burn_related_act<-activities[c(5,13,18,21,27,43,46,52)]


burn_related_treatments<-aoi_haz_ful_treat[aoi_haz_ful_treat$ACTIVITY %in% burn_related_act,]

writeOGR(burn_related_treatments,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\usfs_burntreat_aoi.shp",layer = "usfs_burntreat_aoi",driver="ESRI Shapefile")


###unzipping and then merging all MTBS for aoi


library(plyr)
library(sf)

# get all the zip files
zipF <- list.files(path = "C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\mtbs_aoi\\mtbs", recursive= TRUE, pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir = "C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\mtbs_aoi\\mtbs\\extracted\\")

# get the csv files
shp_files <- list.files(path =   "C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\mtbs_aoi\\mtbs\\extracted\\", full.names = TRUE ,pattern = "*bndy.shp")

# read the csv files
my_data <- ldply(.data = shp_files, .fun = st_read)

sf_my_data<-st_as_sf(my_data)

st_write(sf_my_data,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\mtbs_aoi\\mtbs\\mtbs_rx_aoi.shp")#,layer = "mtbs_rx_aoi",driver="ESRI Shapefile")



#prepping short data
library(data.table)



##realized didn't need short data cause no rx included
####this did not work, though? a little confused why.

short_1992_2018<-fread("C:\\Users\\thebrain\\Dropbox\\FireChasers\\new\\raw_data\\RDS-2013-0009.5_GDB\\short_1992_2018_pnt5.csv")
aoi<-read_sf("C:\\Users\\thebrain\\Dropbox\\DEAL_lab\\S_Apps_Project\\diss_aoi\\buffered_utm.shp")

short_sf<-st_as_sf(short_1992_2018,coords=c("LONGITUDE","LATITUDE"),crs = st_crs(4326))
short_utm<-st_transform(short_sf, crs = st_crs(aoi))

clipped<-st_intersection(short_utm,aoi)
clipped[clipped$
#mydata2 <- st_collection_extract(clipped, "POINT")

st_write(mydata2,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\short_aoi.shp")
