###this script is to take map the fire interval for polygons (usda,doi,state parks)
###this will hopefully help us determine areas of interest/ business as usual burning


library(sf)
library(stringr)
library(dplyr)
library(raster)
library(RWmisc)
#the USDA/DOI shapefile was collected as a feature service from (https://doildt.maps.arcgis.com/home/item.html?id=acdb4a650c824c91ba7efd51d3f9f008#overview)
#with these specs: Created: Oct 4, 2017 Updated: Jul 19, 2021 View Count: 4,511
#kate then exported the feature service from QGIS to a shapefile, clipped to AOI, and selected for only RX treatments

#reading in USDA/DOI RX treatments for our AOI
usda_doi_needproj<-read_sf("C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\usda_doi_fire_treatments2.shp")

#project both to UTM 17N
usda_doi<-st_transform(usda_doi_needproj,crs = 6346)

#the state parks data was provided by folks at the state parks in august of 2020
#st_prk<-read_sf("C:\\Users\\thebrain\\Dropbox\\GRIN\\StateParkInvestigation\\FilesSent_0818\\dprburnrecords\\DPR_areaburned0820.shp")

# this counts the number of times a geometry has been burned
#trying to get to the bottom of what a "unique burn" is, given overlaps

num_burns_pergeom<-usda_doi %>% group_by(GeometryID) %>%
  summarise(n = n())

num_treatments<-usda_doi %>% group_by(Treatmen_1) %>%
  summarise(n = n())

num_treat_geoms<-usda_doi %>% group_by(GeometryID,Treatmen_1) %>%
  summarise(n = n())

num_treatments_ingeom<-usda_doi %>% group_by(GeometryID,F_FY_COMPL) %>%
  count(Treatmen_1)

#think a fed unique burn = a unique GeometryID + Treatmen_1 for fed
#think a state unique burn = unique object ID 



#break usda & state into singlepart, this makes the identifying and counting
#of overlapping polygons more straightforward
#fed_singpart <- st_cast(usda_doi,"POLYGON")
#st_singpart <- st_cast(st_prk,"POLYGON")

#project both to UTM 17N
#fed_sing_utm<-st_transform(fed_singpart,crs = 6346)
#st_sing_utm<-st_transform(st_singpart,crs = 6346)

#add new object ids to identify unique treatments & polygons
#fed_sing_utm$OID_sing<- 1:nrow(fed_sing_utm)
#st_sing_utm$OID_sing<- 1:nrow(st_sing_utm)+2000


usda_doi$ac<-usda_doi$TreatmentA
usda_doi$date<-as.Date(usda_doi$ActualComp)
usda_doi$year<-format(usda_doi$date,format="%Y")
#for the simplest merging, need cols to be the same
#since we don't need many columns, trim it down
fed_tomerge<-usda_doi[,c("geometry","year","OBJECTIDxx","ac")]
#colnames(fed_tomerge)<-c("OID_sing","Year","geomid","ac","geometry")

#cleaning up records that aren't correct
clean<-fed_tomerge[which(fed_tomerge$year!=0 | fed_tomerge$year != "" | fed_tomerge$OBJECTIDxx != " "| fed_tomerge$ac!=0, ),]
#get extent for creating raster

#right type
clean$year<-as.integer(clean$year)
#get rid of after 2010 options, gonna use the burn permit data for that
clean_2010on<-clean[clean$year>2009,]

colnames(clean_2010on)<-c("geometry","year","id","acres")



# withdupcol<-fed_tomerge %>% mutate(hash = str_c(Year, geomid, ac)) %>%
#   group_by(hash) %>% 
#   mutate(duplication_id = seq(n())) %>%
#            ungroup ()
# 
# fed_nodups<-withdupcol[withdupcol$duplication_id==1,]




# st_tomerge<-st_sing_utm[,c("OID_sing","Year","ID","Acres")]
# #not that geomid here was just ID in the original state parks file
# colnames(st_tomerge)<-c("OID_sing","Year","geomid","ac","geometry")

#combined into a single shapefile
# fed_state<-rbind(fed_tomerge,st_tomerge)
# fed_state$ac<-as.integer(fed_state$ac)




#read in burn permit data
burn_perm<-read_sf("C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\SEBurnPermits_NCSUreq\\SEBurnPermits_NCSUreq.shp")

nrow(burn_perm)

burn_perm_utm<-st_transform(burn_perm,crs = 6346)
burn_perms_with_acre<-burn_perm_utm[burn_perm_utm$ACRES>5,]
clean_perms_2010on<-burn_perms_with_acre[burn_perms_with_acre$YEAR>2009,]

write_sf(clean_perms_2010on,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\clean_perms2010on.shp",overwrite=TRUE)
#output this shapefile, then took sapps surf man, selected NPS & USFS, and then selected the burn permits that were
#DISJOINT with NPS & USFS areas
#very few were within NPS & USFS areas, but reading back in the disjoint ones (most of the permits)
cln_perms_2010on_notfed<-read_sf("C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\cln_perms_NOTfsnp_2010on_v3.shp")

final_objid<-(500000+nrow(cln_perms_2010on_notfed))-1
cln_perms_2010on_notfed$objid<-c(500000:final_objid)
cln_perms_2010on_notfed$utm_x<-st_coordinates(cln_perms_2010on_notfed)[,1] #get xs
cln_perms_2010on_notfed$utm_y<-st_coordinates(cln_perms_2010on_notfed)[,2] #get ys
cln_perms_2010on_notfed$radius<-sqrt(as.numeric(cln_perms_2010on_notfed$ACRES*4046.86))/2
cln_perms_2010on_notfed$yPlus<-as.double(cln_perms_2010on_notfed$utm_y+cln_perms_2010on_notfed$radius)
cln_perms_2010on_notfed$xPlus<-as.double(cln_perms_2010on_notfed$utm_x+cln_perms_2010on_notfed$radius)
cln_perms_2010on_notfed$yMinus<-as.double(cln_perms_2010on_notfed$utm_y-cln_perms_2010on_notfed$radius)
cln_perms_2010on_notfed$xMinus<-as.double(cln_perms_2010on_notfed$utm_x-cln_perms_2010on_notfed$radius)


square<-cbind(cln_perms_2010on_notfed$xMinus, cln_perms_2010on_notfed$yPlus,  # NW corner
              cln_perms_2010on_notfed$xPlus, cln_perms_2010on_notfed$yPlus,  # NE corner
              cln_perms_2010on_notfed$xPlus, cln_perms_2010on_notfed$yMinus, # SE corner
              cln_perms_2010on_notfed$xMinus, cln_perms_2010on_notfed$yMinus, # SW corner
              cln_perms_2010on_notfed$xMinus, cln_perms_2010on_notfed$yPlus)  # NW corner again - close polygon

a <- vector('list', length(2))

# loop through each centroid value and create a polygon
# this is where we match the ID to the new plot coordinates
for (i in 1:nrow(cln_perms_2010on_notfed)) {  # for each for in object centroids
  a[[i]]<-Polygons(list(Polygon(matrix(square[i, ], ncol=2, byrow=TRUE))), cln_perms_2010on_notfed$objid[i]) 
  # make it an Polygon object with the Plot_ID from object ID
}

# convert a to SpatialPolygon and assign CRS
polysB<-SpatialPolygons(a,proj4string=CRS(SRS_string = "EPSG:6346"))

burn_perm_sqs<-st_as_sf(polysB)
burn_perm_sqs$year<-cln_perms_2010on_notfed$YEAR
burn_perm_sqs$id<-cln_perms_2010on_notfed$objid
burn_perm_sqs$acres<-cln_perms_2010on_notfed$ACRES

write_sf(burn_perm_sqs,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\burn_perm_sqrs_2010on_nofed.shp",overwrite=TRUE)



# 
# 
# burn_perms_to_poly <- function(input_df) {
#   
# 
#   #for (i in 1:nrow(input_df)){
# 
#     
#     square=cbind(xMinus,yPlus,  # NW corner
#                  xPlus, yPlus,  # NE corner
#                  xPlus,yMinus,  # SE corner
#                  xMinus,yMinus, # SW corner
#                  xMinus,yPlus)
#     
#     new_poly <- vector('list', length(1))
#     
#     new_poly[[1]]<-Polygons(list(Polygon(matrix(square, ncol=2, byrow=TRUE))),1)
#     sf_obj<-st_as_sf(SpatialPolygons(new_poly))#, proj4string=CRS(as.character(st_crs(6346))))
#     sf_obj<-st_crs(input_df)
#     return(sf_obj)
# }
# 
# 
# 
# first_poly<-burn_perms_to_poly(burn_perm_utm)
# 
# burn_polys<-rbind(first_poly,sf_obj)
#   
  


#now, want to create count raster - merge fs/nps & perm data
test<-rbind(clean_2010on,burn_perm_sqs)
test_poly<-st_cast(test, "MULTIPOLYGON") %>% st_cast("POLYGON")


write_sf(test,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\test_comb_fed_perms.shp",overwrite=TRUE)
  
ext<-st_bbox(test_poly)

## create raster
rast <- raster(xmn = ext[1],   # set minimum x coordinate
                      xmx = ext[3],    # set maximum x coordinate
                      ymn = ext[2],     # set minimum y coordinate
                      ymx = ext[4],     # set maximum y coordinate
                      res = c(500,500),
                      crs = crs(fed_sing_utm))#crs(clean)$proj4string)) 


#change these
cnt_overlap<-overlap.weight(rast, test_poly, count = TRUE)
yr_rng_min <- rasterize(clean, rast, field="Year", fun=min)
yr_rng_max <- rasterize(clean, rast, field="Year", fun=max)
avg_ac<- rasterize(clean, rast, field="ac", fun=mean)


yr_rng<-yr_rng_max-yr_rng_min

rx_int<-yr_rng/cnt_overlap


max(na.omit(getValues(yr_rng)))#just checking to see vals reasonable
min(na.omit(getValues(yr_rng)))
min(na.omit(getValues(yr_rng_min))) #1995 first record
max(na.omit(getValues(yr_rng_max))) #2022 last record 

#could do burn intervals by decade 1992-2002,2003-2012, 2013-2022

writeRaster(yr_rng,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\yr_rng_500m_v2.tif")
writeRaster(cnt_overlap,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\cnt_perm_fed_500m.tif",overwrite=TRUE)
writeRaster(rx_int,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\rx_int_500m.tif",overwrite=TRUE)
writeRaster(avg_ac,"C:\\Users\\thebrain\\Dropbox\\GRIN\\data\\avg_ac_500m.tif",overwrite=TRUE)
plot(polys_t, add = TRUE)


