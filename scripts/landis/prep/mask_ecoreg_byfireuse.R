library(sf)
library(sp)
library(raster)

ecoreg<-raster("data\\landis_rasters\\Ecos11_NCLD.tif")
depth<-raster("data\\landis_rasters\\Depth_91120.tif")


lowfire<-read_sf("data\\fire_use_areas\\low_fire_nogaps_utm.shp") #this one had vertices & gaps cleaned up
lowfire_utmnad83<-st_transform(lowfire,crs(ecoreg))
lowfire_sp<-as_Spatial(lowfire_utmnad83)

interfire<-read_sf("data\\fire_use_areas\\inter_fire_use.shp")
interfire_utmnad83<-st_transform(interfire,crs(ecoreg))
interfire_sp<-as_Spatial(interfire_utmnad83)

highfire<-read_sf("data\\fire_use_areas\\high_fire_use_nogaps.shp")
highfire_utmnad83<-st_transform(highfire,crs(ecoreg))
highfire_sp<-as_Spatial(highfire_utmnad83)
#ecoreg_proj<-projectRaster(from=ecoreg,crs=crs(lowfire))

e<-extent(ecoreg)
blank_rast<-raster(e,res=res(ecoreg),vals=1) #set to extent of ecoreg, res of ecoreg, & set values to 1
crs(blank_rast)<-crs(ecoreg)

#not sure why the update didn't work, but whatevs.
low_fire_rast<-rasterize(x=lowfire_sp,y=blank_rast, mask=TRUE)#,update=TRUE,updateValue=2,na.rm=TRUE,getCover=FALSE)
inter_fire_rast<-rasterize(x=interfire_sp,y=blank_rast, mask=TRUE)
high_fire_rast<-rasterize(x=highfire_sp,y=blank_rast, mask=TRUE)


#now, mask unwanted cells form ecoreg rast
low_ecoreg<-mask(ecoreg,low_fire_rast)
low_ecoreg[is.na(low_ecoreg)]<-1

inter_ecoreg<-mask(ecoreg,inter_fire_rast)
inter_ecoreg[is.na(inter_ecoreg)]<-1

high_ecoreg<-mask(ecoreg,high_fire_rast)
high_ecoreg[is.na(high_ecoreg)]<-1

writeRaster(low_ecoreg,"data\\landis_rasters\\lowfire_ecoreg.tif",datatype='INT2S',overwrite=TRUE)
writeRaster(inter_ecoreg,"data\\landis_rasters\\interfire_ecoreg.tif",datatype='INT2S',overwrite=TRUE)
writeRaster(high_ecoreg,"data\\landis_rasters\\highfire_ecoreg.tif",datatype='INT2S',overwrite=TRUE)
