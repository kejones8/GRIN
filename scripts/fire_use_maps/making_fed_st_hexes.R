#combining fed/state burn perms into hexes

#GET HEXES
hex<-read_sf("data\\fire_use_areas\\hex_grid_2500.shp")
st_crs(hex)

hexy<-hex[,c("id")]

#get fed data
fed<-read_sf("data\\usda_doi_fire_treat_singpart.gpkg")
fed_utm<-st_transform(fed,st_crs(hex))


#get st data
st<-read_sf("data\\burn_perm_sqrs_2010on_nofed.shp")
st_utm<-st_transform(st,st_crs(hex))

test_fed_hex<-st_join(hex,fed_utm)
#by hex id, tally object ids to give counts of burns for individual hexes
counting_fed<-test_fed_hex %>% group_by(id) %>% mutate(fed_count=n_distinct(OBJECTID,na.rm=TRUE))

fed_hex_df<-as.data.frame(unique(counting_fed[,c("id","fed_count")]))
fed_hex_df$geometry<-NULL

#do this for state, too
test_st_hex<-st_join(hex,st_utm)
counting_st<-test_st_hex %>% group_by(id.x) %>% mutate(st_count=n_distinct(id.y,na.rm=TRUE))

st_hex_df<-as.data.frame(unique(counting_st[,c("id.x","st_count")]))
st_hex_df$geometry<-NULL


mergey<-merge(st_hex_df,fed_hex_df,by.x="id.x",by.y="id")
mergey$rx_count<-mergey$st_count+mergey$fed_count


hmm<-merge(hexy,mergey,by.x="id",by.y="id.x")

write_sf(hmm,"data\\fire_use_areas\\fed_state_burncounts_2010_2021.shp",overwrite=TRUE)
