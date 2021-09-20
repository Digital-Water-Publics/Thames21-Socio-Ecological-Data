##### DOWNLOAD WATERBODY DATA HOSTED ON VIA CDE package

#OPTIONS

#1 using london management catchment
thames = search_names(string="London", column="MC")

# use old sf file
mapbox = read_sf("mapbox/mapbox_primary.shp")

mapbox_min = mapbox %>% distinct(watrbdy,WtrbdID)


lark_hawstead<-get_status(ea_name="GB106039023260", column="WBID")

lark_OC_rivers<-get_status(ea_name="London", column="MC", startyr=2019, endyr=2019, type="River")
