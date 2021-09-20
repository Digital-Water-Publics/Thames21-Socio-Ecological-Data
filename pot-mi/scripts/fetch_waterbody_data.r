##### DOWNLOAD WATERBODY DATA HOSTED ON VIA CDE package

#OPTIONS

#get thames river catchment data
thames = search_names(string="Thames", column="RBD")

# use old sf file
mapbox = read_sf("mapbox/mapbox_primary.shp")

mapbox_min = mapbox %>% distinct(watrbdy,WtrbdID)
