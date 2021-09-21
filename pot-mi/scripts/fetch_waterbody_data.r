##### DOWNLOAD WATERBODY DATA HOSTED ON VIA CDE package


#OPTIONS
geo = read_sf("../../../../Downloads/WaterBody-6919a77b8ec34ffbce830e99ff5d0003 (1).geojson")
geo = geo %>% select(label,notation)
#get thames river catchment data
thames = search_names(string="Thames", column="RBD") %>% rename(label = name)
waterbodies= waterbodies %>% rename(label = name)

thames_spatial = left_join(waterbodies,geo) %>% st_as_sf()

options(al=1)
plot(al)

tmap_mode("view")
tm_shape(al) + tm_fill("MC", style = "cat") + tmap_options(check.and.fix = TRUE)

# use old sf file
mapbox = read_sf("mapbox/mapbox_primary.shp")

mapbox_min = mapbox %>% distinct(watrbdy,WtrbdID)

unique(sf::st_geometry_type(thames_spatial))
