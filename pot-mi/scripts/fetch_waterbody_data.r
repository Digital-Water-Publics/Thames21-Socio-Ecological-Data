##### DOWNLOAD WATERBODY DATA HOSTED ON VIA CDE package


#OPTIONS
geo = read_sf("../../../../Downloads/reasons_for_not_achieving_good/reasons_for_not_achieving_good_RBD_6.csv")
geo = geo %>% filter(geo$`Classification Year` == 2016)

geo = geo %>% mutate(label = geo$`water body`) %>% mutate(wbid = waterbodies$`Water body id`) %>% select(wbid,label) %>% select(wbid,label)


waterbodies = merge(geo,aa)

select(label,wbid,mine_query)

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
