##### DOWNLOAD WATERBODY DATA HOSTED ON VIA CDE package

#OPTIONS

geo = geo %>% select(label,notation)
#get thames river catchment data
thames = search_names(string="Thames", column="RBD") %>% rename(label = name)
thames_spatial = right_join(thames,geo) %>% st_as_sf()

options(thames_spatial=1)
plot(thames_spatial)

tmap_mode("plot")
tm_shape(thames_spatial) + tm_fill("MC", style = "cat") + tmap_options(check.and.fix = TRUE)

# use old sf file
mapbox = read_sf("mapbox/mapbox_primary.shp")

mapbox_min = mapbox %>% distinct(watrbdy,WtrbdID)

unique(sf::st_geometry_type(thames_spatial))
