library(sf)
# read water bodies cycle 1
wb = read_sf("../../../../Downloads/EA_WFDRiverWaterBodiesCycle1_SHP_Full/data/WFD_River_Water_Bodies_Cycle_1.shp") %>%
  rename(WBID = ea_wb_id) %>%
  select(WBID,name,geometry)
# read lakes
lakes = read_sf("../../../../Downloads/EA_WFDLakeWaterBodiesCycle1_SHP_Full/data/WFD_Lake_Water_Bodies_Cycle_1.shp") %>%
  rename(WBID = ea_wb_id) %>%
  select(WBID,name,geometry)
# read canals
canals = read_sf("../../../../Downloads/EA_WFDArtificialWaterBodiesCanalsCycle1_SHP_Full/data/WFD_Artificial_Water_Bodies_Canals_Cycle_1.shp") %>%
  rename(WBID = ea_wb_id) %>%
  select(WBID,name,geometry)
# read surfaace water bodis
surface = read_sf("../../../../Downloads/EA_WFDArtificialWaterBodiesSurfaceWaterTransfersCycle1_SHP_Full/data/WFD_Artificial_Water_Bodies_Surface_Water_Transfers_Cycle_1.shp") %>%
  rename(WBID = ea_wb_id) %>%
  select(WBID,name,geometry)

wb_all = rbind(wb,lakes,canals,surface)

wb_thames = read_sf("data/web/wb_cat_class.geojson") %>% st_drop_geometry()
wb_test = inner_join(wb_thames,wb_all, by = "WBID") %>%select(-c("name.y")) %>% st_as_sf() %>%  st_transform(crs = 4326)
plot(wb_test$geometry)

write_sf(wb_test, "data/web/lines.geojson")
