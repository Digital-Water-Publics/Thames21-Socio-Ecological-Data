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

wb_all = rbind(wb,lakes,canals)

wb_thames = read_sf("data/web/wb.geojson") %>% st_drop_geometry()

wb_test = inner_join(wb_all,wb_thames, by = "WBID")
