####
####
#### AIM: Prepare twitter data for web map
####

if (file.edit("data/web/oc.geojson")) {

} else {
  # From continous to categorical -------------------------------------------

  clean_senti = readRDS("data/river_queries/clean_senti_polatiry.rds")
  summary(clean_senti$senticent_polarity)

  #Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
  #-3.33573 -0.05964  0.06171  0.11009  0.30321  7.85655

  Percentile_00 = min(clean_senti$senticent_polarity)
  Percentile_33 = quantile(clean_senti$senticent_polarity, 0.333333)
  Percentile_67 = quantile(clean_senti$senticent_polarity, 0.666667)
  Percentile_100 = max(clean_senti$senticent_polarity)
  RB = rbind(Percentile_00, Percentile_33, Percentile_67, Percentile_100)

  # read water bodies
  thames_wb = read_sf("data/thames_river.geojson") %>%
    st_drop_geometry() %>%
    select(WBID, name, OC, OC_num, MC, MC_num, RBD, RBD_num)

  # join with main data to get categorical variables
  clean_senti = inner_join(clean_senti, thames_wb)

  # aggregate data
  MC = as.data.frame(aggregate(senticent_polarity  ~ MC_num, clean_senti, mean))
  OC = as.data.frame(aggregate(senticent_polarity  ~ OC_num, clean_senti, mean))
  RBD = as.data.frame(aggregate(senticent_polarity  ~ RBD_num, clean_senti, mean))
  WB = as.data.frame(aggregate(senticent_polarity  ~ WBID, clean_senti, mean))

  make_that_data_categorial = function(data) {
    data$group[data$senticent_polarity >= Percentile_00 &
                 data$senticent_polarity <  Percentile_33]  = 1
    data$group[data$senticent_polarity >= Percentile_33 &
                 data$senticent_polarity <  Percentile_67]  = 2
    data$group[data$senticent_polarity >= Percentile_67 &
                 data$senticent_polarity <= Percentile_100] = 3
    return(data)
  }

  MC = make_that_data_categorial(data = MC)
  OC = make_that_data_categorial(data = OC)
  RBD = make_that_data_categorial(data = RBD)
  WB = make_that_data_categorial(WB)
  # Generate geojson files for various catchment geogrpahies ----------------

  # generate mc geojson
  mc_sf = read_sf("data/mc_test.geojson") %>%
    select(-c(val)) %>%
    rename(MC_num = notation) %>%
    right_join(MC) %>%
    mutate(group = as.numeric(group)) %>%
    rename(MC = label) %>%
    select(MC, MC_num, senticent_polarity, group, geometry)
  mc_sf$MC = gsub("_", " ", mc_sf$MC)
  write_sf(mc_sf, "data/web/mc_cat.geojson")

  # generate oc geojson
  oc_sf = read_sf("data.oc_test.geojson") %>%
    select(-c(val)) %>%
    rename(OC_num = notation) %>%
    right_join(OC) %>%
    mutate(group = as.numeric(group)) %>%
    rename(OC = label) %>%
    select(OC, OC_num, senticent_polarity, group,geometry)

  write_sf(oc_sf, "data/web/oc_cat.geojson")

  # generate wbid geojson
  wb_sf = read_sf("data/web/wb_class.geojson") %>% select(-senticent_polarity) %>% right_join(WB)
  write_sf(wb_sf, "data/web/wb_cat_class.geojson")



  # Search centroid data -----------------------------------------------------------
  mc_sf = read_sf("data/web/mc.geojson")
  mc_cent = st_centroid(mc_sf) %>% rename(name = MC) %>% select(name, geometry)

  oc_sf = read_sf("data/web/oc.geojson")
  oc_cent = st_centroid(oc_sf) %>% rename(name = OC) %>% select(name, geometry)

  wb_sf = read_sf("data/web/wb.geojson")
  wb_cent = st_centroid(wb_sf) %>% select(name, geometry)

  wb_sf_class = inner_join(wb_sf, wb_class)
  write_sf(wb_sf_class, "data/web/wb_class.geojson")

  all_cent = rbind(wb_cent, oc_cent, mc_cent)
  write_sf(all_cent, "data/web/search_centroids.geojson")

}
