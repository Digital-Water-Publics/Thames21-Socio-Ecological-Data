####
####
#### AIM: Prepare twitter data for web map
####
####

# From continous to categorical -------------------------------------------
make_that_data_categorial = function(data) {
  data$group[data$senti_score >= -1.1 &
               data$senti_score <  0.26]  = 1
  data$group[data$senti_score >= 0.26 &
               data$senti_score <  0.4]  = 2
  data$group[data$senti_score >= 0.4 &
               data$senti_score <= 2.9] = 3
  return(data)
}

mine_query_sheet = read.csv("mine_query_sheet.csv")
sf::sf_use_s2(FALSE)
WBIDS = as.data.frame(unique(mine_query_sheet$WBID))
colnames(WBIDS) = "WBID"

wbs = inner_join(WBIDS, wb_all)
wbs = wbs[!duplicated(wbs$name),]
wbs = st_as_sf(wbs)

WB = as.data.frame(aggregate(senti_score  ~ WBID, clean_senti, mean))
WB = make_that_data_categorial(WB)
WB_final = inner_join(wbs, WB)

write_sf(WB_final, "data/web/wb_line_groups.geojson")

# centroids
wb_cent = st_centroid(WB_final) %>% select(name, WBID, geometry)
sf::write_sf(wb_cent, "data/web/wb_line_centroids.geojson")
