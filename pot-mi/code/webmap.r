####
####
#### AIM: Prepare twitter data for web map
####
####
if (file.edit("data/web/oc.geojson")) {

} else {
  # From continous to categorical -------------------------------------------

  clean_senti = readRDS("data/river_queries/clean_senti_polatiry.rds")
  summary(clean_senti$senti_score)

  # read water bodies
  thames_wb = read_sf("data/thames_river.geojson") %>%
    st_drop_geometry() %>%
    select(WBID, name, OC, OC_num, MC, MC_num, RBD, RBD_num)

  # join with main data to get categorical variables
  clean_senti = inner_join(clean_senti, thames_wb)

  # aggregate data
  MC = as.data.frame(aggregate(senti_score  ~ MC_num, clean_senti, mean))
  OC = as.data.frame(aggregate(senti_score  ~ OC_num, clean_senti, mean))
  RBD = as.data.frame(aggregate(senti_score  ~ RBD_num, clean_senti, mean))
  WB = as.data.frame(aggregate(senti_score  ~ WBID, clean_senti, mean))

  make_that_data_categorial = function(data) {
    data$group[data$senti_score >= Percentile_00 &
                 data$senti_score <  Percentile_33]  = 1
    data$group[data$senti_score >= Percentile_33 &
                 data$senti_score <  Percentile_67]  = 2
    data$group[data$senti_score >= Percentile_67 &
                 data$senti_score <= Percentile_100] = 3
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
    rename(MC = label) %>%
    select(MC, MC_num, senti_score, geometry)
  mc_sf$MC = gsub("_", " ", mc_sf$MC)
  write_sf(mc_sf, "data/web/mc_cat.geojson")

  # generate oc geojson
  oc_sf = read_sf("data.oc_test.geojson") %>%
    select(-c(val)) %>%
    rename(OC_num = notation) %>%
    right_join(OC) %>%
    mutate(group = as.numeric(group)) %>%
    rename(OC = label) %>%
    select(OC, OC_num, senti_score, group,geometry)

  write_sf(oc_sf, "data/web/oc_cat.geojson")

  # generate wbid geojson
  wb_sf = read_sf("data/web/wb_class.geojson") %>%
    select(-c(senticent_polarity))%>%
    mutate(senti_score = 0) %>% right_join(WB)
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



wbids = as.data.frame(unique(clean_senti$WBID))
colnames(wbids) = "WBID"

for (i in 1:nrow(wbids)) {
  ####
  ####
  #### STEP 1: Filter sentiment data for waterbody & set up file path
  ####
  ####
  river = clean_senti %>% filter(WBID == wbids$WBID[i])
  path = paste0("open-data/",river$RBD[1],"/",river$WBID[1])
  ####
  ####
  #### STEP 2: Generate sentiment and textual datasets for the waterbody
  ####
  ####
  # 2.1 Sentiment Polarity score
  river_mean_senti = river %>% mutate(created_at = ymd_hms(created_at)) %>%
    group_by(created_at) %>%
    summarise(mean_senti = mean(senti_score),)
  write.csv(river_mean_senti,paste0(path,"-polarity-score.csv"))
  # 2.2 Emotional frequency in tweets
  corpus = corpus(river$clean_tweet)
  emo_freq = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text) %>%
    inner_join(get_sentiments("nrc") %>%
    filter(sentiment %ni% c("positive", "negative"))) %>%
    count(sentiment) %>%
    mutate(percent = (senti_word$n / sum(senti_word$n)) * 100) %>%
    mutate(percent = round(percent, 2))
  write.csv(emo_freq,paste0(path,"-emolex-frequency.csv"))
  #2.3 Common nounphrases
  parsed = spacy_extract_nounphrases(
    river$clean_tweet,
    output = c("data.frame"),
    multithread = TRUE
  ) %>%
    filter(length > 2) %>%
    select(text) %>%
    mutate(text = str_remove_all(text, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
    mutate(text = str_remove_all(text, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(text = str_remove_all(text, regex("#[[:alnum:]_]+"))) %>%
    mutate(text = str_remove_all(text, regex("[[:punct:]]"))) %>%
    mutate(text = str_remove_all(text, regex("^RT:? "))) %>%
    mutate(text = str_replace(text, "amp", "and")) %>%
    anti_join(stop_words, by = c("text" = "word")) %>%
    mutate(text = str_to_lower(text)) %>%
    # Remove any trailing whitespace around the text
    mutate(text = str_trim(text, "both")) %>%
    count(text) %>%
    arrange(desc(n))
  write.csv(parsed,paste0(path,"-common-nounphrase.csv"))
  ####
  ####
  #### STEP 3: Fetch ecological data from the EA for each waterbody
  ####
  ####
  #3.1 Get Ecological Classification
  wb_class = get_wb_classification(string = river$WBID[1], column = "WB") %>%
    filter(Classification.Item == "Ecological") %>%
    filter(Status != "Does not require assessment") %>%
  filter(Cycle == 2)

  #3.2 Get Reasons for not achieving good
  wb_rnag = get_wb_rnag(string = river$WBID[1], column = "WB")
  #3.3 Get Reasons for not achieving good
  wb_sf = get_wb_sf(string = river$WBID[1], column = "WB")

}


