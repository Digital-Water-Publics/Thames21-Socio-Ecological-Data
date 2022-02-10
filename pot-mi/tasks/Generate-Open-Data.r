clean_senti = readRDS("data/river_queries/clean_senti.RDS")
wbids = as.data.frame(unique(clean_senti$WBID))
colnames(wbids) = "WBID"

# read data for waterbodies
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
#clean environment
rm(wb,lakes,canals,surface)

for (i in 1:nrow(wbids)) {
  ####
  ####
  #### STEP 1: Filter sentiment data for waterbody & set up file path
  ####
  ####
  message("STEP 1: Filter sentiment data for waterbody & set up file path")
  river = clean_senti %>% filter(WBID == wbids$WBID[i])
  path = paste0("Open-Data/",river$RBD[1],"/")
  setwd(path)
  dir.create(river$WBID[1])
  path = paste0("Open-Data/",river$RBD[1],"/",river$WBID[1])
  setwd("../../")
  message(paste0("Generating for ", river$WBID[i], "path = ", path ))
  ####
  ####
  #### STEP 2: Generate sentiment and textual datasets for waterbody
  ####
  ####
  message("STEP 2: Generate sentiment and textual datasets for waterbody")
  # 2.1 Sentiment Polarity score
  river_mean_senti = river %>% mutate(created_at = as.Date(ymd_hms(created_at))) %>%
    group_by(created_at) %>%
    summarise(mean_senti = mean(senti_score),)
  write.csv(river_mean_senti,paste0(path,"/polarity-score.csv"))
  # 2.2 Emotional frequency in tweets
  corpus = corpus(river$clean_tweet)
  emo_freq = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text) %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %ni% c("positive", "negative"))) %>%
    count(sentiment) %>%
    mutate(percent = (n / sum(n)) * 100) %>%
    mutate(percent = round(percent, 2))
  write.csv(emo_freq,paste0(path,"/emolex-frequency.csv"))
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
  write.csv(parsed,paste0(path,"/common-nounphrase.csv"))
  ####
  ####
  #### STEP 3: Fetch ecological data from the EA for each waterbody
  ####
  ####
  message("STEP 3: Fetch ecological data from the EA for each waterbody")
  #3.1 Get Ecological Classification
  wb_class = get_wb_classification(string = river$WBID[1], column = "WB") %>%
    filter(Classification.Item == "Ecological") %>%
    filter(Status != "Does not require assessment") %>%
    filter(Cycle == 2) %>%
    select(Year,Status)  %>%
    pivot_wider(names_from = "Year",values_from = "Status")
  write.csv(wb_class,paste0(path,"/eco-class.csv"))
  #3.2 Get Reasons for not achieving good
  wb_rnag = get_wb_rnag(string = river$WBID[1], column = "WB") %>%
    filter(Reason.Type == "RFF") %>%
    select(WBID,Category,Category.Certainty,Business.Sector,Activity,Activity.Certainty) %>%
    rename(`Business Sector` = Business.Sector,
           `Category Certainty` = Category.Certainty,
           `Activity Certainty` = Activity.Certainty,)
  write.csv(wb_rnag,paste0(path,"/rnag.csv"))
  #3.3 Get waterbody polygon
  wb_sf = get_wb_sf(string = river$WBID[1], column = "WB")
  write_sf(wb_sf,paste0(path,"/wb_poly.geojson"))
  #3.4 Get waterbody line
  tryCatch(
    expr = {
      wb_line = wb_all %>% filter(river$WBID[1] == WBID)
      write_sf(wb_line,paste0(path,"/wb_line.geojson"))
    },
    error = function(e){
      message("No waterbody line found, moving on")
    }
  )
}