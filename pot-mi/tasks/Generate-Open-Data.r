#################################################################
##                      primary functions                      ##
#################################################################
clean_tweets_sentiment = function(x) {
  x %>% mutate(
    clean_tweet = text %>%
      str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
      # Remove mentions e.g. "@my_account"
      str_remove_all("@[[:alnum:]_]{4,}") %>%
      # Remove hashtags
      str_remove_all("#[[:alnum:]_]+") %>%
      # Replace "&" character reference with "and"
      str_replace_all("&amp;", "and") %>%
      # Remove puntucation, using a standard character class
      str_remove_all("[[:punct:]]") %>%
      # Remove "RT: " from beginning of retweets
      str_remove_all("^RT:? ") %>%
      # Replace any newline characters with a =space
      str_replace_all("\\\n", " ") %>%
      # Make everything lowercase
      str_to_lower() %>%
      # Remove any trailing whitespace around the text
      str_trim("both")
  )
}

clean_user_location = function(data) {
  data %>% mutate(
    clean_location = user_location %>%
      str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>% #remove links
      str_remove_all("#[[:alnum:]_]+") %>% # Remove hashtags
      str_remove_all("^\\s*<U\\+\\w+>\\s*") %>%
      str_replace_na("") %>% # convert NAs
      str_replace_all("^$", "") # convert empty characters
  )
}

`%ni%` = function (x, table)
  is.na(match(x, table, nomatch = NA_integer_))

report = function(x) {
  X111021_mine_query_sheet_hp = read.csv("data/111021_mine_query_sheet_hp.csv")
  en_tweets %>%
    group_by(WBID) %>%
    count(WBID) %>%
    right_join(filter(ea_wbids, RBD == "Thames")) %>%
    arrange(desc(n)) %>%
    right_join(X111021_mine_query_sheet_hp) %>%
    select(WBID, name, n, mine_query) %>%
    kableExtra::kable() %>%
    kableExtra::kable_material_dark()
}

#################################################################
##                      Read Data                             ##
#################################################################
clean_senti = readRDS("data/river_queries/clean_senti.RDS")
query = as.data.frame(unique(clean_senti$query))
colnames(query) = "query"

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

#################################################################
##                      loop through wbid                      ##
#################################################################
for (i in 1:nrow(query)) {
  ####
  ####
  #### STEP 1: Filter sentiment data for waterbody & set up file path
  ####
  ####
  message("STEP 1: Filter sentiment data for waterbody & set up file path")
  river = clean_senti %>% filter(query == query[i])
  path = paste0("Open-Data/",river$RBD[1],"/")
  setwd(path)
  if(file.exists(path) == TRUE){
    path = paste0(path,i)
    dir.create(paste0(path,i))
  } else {
    dir.create(river$WBID[1])
  }

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
  # #3.1 Get Ecological Classification
  wb_class = get_wb_classification(string = river$WBID[1], column = "WB") %>%
    filter(Classification.Item == "Ecological") %>%
    filter(Status != "Does not require assessment") %>%
    filter(Cycle == 2) %>%
    select(Year,Status)  %>%
    pivot_wider(names_from = "Year",values_from = "Status")
  write.csv(wb_class,paste0(path,"/eco-class.csv"))
  #3.2 Get Reasons for not achieving good
  wb_rnag = get_wb_rnag(string = river$WBID[1], column = "WB") %>%
    select(Category,Activity) %>%
    group_by(Category,Activity) %>%
    count() %>%
    filter(Activity != "Not applicable") %>%
    rename(source = Category, target = Activity, value = n)
  write.csv(wb_rnag,paste0(path,"/rnag.csv"), row.names = FALSE)
  #3.3 Get waterbody polygon
  wb_sf = get_wb_sf(string = river$WBID[1], column = "WB")
  write_sf(wb_sf,paste0(path,"/wb_poly.geojson"))
  # #3.4 Get waterbody line
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

