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
setwd("~/pot-mi/pot-mi")

##################################################################
##                             TODO                             ##

#1 Rerun clean script and include user_location --------------------------- sf package, geocode location and keep NA
#2 nounphrase topic modelling ----------------------------------------------
#3 Sentiment Function and Comparison ---------------------------------------
#4 Find other river lines ------------------------------------------------
##################################################################

if(file.exists("data/river_queries/raw_data.RDS")){
  raw_data =readRDS("data/river_queries/raw_data.RDS")
} else {
  # # read and clean data
  setwd("data/river_queries/")
  # Read min data -----------------------------------------------------------
  # create df of raw csvs
  loop_csv = as.data.frame(grep(
    list.files(full.names = TRUE),
    pattern = '*GB',
    invert = TRUE,
    value = TRUE
  ))
  colnames(loop_csv) = "filename"
  loop_csv = subset(loop_csv, !grepl("rds",filename))

  # Loop and clean data -----------------------------------------------------
  n = 1
  for (i in 1:nrow(loop_csv)) {
    files = nrow(loop_csv)
    path = loop_csv$filename[i]
    csv = read.csv(path)
    csv = csv %>%
      select(
        c(
          "tweet_id",
          "created_at",
          "lang",
          "like_count",
          "retweet_count",
          "user_username",
          "text",
          "possibly_sensitive",
          "author_id",
          "user_location",
          "WBID"
        )
      )

    wbid = csv$WBID[1]

    clean_user_location = function(data){
      data %>% mutate(
        clean_location = user_location %>%
          str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>% #remove links
          str_remove_all("#[[:alnum:]_]+") %>% # Remove hashtags
          str_remove_all("^\\s*<U\\+\\w+>\\s*") %>%
          str_replace_na("") %>% # convert NAs
          str_replace_all("^$","") # convert empty characters
      )
    }


    wbid = clean_user_location(wbid) %>%
      geocode(clean_location) %>%
      select(tweet_id,clean_location,text,lat,long) %>%
      na.omit(lat) %>%
      st_as_sf(coords=c("lat","long"), crs = 4326)


    write.csv(csv, paste0(wbid, ".csv"))
    message(paste0(n, "/", files, " files cleaned. Cleaning the next file"))
    n = n + 1
  }

  # bind data
  min_files = list.files(pattern = "*GB")
  raw_min_data = lapply(min_files, function(i) {
    read.csv(i)
  })
  raw_data = do.call(rbind.data.frame, raw_min_data) %>%
    distinct(tweet_id, .keep_all = TRUE)

  clean_tweet = raw_data %>% clean_tweets_sentiment()
  saveRDS(clean_tweet, "data/river_queries/raw_data.RDS")
}



##################################################################
##                          Other Functions                   ##


reduce_noise = function(x){
  print(table(x$lang))
  clean_tweets = x %>% filter(lang == "en") %>%
    distinct(tweet_id, .keep_all = TRUE) %>%
    filter(str_detect(text,"^RT:? ") == FALSE)
  return(clean_tweets)
}
en_tweets = reduce_noise(raw_data)

report = function(x){
  X111021_mine_query_sheet_hp = read.csv("data/111021_mine_query_sheet_hp.csv")
  en_tweets %>%
    group_by(WBID) %>%
    count(WBID) %>%
    right_join(filter(ea_wbids, RBD == "Thames")) %>%
    arrange(desc(n)) %>%
    right_join(X111021_mine_query_sheet_hp) %>%
    select(WBID,name,n,mine_query) %>%
    kableExtra::kable() %>%
    kableExtra::kable_material_dark()
}

# parsedtxt = spacy_parse(
#   "Sewage fungus coating the bed of the River Windrush. And the @EnvAgency does what exactly?",
#   pos = TRUE,
#   lemma = TRUE,
#   dependency = TRUE,
#   nounphrase = TRUE,
#   multithread = TRUE
# )
# library(rsyntax)
# tokens = as_tokenindex(parsedtxt)
# plot_tree(tokens, token, lemma)
