##################################################################
##                            Set up                            ##
##################################################################
setwd("~/pot-mi/pot-mi") # set wd
uk = read_sf("Data/Countries_(December_2020)_UK_BGC.geojson") %>%
  st_as_sf(crs = 4326) # read uk countries shapefile


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
##################################################################
##                      read and tidy data                      ##
##################################################################

`%ni%` = function (x, table)
  is.na(match(x, table, nomatch = NA_integer_))

if (file.exists("data/river_queries/raw_data_clean132.RDS")) {
  raw_data = readRDS("data/river_queries/raw_data.RDS")
} else {
  # # read and clean data
  setwd("~/pot-mi/pot-mi/data/river_queries/")
  # Read min data -----------------------------------------------------------
  # create df of raw csvs
  loop_csv = as.data.frame(grep(
    list.files(full.names = TRUE),
    pattern = '*GB',
    invert = TRUE,
    value = TRUE
  ))
  colnames(loop_csv) = "filename"
  loop_csv = subset(loop_csv, !grepl("rds", filename))
  loop_csv = subset(loop_csv, !grepl("RDS", filename))

  # Loop and clean data -----------------------------------------------------
  n = 1
  for (i in 1:nrow(loop_csv)) {
    files = nrow(loop_csv)
    path = loop_csv$filename[i]
    csv = read.csv(path)
    if ("user_location" %ni% colnames(csv)) {
      csv$user_location = ""
    }
    if ("tweet_id" %ni% colnames(csv)) {
      csv$tweet_id = "none"
    }
    if ("source" %ni% colnames(csv)) {
      csv$source = "none"
    }
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
          "source",
          "user_following_count",
          "user_followers_count",
          "possibly_sensitive",
          "author_id",
          "user_location",
          "query",
          "WBID"
        )
      ) %>%
      filter(lang == "en") %>%
      distinct(tweet_id, .keep_all = TRUE) %>%
      filter(str_detect(text, "^RT:? ") == FALSE)


    wbid = csv$WBID[1] # set wbid

    write.csv(csv, paste0(wbid, ".csv"))
    message(paste0(n, "/", files, " files cleaned. Cleaning the next file"))
    n = n + 1
  }

  # Bind data ---------------------------------------------------------------
  min_files = list.files(pattern = "*GB")
  raw_min_data = lapply(min_files, function(i) {
    read.csv(i)
  })
  raw_data = do.call(rbind.data.frame, raw_min_data)
  clean_tweet = raw_data %>% clean_tweets_sentiment()
  saveRDS(clean_tweet, "raw_data_new.RDS")
}

##################################################################
##                          Report Functions                   ##
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
