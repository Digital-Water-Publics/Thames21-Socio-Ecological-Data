##################################################################
##                      read and tidy data                      ##
##################################################################

if (file.exists("data/river_queries/raw_data_new.RDS")) {
  raw_data = readRDS("data/river_queries/raw_data_new.RDS")
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



