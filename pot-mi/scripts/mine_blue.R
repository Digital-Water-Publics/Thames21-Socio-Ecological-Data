# AIM: Individual dataframes for mentions on twitter for each Thames catchment water body

# Twitter Mine ------------------------------------------------------------
mine_blue_mentions = function(river_query,wbid) {
  river_query = as.character(river_query)

  try(
    tweets = get_all_tweets(
      query = river_query,
      is_retweet = FALSE,
      lang = "en",
      start_tweets = "2006-04-01T00:00:00Z", #TODO discuss with Helge - should this go back to 2016?
      end_tweets = "2021-09-01T00:00:00Z",
      bearer_token = get_bearer(),
      data_path = "data/mined_tweets",
      bind_tweets = TRUE,
      context_annotations = FALSE,
      page_n = 500,
      n = Inf
    )

  )


  # Clean and write csv --------------------------------------------------------------
  river_tweets = bind_tweets(data_path = "data/mined_tweets", output_format = "tidy") %>%
    mutate(query = river_query) %>%
    mutate(WBID = wbid)

  #delete files
  do.call(file.remove, list(list.files("data/mined_tweets", full.names = TRUE)))

  file_path = paste0("data/river_queries/",river_query,".rds")

  #save river file
  saveRDS(river_tweets, file_path)
}

waterbodies = read.csv("data/waterbodies.csv")

for(i in 1:nrow(waterbodies)){
  log_calculator(river_query = waterbodies$mine_query[i],wbid = waterbodies$WBID[i])
}





