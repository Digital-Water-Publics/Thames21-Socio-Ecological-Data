# # read and clean data
setwd("data/river_queries/")
filenames = list.files(full.names = TRUE)


# Read raw data -----------------------------------------------------------
# All = lapply(filenames, function(i) {
#   read.csv(i)
# })
# set wd()


min_files = list.files(pattern = "*GB")
raw_min_data = lapply(min_files, function(i) {
  read.csv(i)
})

# bind data
raw_data = do.call(rbind.data.frame, raw_min_data) %>%
  distinct(tweet_id, .keep_all = TRUE)

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
#clean_data = clean_tweets_sentiment(raw_data)
clean_data = readRDS("data/river_queries/clean_data.rds")

