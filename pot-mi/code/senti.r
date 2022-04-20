####
####
#### AIM: Calculate sentiment for tweets
#### 1. NRC Emo Lex () - Mohammad & Turney', P. D.'s (2010)
#### 2. Senticinet (negative positive) - Cambria, Poria, Bajpai,& Schuller's (2016)
####
####

# get NRC data
nrc_data = lexicon::nrc_emotions %>%
  gather("sentiment", "flag", anger:trust, -term) %>%
  filter(flag == 1)

if(file.exists("data/river_queries/clean_senti.RDS")){
  clean_senti = readRDS("data/river_queries/clean_senti.RDS")
} else {
  # calculate words that match the NRC emo lex, group by tweet_id and pivot into wide format
  emotions = data %>%
    unnest_tokens(word, clean_tweet) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!grepl('[0-9]', word)) %>%
    inner_join(nrc_data, by = c("word" = "term"))  %>%
    group_by(tweet_id, sentiment) %>%
    summarize(freq = n()) %>%
    ungroup() %>%
    pivot_wider(values_from = freq,
                names_from = sentiment) %>% mutate_at(
                  vars(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
                  ~ replace_na(., 0)
                )

  # merge data
  clean_senti_run = left_join(new_run, emotions)
  clean_senti_run[,17:24][is.na(clean_senti_run[,17:24])] = 0
  
  # sesntiment plot from syhzet package
  clean_senti_run$senti_score = get_sentiment(clean_senti_run$clean_tweet, method = "syuzhet")
  clean_senti_run$senti_log = log(clean_senti_run$senti_score)
  # save rds
  saveRDS(clean_senti, "data/river_queries/clean_senti.RDS")
}






