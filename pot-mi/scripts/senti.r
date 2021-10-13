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
  emotions = clean_data %>%
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
  clean_senti = left_join(clean_data, emotions)

  # save rds
  saveRDS(clean_senti, "data/river_queries/clean_senti.RDS")
}

clean_senti_1 = clean_senti[1:488728,]
clean_senti_2 = clean_senti[488729:977457,]
clean_senti_3 = clean_senti[977458:1466186,]
clean_senti_4 = clean_senti[1466187:1954915,]
clean_senti_5 = clean_senti[1954916:2443644,]
clean_senti_6 = clean_senti[2443645:2932373,]
clean_senti_7 = clean_senti[2932374:3421102,]
clean_senti_8 = clean_senti[3421103:3909824,]


# calculate polarity score
clean_senti_8$senti_nrc = sentiment(
  clean_senti_8$clean_tweet,
  polarity_dt = lexicon::hash_sentiment_senticnet,
  hyphen = " ",
  amplifier.weight = 0.8,
  n.before = Inf,
  n.after = Inf,
  question.weight = 1,
  adversative.weight = 0.5,
  neutral.nonverb.like = TRUE,
  missing_value = 0
)

clean_senti_8 = clean_senti_8 %>% select(-c(senti_nrc))

clean_senti_polarity = rbind(clean_senti_1,clean_senti_2,
                             clean_senti_3,clean_senti_4,
                             clean_senti_5,clean_senti_6,
                             clean_senti_7,clean_senti_8)

saveRDS(clean_senti_polarity,"data/river_queries/clean_senti_polatiry.rds")

