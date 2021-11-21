sample = sample_n(raw_data, 10)

sample = sample %>%
  mutate(
    senti_nrc = sentiment(
      # NRC sentiment from Mohammad & Turney (2010)
      sample$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_nrc,
      hyphen = " "
    ),
    senti_nrc = senti_nrc$sentiment
  ) %>%
  mutate(
    senti_emojis = sentiment(
      # Novak, Smailovic, Sluban, & Mozetic's (2015)
      sample$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_emojis,
      hyphen = " "
    ) ,
    senti_emojis = senti_emojis$sentiment
  ) %>%
  mutate(
    senti_sentiword = sentiment(
      #  Baccianella, Esuli and Sebastiani's (2010)
      sample$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_sentiword,
      hyphen = " "
    ),
    senti_sentiword = senti_sentiword$sentiment
  ) %>%
  mutate(
    senti_jockers_rinkers = sentiment(
      # Jockers (2017) & Rinker's augmented Hu & Liu (2004)
      sample$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_jockers_rinker,
      hyphen = " "
    ),
    senti_jockers_rinkers = senti_jockers_rinkers$sentiment
  ) %>%
  mutate(
    senti_cnet = sentiment(
      # Cambria, Poria, Bajpai,& Schuller's (2016)
      sample$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_senticnet,
      hyphen = " "
    ),
    senti_cnet = senti_cnet$sentiment
  ) %>%
  mutate(
    senti_google = sentiment(
      # Taboada, Brooke, Tofiloski, Voll, & Stede's (2011)
      sample$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_socal_google,
      hyphen = " "
    ),
    senti_google = senti_google$sentiment
  )

sample = as.data.frame(unlist(sample))
