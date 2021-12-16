library(sf)
library(tidyverse)

raw_data = readRDS("data/river_queries/raw_data.RDS")

set.seed(2839)
test_data = sample_n(raw_data, 1000) %>%
  select(text, clean_tweet, WBID) %>%
  mutate(
    senti_nrc = sentiment(
      # NRC sentiment from Mohammad & Turney (2010)
      test_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_nrc,
      hyphen = " "
    ),
    senti_nrc = senti_nrc$sentiment
  ) %>%
  mutate(
    senti_sentiword = sentiment(
      #  Baccianella, Esuli and Sebastiani's (2010)
      test_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_sentiword,
      hyphen = " "
    ),
    senti_sentiword = senti_sentiword$sentiment
  ) %>%
  mutate(
    senti_jockers_rinkers = sentiment(
      # Jockers (2017) & Rinker's augmented Hu & Liu (2004)
      test_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_jockers_rinker,
      hyphen = " "
    ),
    senti_jockers_rinkers = senti_jockers_rinkers$sentiment
  ) %>%
  mutate(
    senti_cnet = sentiment(
      # Cambria, Poria, Bajpai,& Schuller's (2016)
      test_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_senticnet,
      hyphen = " "
    ),
    senti_cnet = senti_cnet$sentiment
  ) %>%
  mutate(
    senti_google = sentiment(
      # Taboada, Brooke, Tofiloski, Voll, & Stede's (2011)
      test_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_socal_google,
      hyphen = " "
    ),
    senti_google = senti_google$sentiment
  )

for (i in 1:nrow(test_data)) {
  test_datalog_senti[i] = log(
    mean(
      test_data$senti_nrc[i] + test_data$senti_sentiword[i] + test_data$senti_jockers_rinkers[i] + test_data$senti_cnet[i] + test_data$senti_google[i]
    )
  )

}

WB = as.data.frame(aggregate(senti_nrc ~ WBID, test_data, mean))




waterbody = read_sf("data/web/wb_cat_class.geojson") %>%
  st_drop_geometry() %>%
  select(WBID,
         status,
         senticent_polarity) %>%
  na.omit(status) %>%
  mutate(status = factor(status, levels = c('Bad', 'Poor', 'Moderate', 'Good', 'NA')))


ggplot2::ggplot(waterbody, aes(x = status, y = senticent_polarity, group = status)) +
  ggplot2::geom_boxplot(aes(fill = status)) +
  theme_tinyhand()

summary(lm(senticent_polarity ~ status, data = waterbody))
