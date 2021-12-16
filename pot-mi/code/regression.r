library(sf)
library(tidyverse)

raw_data = readRDS("data/river_queries/raw_data.RDS")

set.seed(2832)
regression_data = sample_n(raw_data, 10000)

regression_data = regression_data %>%
  select(text, clean_tweet, WBID) %>%
  mutate(
    senti_nrc = sentiment(
      # NRC sentiment from Mohammad & Turney (2010)
      regression_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_nrc,
      hyphen = " "
    ),
    senti_nrc = senti_nrc$sentiment
  ) %>%
  mutate(
    senti_sentiword = sentiment(
      #  Baccianella, Esuli and Sebastiani's (2010)
      regression_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_sentiword,
      hyphen = " "
    ),
    senti_sentiword = senti_sentiword$sentiment
  ) %>%
  mutate(
    senti_jockers_rinkers = sentiment(
      # Jockers (2017) & Rinker's augmented Hu & Liu (2004)
      regression_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_jockers_rinker,
      hyphen = " "
    ),
    senti_jockers_rinkers = senti_jockers_rinkers$sentiment
  ) %>%
  mutate(
    senti_cnet = sentiment(
      # Cambria, Poria, Bajpai,& Schuller's (2016)
      regression_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_senticnet,
      hyphen = " "
    ),
    senti_cnet = senti_cnet$sentiment
  ) %>%
  mutate(
    senti_google = sentiment(
      # Taboada, Brooke, Tofiloski, Voll, & Stede's (2011)
      regression_data$clean_tweet,
      polarity_dt = lexicon::hash_sentiment_socal_google,
      hyphen = " "
    ),
    senti_google = senti_google$sentiment
  )

scaleit = function(x) {
  (x - min(x)) / (max(x) - min(x))
}


for (i in 1:nrow(regression_data)) {
  regression_data$log_senti[i] = mean(regression_data$senti_nrc[i] + regression_data$senti_sentiword[i] + regression_data$senti_jockers_rinkers[i] + regression_data$senti_cnet[i] + regression_data$senti_google[i])
}

WB = as.data.frame(aggregate(scale_senti ~ WBID, regression_data, mean))

waterbody = read_sf("data/web/wb_cat_class.geojson") %>%
  st_drop_geometry() %>%
  select(WBID,
         status,
         senticent_polarity) %>%
  na.omit(status) %>%
  mutate(status = factor(status, levels = c('Bad', 'Poor', 'Moderate', 'Good', 'NA')))

reg_test = inner_join(WB,waterbody)


ggplot2::ggplot(reg_test, aes(x = status, y = scale_senti, group = status)) +
  ggplot2::geom_boxplot(aes(fill = status)) +
  theme_tinyhand()

summary(lm(senticent_polarity ~ status, data = waterbody))
