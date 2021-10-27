data_main = readRDS("data/river_queries/clean_senti_polatiry.rds")

data_main_1 = data_main[1:488728, ]
data_main_2 = data_main[488729:977457, ]
data_main_3 = data_main[977458:1466186, ]
data_main_4 = data_main[1466187:1954915, ]
data_main_5 = data_main[1954916:2443644, ]
data_main_6 = data_main[2443645:2932373, ]
data_main_7 = data_main[2932374:3421102, ]
data_main_8 = data_main[3421103:3909824, ]

rm(data_main)

# parse text into a single text df
parsedtxt_1 = spacy_extract_nounphrases(
  data_main_1$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE
)
rm(data_main_1)

# parse text into a single text df
parsedtxt_2 = spacy_extract_nounphrases(
  data_main_2$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE
)
rm(data_main_2)

# parse text into a single text df
parsedtxt_3 = spacy_extract_nounphrases(
  data_main_3$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE)
rm(data_main_3)

# parse text into a single text df
parsedtxt_4 = spacy_extract_nounphrases(
  data_main_4$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE)
rm(data_main_4)

# parse text into a single text df
parsedtxt_5 = spacy_extract_nounphrases(
  data_main_5$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE
)
rm(data_main_5)

# parse text into a single text df
parsedtxt_6 = spacy_extract_nounphrases(
  data_main_6$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE
)
rm(data_main_6)

# parse text into a single text df
parsedtxt_7 = spacy_extract_nounphrases(
  data_main_7$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE
)
rm(data_main_7)

# parse text into a single text df
parsedtxt_8 = spacy_extract_nounphrases(
  data_main_8$clean_tweet,
  output = c("data.frame"),
  multithread = TRUE
)
rm(data_main_8)

pp = rbind(
  parsedtxt_1,
  parsedtxt_2,
  parsedtxt_3,
  parsedtxt_4,
  parsedtxt_5,
  parsedtxt_6,
  parsedtxt_7,
  parsedtxt_8
)

saveRDS(pp,"data/web/pp.RDS")
nounphrase = readRDS("data/web/pp.RDS")

nounphrase_sub = nounphrase %>%
  filter(length > 2) %>%
  select(text) %>%
  mutate(text = str_remove_all(text, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
  mutate(text = str_remove_all(text, regex("@[[:alnum:]_]{4,}"))) %>%
  mutate(text = str_remove_all(text, regex("#[[:alnum:]_]+"))) %>%
  mutate(text = str_remove_all(text, regex("[[:punct:]]"))) %>%
  mutate(text = str_remove_all(text, regex("^RT:? "))) %>%
  mutate(text = str_replace(text, "amp", "and")) %>%
  anti_join(stop_words, by = c("text" = "word")) %>%
  mutate(text = str_to_lower(text)) %>%
  # Remove any trailing whitespace around the text
  mutate(text = str_trim(text, "both")) %>%
  count(text) %>%
  arrange(desc(n))
nounphrase_sub = nounphrase_sub[-15,]
wordcloud2(nounphrase_sub,shape = "square")
