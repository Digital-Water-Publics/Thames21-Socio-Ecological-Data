spacy_initialize()

# parse data
if(file.exists("data/parsed/raw_parsed_text.csv")){
  pp = read.csv("data/parsed/raw_parsed_text.csv")
} else {
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
  parsedtxt_1 = spacy_parse(
    data_main_1$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  rm(data_main_1)

  # parse text into a single text df
  parsedtxt_2 = spacy_parse(
    data_main_2$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  rm(data_main_2)

  # parse text into a single text df
  parsedtxt_3 = spacy_parse(
    data_main_3$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  rm(data_main_3)

  # parse text into a single text df
  parsedtxt_4 = spacy_parse(
    data_main_4$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  rm(data_main_4)

  # parse text into a single text df
  parsedtxt_5 = spacy_parse(
    data_main_5$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  rm(data_main_5)

  # parse text into a single text df
  parsedtxt_6 = spacy_parse(
    data_main_6$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  rm(data_main_6)

  # parse text into a single text df
  parsedtxt_7 = spacy_parse(
    data_main_7$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
    multithread = TRUE
  )
  rm(data_main_7)

  # parse text into a single text df
  parsedtxt_8 = spacy_parse(
    data_main_8$clean_tweet,
    pos = TRUE,
    tag = TRUE,
    lemma = TRUE,
    entity = TRUE,
    dependency = TRUE,
    nounphrase = TRUE,
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

  write.csv(pp,"data/parsed/raw_parsed_text.csv")

  parsed_noun = pp %>%
    filter(pos == "NOUN") %>%
    select(lemma) %>%
    mutate(lemma = str_remove_all(lemma, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("#[[:alnum:]_]+"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("[[:punct:]]"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("^RT:? "))) %>%
    mutate(lemma = str_replace(lemma, "amp", "and")) %>%
    anti_join(stop_words, by = c("lemma" = "word")) %>%
    mutate(lemma = str_to_lower(lemma)) %>%
    count(lemma) %>%
    arrange(desc(n))
  head(parsed_sub,20)

  write.csv(parsed_noun,"data/parsed/parsed_noun_freq.csv")

  parsed_adj = pp %>%
    filter(pos == "ADJ") %>%
    select(lemma) %>%
    mutate(lemma = str_remove_all(lemma, regex(" ?(f&ht)(tp)(s?)(://)(.*)[.&/](.*)"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("@[[:alnum:]_]{4,}"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("#[[:alnum:]_]+"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("[[:punct:]]"))) %>%
    mutate(lemma = str_remove_all(lemma, regex("^RT:? "))) %>%
    mutate(lemma = str_replace(lemma, "amp", "and")) %>%
    anti_join(stop_words, by = c("lemma" = "word")) %>%
    mutate(lemma = str_to_lower(lemma)) %>%
    count(lemma) %>%
    arrange(desc(n))
  head(parsed_adj,50)

  write.csv(parsed_adj,"data/parsed/parsed_adj_freq.csv")

}

adj_ferq = read.csv("data/parsed/parsed_adj_freq.csv") %>% select(-c(X))
noun_freq = read.csv("data/parsed/parsed_noun_freq.csv") %>% select(-c(X))
noun_freq = noun_freq[-1,]
library(wordcloud2)
wordcloud2(adj_ferq)
wordcloud2(noun_freq)
