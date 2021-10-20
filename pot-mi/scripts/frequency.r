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

}


