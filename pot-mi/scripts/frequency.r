library(spacyr)

# parse text into a single text df
parsedtxt = spacy_parse(
  data_main$clean_tweet,
  pos = TRUE,
  tag = TRUE,
  lemma = TRUE,
  entity = TRUE,
  dependency = TRUE,
  nounphrase = TRUE,
  multithread = TRUE
)

