

#################################################################
##                         Sample Tweet                       ##
#################################################################

data = sample_n(raw_data, 1)
print(data$text)
#anger, fear, anticipation, trust, surprise, sadness, joy, and disgust
#Create entity string dataframe
entity_sentiment_string <- function(data) {
  out <- tryCatch({
    #################################################################
    ##                         Parse Tweet                         ##
    #################################################################
    parsedtxt = spacy_parse(
      data$clean_tweet,
      pos = TRUE,
      lemma = TRUE,
      dependency = TRUE,
      nounphrase = TRUE,
      multithread = TRUE
    ) %>%
      rename(word = token) %>%
      left_join(get_sentiments("bing")) %>%
      filter(entity != "TIME_B", entity != "TIME_I")

    #################################################################
    ##                         Join w NRC                         ##
    #################################################################
    sample_data = parsedtxt %>%
      left_join(get_sentiments("nrc"))
    #################################################################
    ##                Filter by sentiment trigger                 ##
    #################################################################
    senti_triggers = filter(parsedtxt, sentiment != is.na(sentiment)) %>% select(word)
    print(paste0("Unique sentiment triggers: ",unique(senti_triggers)))
    # Condition to catch if tweet has no emo-lex association
    suppressWarnings(if (all(is.na(parsedtxt$sentiment))) {
      message("tweet doesn't contain emo-lex")
      message("calculating sentiment polarity instead")
    } else {
      message("tweet contains sentiment emo-lex")
      # Get sentiment with largest count
      emotion =  names(table(parsedtxt$sentiment))[as.vector(table(parsedtxt$sentiment)) ==
                                                     max(table(parsedtxt$sentiment))]
      #Pasting settings
      emotion = paste(emotion, collapse = " OR ")
      emotion_direction = " @"
      emotion_phrase = paste(emotion, emotion_direction, sep = "")
      message("calculating sentiment at entity level")
      #Extract unique noun phrases and merge into DF
      nounphrase_entity = spacy_extract_nounphrases(sample_data$word, output = "data.frame")
      nounphrase_entity = names(table(nounphrase_entity$text))[as.vector(table(nounphrase_entity$text)) ==
                                                                 unique(table(nounphrase_entity$text))]
      nounphrase_entity_df = as.data.frame(nounphrase_entity)

      #Loop of DF to paste senitmnet direction
      for (i in 1:nrow(nounphrase_entity_df)) {
        nounphrase_entity_df$nounphrase_entity[i] = paste(emotion_phrase,
                                                          nounphrase_entity_df$nounphrase_entity[i])
      }
      return(nounphrase_entity_df)

    })

  },
  error = function(cond) {
    message(paste("Data has no nounphrase:", data))
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  warning = function(cond) {
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  },
  finally = {
    print("finishing this operation")
  })
}
entity_sentiment_string(data = data)
