library(tidyverse) # importing, cleaning, visualising
library(tidytext) # working with text
library(wordcloud) # visualising text
library(gridExtra) # extra plot options
library(grid) # extra plot options
library(keras) # deep learning with keras

data = readRDS("data/river_queries/raw_data_new.RDS")
train_data = sample_n(data,1000)
train_data$document = seq.int(nrow(train_data))
rm(data)

# Split tweets into topic clusters
# Enviromental reporting - alert/warning/flood/ metadata device
# Biophilla - beautiful/love/view/ metadata android or iphone
#

# anitcipation = anxiety
# trust
#

burnin = 1000
#set iterations
iter = 1000
#thin the spaces between samples
thin = 100
#set random starts at 5
nstart = 5
#use random integers as seed
seed = list(123, 456, 789, 987, 654)
# return the highest probability as the result
best = TRUE

topic_model_tweet_corpus = function(tweet_vector, n) {
  # Converting tweets into corpus ----------------------------------------------
  print("Converting tweets into corpus")
  text_corpus = SimpleCorpus(VectorSource(tweet_vector))
  text_corpus = tm_map(text_corpus, removeWords, stopwords("en"))
  # Remove numbers. This could have been done earlier, of course.
  text_corpus = tm_map(text_corpus, removeNumbers)

  # Stem the words. Google if you don't understand
  text_corpus = tm_map(text_corpus, stemDocument)

  # Remove the stems associated with our search terms!
  text_corpus = tm_map(text_corpus, removeWords, c("env", "agency"))
  # Converting corpus into DTM ----------------------------------------------
  text_dtm = DocumentTermMatrix(
    text_corpus,
    control = list(
      tolower = TRUE,
      removePunctuation = TRUE,
      removeNumbers = TRUE,
      stopwords = TRUE,
      sparse = TRUE
    )
  )
  print("Converting corpus into DTM")
  # Generating sparse matrix to save memory ----------------------------------------------
  text_dtm2 = Matrix::sparseMatrix(
    i = text_dtm$i,
    j = text_dtm$j,
    x = text_dtm$v,
    dims = c(text_dtm$nrow, text_dtm$ncol),
    dimnames = text_dtm$dimnames
  )
  # Calculating row sums ----------------------------------------------
  doc_lengths = Matrix::rowSums(text_dtm2)
  # Removing lengths of 0 ----------------------------------------------
  text_dtm3 = text_dtm2[doc_lengths > 0, ]
  print("Calculated spare matrix for memory isses")
  # Building LDA model ----------------------------------------------
  print("building LDA model")
  text_lda = LDA(text_dtm3, n, method = "Gibbs", control=list(iter = 500, verbose = 25))

  result = tidytext::tidy(text_lda, 'beta')
  result %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", ncol = 4) +
    coord_flip()

  text_posterior_result = posterior(text_lda)
  attributes(text_posterior_result)

  beta = text_posterior_result$terms
  dim(beta)
  theta = text_posterior_result$topics
  dim(theta)


  print("Extracting model features")
  top10terms_10 = as.matrix(terms(text_lda, 10))
  top10terms_10
  #write.csv(top10terms_10,paste("top_10_topic_terms_best_", n, ".csv", sep = ""))
  #Create topic per tweet
  topic_per_tweet = tidy(text_lda, matrix = "gamma") %>%
    pivot_wider(names_from = topic, values_from = "gamma")


  suppressWarnings(for (i in 1:nrow(topic_per_tweet)) {
    topic_per_tweet$key_topic[i] = max.col(topic_per_tweet[i, 1:n])
  })

  #Rename column
  topic_per_tweet = topic_per_tweet %>%
    rename_all(paste0, " topic(k=", n, ")")
  colnames(topic_per_tweet)[1] = "document"
  topic_per_tweet$document = as.numeric(topic_per_tweet$document)

  tweets_primary_df = left_join(topic_per_tweet, train_data)
  print("merge complete")
  return(tweets_primary_df)
}

topic = topic_model_tweet_corpus(tweet_vector = train_data$clean_tweet, n =12)


topic_model_tweet_corpus(tweet_vector = tweets_primary_df$word, n = 6)
saveRDS(tweets_primary_df,"data/tweets_primary_df_topics.rds")
tweets_primary_df = readRDS("data/tweets_primary_df_topics.rds")

library(tidyverse)
url <- "https://raw.githubusercontent.com/cjhutto/vaderSentiment/master/vaderSentiment/vader_lexicon.txt"
# note: the command below gives warning messages due to the details column, these can be safely ignored
vader <- read_delim(url, col_names=c("word","sentiment", "details"),  col_types="cdc",  delim="\t")
head(vader)

vader = dictionary(vader)
corp <- corpus(train_data, docid_field = 'document', text_field = 'clean_tweet')
dtm <- corp %>%
  tokens() %>%
  tokens_tolower() %>%
  dfm()
result2 = dtm  %>% dfm_lookup(vader) %>% as_tibble()

# topics6 = tweets_primary_df %>% select(2:7)
# topics5 = tweets_primary_df %>% select(9:13)
#
# plot_topic_prob = function(topic) {
#
#   topics5 %>%
#     pivot_longer(cols = 1:5) %>%
#     ggplot(aes(value)) +
#     geom_histogram() +
#     facet_wrap(~name, scales = "free")
#
#   ggsave(paste("data/",topic, "_hist_plot.png", sep = ""))
# }
#
# plot_topic_prob(topic = topics5)
# plot_topic_prob(topic = topics6)
# plot_topic_prob(topic = topics7)
# plot_topic_prob(topic = topics8)
