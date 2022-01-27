####
####
#### AIM: Prepare twitter data for web map
####

if (file.edit("data/web/oc.geojson")) {

} else {
  # From continous to categorical -------------------------------------------

  clean_senti = readRDS("data/river_queries/clean_senti_polatiry.rds")
  summary(clean_senti$senti_score)

  #Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
  #-3.33573 -0.05964  0.06171  0.11009  0.30321  7.85655

  Percentile_00 = min(clean_senti$senti_score)
  Percentile_33 = quantile(clean_senti$senti_score, 0.333333)
  Percentile_67 = quantile(clean_senti$senti_score, 0.666667)
  Percentile_100 = max(clean_senti$senti_score)
  RB = rbind(Percentile_00, Percentile_33, Percentile_67, Percentile_100)

  # read water bodies
  thames_wb = read_sf("data/thames_river.geojson") %>%
    st_drop_geometry() %>%
    select(WBID, name, OC, OC_num, MC, MC_num, RBD, RBD_num)

  # join with main data to get categorical variables
  clean_senti = inner_join(clean_senti, thames_wb)

  # aggregate data
  MC = as.data.frame(aggregate(senti_score  ~ MC_num, clean_senti, mean))
  OC = as.data.frame(aggregate(senti_score  ~ OC_num, clean_senti, mean))
  RBD = as.data.frame(aggregate(senti_score  ~ RBD_num, clean_senti, mean))
  WB = as.data.frame(aggregate(senti_score  ~ WBID, clean_senti, mean))

  make_that_data_categorial = function(data) {
    data$group[data$senti_score >= Percentile_00 &
                 data$senti_score <  Percentile_33]  = 1
    data$group[data$senti_score >= Percentile_33 &
                 data$senti_score <  Percentile_67]  = 2
    data$group[data$senti_score >= Percentile_67 &
                 data$senti_score <= Percentile_100] = 3
    return(data)
  }

  MC = make_that_data_categorial(data = MC)
  OC = make_that_data_categorial(data = OC)
  RBD = make_that_data_categorial(data = RBD)
  WB = make_that_data_categorial(WB)
  # Generate geojson files for various catchment geogrpahies ----------------

  # generate mc geojson
  mc_sf = read_sf("data/mc_test.geojson") %>%
    select(-c(val)) %>%
    rename(MC_num = notation) %>%
    right_join(MC) %>%
    rename(MC = label) %>%
    select(MC, MC_num, senti_score, geometry)
  mc_sf$MC = gsub("_", " ", mc_sf$MC)
  write_sf(mc_sf, "data/web/mc_cat.geojson")

  # generate oc geojson
  oc_sf = read_sf("data.oc_test.geojson") %>%
    select(-c(val)) %>%
    rename(OC_num = notation) %>%
    right_join(OC) %>%
    mutate(group = as.numeric(group)) %>%
    rename(OC = label) %>%
    select(OC, OC_num, senti_score, group,geometry)

  write_sf(oc_sf, "data/web/oc_cat.geojson")

  # generate wbid geojson
  wb_sf = read_sf("data/web/wb_class.geojson") %>%
    select(-c(senticent_polarity))%>%
    mutate(senti_score = 0) %>% right_join(WB)
  write_sf(wb_sf, "data/web/wb_cat_class.geojson")



  # Search centroid data -----------------------------------------------------------
  mc_sf = read_sf("data/web/mc.geojson")
  mc_cent = st_centroid(mc_sf) %>% rename(name = MC) %>% select(name, geometry)

  oc_sf = read_sf("data/web/oc.geojson")
  oc_cent = st_centroid(oc_sf) %>% rename(name = OC) %>% select(name, geometry)

  wb_sf = read_sf("data/web/wb.geojson")
  wb_cent = st_centroid(wb_sf) %>% select(name, geometry)

  wb_sf_class = inner_join(wb_sf, wb_class)
  write_sf(wb_sf_class, "data/web/wb_class.geojson")

  all_cent = rbind(wb_cent, oc_cent, mc_cent)
  write_sf(all_cent, "data/web/search_centroids.geojson")

}

sentiment_key_word_subsets = function(df) {
  corpus = corpus(df$clean_tweet)
  token_word = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
  senti_word = inner_join(token_word, get_sentiments("nrc")) %>%
    count(sentiment)
  senti_word$percent = (senti_word$n / sum(senti_word$n)) * 100
  ggplot(senti_word, aes(sentiment, percent)) +
    geom_bar(aes(fill = sentiment), position = 'stack', stat = 'identity') +
    coord_flip() +
    theme(
      plot.background = element_rect(fill = "black", colour = NA),
      panel.background = element_rect(fill = "black", colour = NA),
      legend.position = "none",
      axis.text = element_text(color = "white"),
      text = element_text(
        size = 16,
        family = "times",
        colour = "white"
      ),
    )
}

sentiment_key_word_subsets(df = river)

wbids = as.data.frame(unique(clean_senti$WBID))
colnames(wbids) = "WBID"

for (i in 1:nrow(wbids)) {

  river = clean_senti %>% filter(WBID == wbids$WBID[44])

  p=river %>% mutate(created_at = ymd_hms(created_at)) %>%
    group_by(created_at) %>%
    summarise(mean_senti = mean(senti_score),) %>%
    ggplot( aes(x=created_at, y=mean_senti)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("Sentiment score") +
    xlab("Date") +
    ggtitle(paste0(river$name, "\nOperational catchment: ", river$OC)) +
    geom_smooth() +
dark_theme()

  corpus = corpus(river$clean_tweet)
  token_word = data.frame(text = corpus, stringsAsFactors = FALSE) %>% unnest_tokens(word, text)
  senti_word = inner_join(token_word, get_sentiments("nrc")) %>%
    count(sentiment)
  senti_word$percent = (senti_word$n / sum(senti_word$n)) * 100
  senti_word$percent = round(senti_word$percent,2)

  t = ggplot(senti_word, aes(x = reorder(sentiment,percent), y = percent, fill = sentiment, label = percent)) +
    geom_bar(stat="identity", show.legend = FALSE) +
    coord_flip() +
    labs(title = "Common Emotions", x = "Emotion", y = "Percent") +
dark_theme() +
    geom_label(aes(fill = sentiment),colour = "white", fontface = "bold", show.legend = FALSE) + scale_fill_manual(values = wes_palette("GrandBudapest2", 10, type = "continuous"))



  parsed = spacy_extract_nounphrases(
    river$clean_tweet,
    output = c("data.frame"),
    multithread = TRUE
  )

  parsed =  parsed %>%
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

 y = ggplot(head(parsed,10), aes(x = reorder(text, n), y = n, fill = text, label = n)) +
    geom_bar(stat="identity", show.legend = FALSE) +
    coord_flip() +
    labs(title = "Common topics", x = "Phrase", y = "Count") +
   dark_theme() + geom_label(aes(fill = text),colour = "white", fontface = "bold", show.legend = FALSE) +
   scale_fill_manual(values = wes_palette("Darjeeling2", 10, type = "continuous"))

 ggarrange(p,                                                 # First row with scatter plot
           ggarrange(b, y, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
           nrow = 2,
           labels = "A"                                        # Labels of the scatter plot
 )

  ggplotly(z)

  head(river)

}

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)



p_grid = seq(from=0,to=1,len=1000)
prob_p = rep(1,1000)
prop_data = dbinom(6,9, prob=p_grid)
posterior = prop_data * prob_p
posterior = posterior / sum(posterior)
plot(posterior)

p_grid = seq(from=0,to=1,len=1000)
prob_p = dbeta(p_grid, 3,1)
prop_data = dbinom(6,9, prob=p_grid)
posterior = prop_data * prob_p
posterior = posterior / sum(posterior)
plot(posterior)


samples = sample(p_grid, prob = posterior, size = 1e4,replace = TRUE)
plot(samples , type = "l")
w = rbinom(1e4,size=9,prob=samples)
plot(w)
