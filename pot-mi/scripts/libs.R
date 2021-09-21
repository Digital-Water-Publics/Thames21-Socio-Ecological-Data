pkgs = c("academictwitteR",
         "dplyr",
         "sf",
         "cde")

# Install pkgs not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))

log_calculator = function(river_query,wbid){
  tryCatch(
    expr = {
      river_query = as.character(river_query)
      tweets = get_all_tweets(
        query = river_query,
        is_retweet = FALSE,
        lang = "en",
        start_tweets = "2006-04-01T00:00:00Z", #TODO discuss with Helge - should this go back to 2016?
        end_tweets = "2021-09-01T00:00:00Z",
        bearer_token = get_bearer(),
        data_path = "data/mined_tweets",
        bind_tweets = TRUE,
        context_annotations = FALSE,
        page_n = 500,
        n = Inf
      )

      # Clean and write csv --------------------------------------------------------------
      river_tweets = bind_tweets(data_path = "data/mined_tweets", output_format = "tidy") %>%
        mutate(query = river_query) %>%
        mutate(WBID = wbid)

      #delete files
      do.call(file.remove, list(list.files("data/mined_tweets", full.names = TRUE)))

      file_path = paste0("data/river_queries/",river_query,".rds")

      #save river file
      saveRDS(river_tweets, file_path)
    },
    error = function(e){
      message('Caught an error!')
      print(e)
    },
    finally = {
      message('All done, quitting.')
    }
  )
}


library(dplyr)
library(tidyr)
library(stringr)


aa = read.csv("../../../../Downloads/200921_rivers_list.csv")
aa = as.data.frame(str_split_fixed(aa$Name.Query., ";", 2)) # split into two columns
aa$V1 = gsub('[[:digit:]]+', '', aa$V1) # remove digits
aa$V1 = aa$V1 %>% str_remove_all("[[:punct:]]") %>% str_trim("both") # remove punct and trim

aa$V2 = sub(";x","",aa$V2)
aa$V2 = aa$V2 %>% str_remove_all("[[:punct:]]") %>% str_trim("both")

aa = aa %>%
  rename(label = V1) %>%
  rename(mine_query = V2)

al = right_join(thames,aa)

tt = as.data.frame(unique(subset(aa$mine_query, str_count(aa$mine_query,"\\S+") == 1)))

az = aa %>%
