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

library(dplyr)
library(tidyr)
library(stringr)

tt = read.csv("../../../../Downloads/200921_rivers_list.csv")
aa = as.data.frame(str_split_fixed(tt$Name.Query., ";", 2)) # split into two columns
aa$V2 = gsub('.{1}$', '', aa$V2) # remove trailing ';

aa$V1 = gsub('[[:digit:]]+', '', aa$V1) # remove digits
aa$V1 = aa$V1 %>% str_remove_all("[[:punct:]]") %>% str_trim("both") # remove punct and trim

aa = aa %>%
  rename(name = V1) %>%
  rename(mine_query = V2)

aa = right_join(aa, thames)

write.csv()
