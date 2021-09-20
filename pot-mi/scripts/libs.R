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
aa$V1 = gsub('.{1}$', '', aa$V1) # remove trailing "

aa$v1 <- gsub("^.{0,3}", "", aa$V1)

sub("\\S+\\s+", "", aa$usertweet)
aa$v1 <-substr(aa$v1,1,nchar(aa$V1)-5)

aa %>% str
