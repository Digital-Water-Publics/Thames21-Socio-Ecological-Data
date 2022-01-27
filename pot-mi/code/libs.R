####
####
#### AIM: install and load librarys
####
####

pkgs = c("academictwitteR",
         "tidyverse",
         "lubridate",
         "sf",
         "cde",
         "tm",
         "tidyr",
         "stringr",
         "quanteda",
         "sentimentr",
         "spacyr",
         "ggpubr",
         "tidytext",
         "tidygeocoder")

# Install pkgs not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))
