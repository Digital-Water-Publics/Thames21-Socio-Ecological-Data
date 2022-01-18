####
####
#### AIM: install and load librarys
####
####

pkgs = c("academictwitteR",
         "dplyr",
         "sf",
         "cde",
         "tm"
         "stringr",
         "sentimentr",
         "spacyr",
         "tidytext",
         "tidygeocoder")

# Install pkgs not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))
