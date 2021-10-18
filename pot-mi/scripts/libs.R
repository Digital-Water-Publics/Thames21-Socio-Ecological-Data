pkgs = c("academictwitteR",
         "dplyr",
         "sf",
         "cde",
         "stringr",
         "sentimentr",
         "spacyr",
         "tidytext")

# Install pkgs not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))
