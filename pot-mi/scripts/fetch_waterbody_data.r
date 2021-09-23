##### DOWNLOAD WATERBODY DATA HOSTED ON VIA CDE package

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


#read data from catchment explorer
geo = read_sf("../../../../Downloads/reasons_for_not_achieving_good/reasons_for_not_achieving_good_RBD_6.csv")
colnames(geo)[which(names(geo) == "water body")] = "label"
colnames(geo)[which(names(geo) == "Water body id")] = "wbid"
geo = geo %>% select(wbid,label)
geo = unique(geo)

# join waterbodies and geo
waterbodies = right_join(geo,aa)

# count unique one word queries
tt = as.data.frame(unique(subset(waterbodies$mine_query, str_count(waterbodies$mine_query,"\\S+") == 1)))
colnames(tt) = "mine_query"

# anti join one word queries with waterbody list
water_bodies = anti_join(waterbodies,tt)
